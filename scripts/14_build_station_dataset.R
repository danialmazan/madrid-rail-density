#!/usr/bin/env Rscript
source("R/utils.R")
source("R/station_helpers.R")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(data.table))

cfg <- read_config()
ensure_dir(cfg$paths$phase3_interim)
ensure_dir(cfg$paths$phase3_derived)

source_layers <- lapply(cfg$phase3$station_services, function(service) {
  path <- file.path(cfg$paths$rail_phase3_raw, paste0(service$code, "_estaciones.geojson"))
  if (!file.exists(path)) stop("Missing Phase 3 source: ", path, ". Run make download-phase3-stations.")
  x <- st_read(path, quiet = TRUE, stringsAsFactors = FALSE)
  if (st_crs(x)$epsg != cfg$project$crs) x <- st_transform(x, cfg$project$crs)
  xy <- st_coordinates(x)
  dt <- as.data.table(st_drop_geometry(x))
  dt[, `:=`(source_code = service$code, source_mode = service$source_mode, source_x = xy[, 1], source_y = xy[, 2])]
  dt
})
raw <- rbindlist(source_layers, fill = TRUE)
setnames(raw, names(raw), tolower(names(raw)))

for (field in c("idestacion", "denominacion", "codigointercambiador", "codigoprovincia", "codigomunicipio", "fechafin", "fechaactual", "lineas")) {
  raw[, (field) := station_trim(get(field))]
}
raw[, active := is.na(fechafin) | fechafin >= gsub("-", "", cfg$phase3$station_snapshot)]
raw[, exclusion_reason := fcase(
  source_code == "M5" & codigoprovincia != "28", "cercanias_outside_comunidad_de_madrid",
  is.na(denominacion), "missing_station_name",
  !active, "inactive_at_snapshot",
  default = NA_character_
)]

excluded <- raw[!is.na(exclusion_reason), .(
  source_code, idestacion, denominacion, codigointercambiador, codigoprovincia,
  codigomunicipio, fechafin, fechaactual, exclusion_reason, source_x, source_y
)]
included <- raw[is.na(exclusion_reason)]
included[, station_id := station_group_key(codigointercambiador, idestacion, cfg)]

# Source-record-to-service bridge. A CRTM record can list more than one line.
service_rows <- vector("list", nrow(included))
for (i in seq_len(nrow(included))) {
  services <- canonical_service_lines(included$source_code[i], included$lineas[i])
  if (!nrow(services)) stop("No service line found for included CRTM record ", included$idestacion[i])
  services[, `:=`(
    station_id = included$station_id[i], source_code = included$source_code[i],
    source_station_id = included$idestacion[i]
  )]
  service_rows[[i]] <- services
}
services <- unique(rbindlist(service_rows), by = c("station_id", "mode", "line_id"))
mode_order <- c("metro", "metro_ligero", "cercanias", "tranvia_parla")
line_order <- c(paste0("M", c(1:12)), "MR", paste0("ML", 1:3), paste0("C", c(1:5, 7, 8, 9, 10)), "C8A", "C8B", "TP")

municipalities <- unique(as.data.table(st_drop_geometry(readRDS(file.path(cfg$paths$phase2_interim, "ine_sections_2025.rds"))))[, .(CUMUN, NMUN)])
municipalities[, crtm_municipality_code := substr(CUMUN, 3, 5)]
municipality_lookup <- setNames(municipalities$NMUN, municipalities$crtm_municipality_code)

overrides <- station_name_overrides()
stations_dt <- included[, {
  service_group <- services[station_id == .BY$station_id]
  raw_names <- unique(denominacion)
  base_name <- station_title_case(sort(raw_names)[1])
  canonical_name <- if (.BY$station_id %in% names(overrides)) unname(overrides[.BY$station_id]) else base_name
  municipality_codes <- sort(unique(codigomunicipio))
  municipality_names <- unname(municipality_lookup[municipality_codes])
  group_mean_x <- mean(source_x)
  group_mean_y <- mean(source_y)
  reference_index <- order((source_x - group_mean_x)^2 + (source_y - group_mean_y)^2, idestacion)[1]
  list(
    station_name = canonical_name,
    source_names = ordered_collapse(raw_names),
    modes = ordered_collapse(service_group$mode, mode_order),
    lines = ordered_collapse(service_group$line_id, line_order),
    mode_count = uniqueN(service_group$mode),
    line_count = uniqueN(service_group$line_id),
    is_interchange = uniqueN(service_group$mode) > 1L || uniqueN(service_group$line_id) > 1L,
    crtm_interchange_code = ordered_collapse(codigointercambiador),
    municipality_code = ordered_collapse(paste0("28", municipality_codes)),
    municipality_name = ordered_collapse(municipality_names),
    province_code = ordered_collapse(codigoprovincia),
    source_record_count = .N,
    reference_x = source_x[reference_index],
    reference_y = source_y[reference_index],
    reference_source_code = source_code[reference_index],
    reference_source_station_id = idestacion[reference_index],
    reference_source_station_name = denominacion[reference_index],
    source_point_max_separation_m = maximum_point_separation(source_x, source_y),
    source_data_date = ordered_collapse(fechaactual),
    snapshot_date = cfg$phase3$station_snapshot,
    scope = cfg$phase3$scope,
    reference_method = cfg$phase3$reference_point_method
  )
}, by = station_id]

stations <- st_as_sf(stations_dt, coords = c("reference_x", "reference_y"), crs = cfg$project$crs, remove = FALSE)
lonlat <- st_coordinates(st_transform(stations, 4326))
stations$longitude <- lonlat[, 1]
stations$latitude <- lonlat[, 2]
stations <- stations[order(stations$station_name), ]

mode_points_dt <- included[, .(
  source_record_count = .N, reference_x = mean(source_x), reference_y = mean(source_y),
  source_point_max_separation_m = maximum_point_separation(source_x, source_y)
), by = .(station_id, source_mode)]
mode_points <- st_as_sf(mode_points_dt, coords = c("reference_x", "reference_y"), crs = cfg$project$crs, remove = FALSE)

source_map <- included[, .(
  station_id, source_code, source_mode, source_station_id = idestacion,
  source_station_code = codigoestacion, source_station_name = denominacion,
  source_lines = lineas, crtm_interchange_code = codigointercambiador,
  province_code = codigoprovincia, municipality_code = codigomunicipio,
  source_situation = situacion, source_type = tipo,
  source_data_date = fechaactual, source_x, source_y
)]

# Entrance geometry is kept at full detail. IDFESTACION provides a direct link
# from each CRTM access point to an included source station and hence to the
# canonical physical-station identifier.
access_layers <- lapply(cfg$phase3$station_services, function(service) {
  path <- file.path(cfg$paths$rail_phase3_raw, paste0(service$code, "_accesos.geojson"))
  if (!file.exists(path)) stop("Missing Phase 3 entrance source: ", path, ". Run make download-phase3-stations.")
  x <- st_read(path, quiet = TRUE, stringsAsFactors = FALSE)
  if (st_crs(x)$epsg != cfg$project$crs) x <- st_transform(x, cfg$project$crs)
  xy <- st_coordinates(x)
  dt <- as.data.table(st_drop_geometry(x))
  dt[, `:=`(source_code = service$code, access_x = xy[, 1], access_y = xy[, 2])]
  dt
})
access_raw <- rbindlist(access_layers, fill = TRUE)
setnames(access_raw, names(access_raw), tolower(names(access_raw)))
for (field in c("idacceso", "idfestacion", "codigoestacion", "denominacion", "fechafin", "fechaactual", "nombrevia", "numeroportal", "accesominusvalidos", "horaapertura", "horacierre")) {
  access_raw[, (field) := station_trim(get(field))]
}
access_raw[, active := is.na(fechafin) | fechafin >= gsub("-", "", cfg$phase3$station_snapshot)]
access_joined <- merge(
  access_raw[active == TRUE],
  source_map[, .(source_code, source_station_id, station_id)],
  by.x = c("source_code", "idfestacion"), by.y = c("source_code", "source_station_id"),
  all.x = TRUE
)
unmatched_access <- access_joined[is.na(station_id), .(
  source_code, source_access_id = idacceso, source_station_id = idfestacion,
  source_station_code = codigoestacion, access_name = denominacion,
  province_code = codigoprovincia, municipality_code = codigomunicipio,
  source_data_date = fechaactual, access_x, access_y,
  exclusion_reason = "source_station_outside_phase3_scope"
)]
entrance_dt <- access_joined[!is.na(station_id), .(
  entrance_id = paste0(tolower(source_code), ":", tolower(idacceso)),
  station_id, source_code, source_access_id = idacceso,
  source_station_id = idfestacion, source_station_code = codigoestacion,
  entrance_name = denominacion, street_type = tipovia, street_name = nombrevia,
  street_number = numeroportal, street_number_qualifier = calificadorportal,
  postal_code = codigopostal, level = nivel,
  vestibule_code = codigovestibuloctm, vestibule_number = numerovestibulo,
  accessibility_source_value = accesominusvalidos,
  opening_time = horaapertura, closing_time = horacierre,
  province_code = codigoprovincia, municipality_code = codigomunicipio,
  source_data_date = fechaactual, snapshot_date = cfg$phase3$station_snapshot,
  access_x, access_y
)]
entrances <- st_as_sf(entrance_dt, coords = c("access_x", "access_y"), crs = cfg$project$crs, remove = FALSE)
entrance_lonlat <- st_coordinates(st_transform(entrances, 4326))
entrances$longitude <- entrance_lonlat[, 1]
entrances$latitude <- entrance_lonlat[, 2]
station_index <- match(entrances$station_id, stations$station_id)
entrances$distance_to_station_reference_m <- as.numeric(st_distance(entrances, stations[station_index, ], by_element = TRUE))

entrance_counts <- entrance_dt[, .(official_entrance_count = .N), by = station_id]
stations$official_entrance_count <- entrance_counts$official_entrance_count[match(stations$station_id, entrance_counts$station_id)]
stations$official_entrance_count[is.na(stations$official_entrance_count)] <- 0L
stations$has_official_entrance <- stations$official_entrance_count > 0L
stations$preferred_origin_type <- ifelse(stations$has_official_entrance, "official_entrance", "official_station_point_fallback")

entrance_origins <- entrances[, c("entrance_id", "station_id", "source_code")]
names(entrance_origins)[names(entrance_origins) == "entrance_id"] <- "origin_id"
entrance_origins$origin_type <- "official_entrance"
entrance_origins$is_fallback <- FALSE
missing_entrance <- stations[!stations$has_official_entrance, c("station_id", "reference_source_code")]
fallback_origins <- missing_entrance
fallback_origins$origin_id <- paste0(fallback_origins$station_id, ":station_point")
fallback_origins$source_code <- fallback_origins$reference_source_code
fallback_origins$origin_type <- "official_station_point_fallback"
fallback_origins$is_fallback <- TRUE
fallback_origins <- fallback_origins[, names(entrance_origins)]
preferred_origins <- rbind(entrance_origins, fallback_origins)

station_reference_points <- stations[, c(
  "station_id", "station_name", "reference_source_code", "reference_source_station_id",
  "reference_method", "reference_x", "reference_y"
)]
station_reference_points$distance_role <- "fallback_and_sensitivity"

# Phase 7 platform geometry. IDFESTACION is the same direct source-station
# bridge used by the access layers. Platforms retain their source mode so that
# mode-specific resident analyses do not borrow a platform from another mode
# at a multimodal interchange.
platform_layers <- lapply(cfg$phase3$station_services, function(service) {
  path <- file.path(cfg$paths$rail_phase3_raw, paste0(service$code, "_andenes.geojson"))
  if (!file.exists(path)) stop("Missing Phase 7 platform source: ", path, ". Run make download-phase3-stations.")
  x <- st_read(path, quiet = TRUE, stringsAsFactors = FALSE)
  if (st_crs(x)$epsg != cfg$project$crs) x <- st_transform(x, cfg$project$crs)
  xy <- st_coordinates(x)
  dt <- as.data.table(st_drop_geometry(x))
  dt[, `:=`(source_code = service$code, platform_x = xy[, 1], platform_y = xy[, 2])]
  dt
})
platform_raw <- rbindlist(platform_layers, fill = TRUE)
setnames(platform_raw, names(platform_raw), tolower(names(platform_raw)))
for (field in c("idanden", "idfestacion", "codigoestacion", "denominacion", "denominacionestacion", "fechafin", "fechaactual")) {
  platform_raw[, (field) := station_trim(get(field))]
}
platform_raw[, active := is.na(fechafin) | fechafin >= gsub("-", "", cfg$phase3$station_snapshot)]
platform_joined <- merge(
  platform_raw[active == TRUE],
  source_map[, .(source_code, source_station_id, station_id, source_lines)],
  by.x = c("source_code", "idfestacion"), by.y = c("source_code", "source_station_id"),
  all.x = TRUE
)
if (platform_joined[is.na(station_id) & source_code != "M5", .N]) {
  stop("Non-Cercanías platform records failed to match an in-scope station.")
}
platform_joined <- platform_joined[!is.na(station_id)]
platform_joined[, source_mode := vapply(seq_len(.N), function(i) {
  modes <- unique(canonical_service_lines(source_code[i], source_lines[i])$mode)
  if (length(modes) != 1L) stop("Platform source station does not resolve to one source mode: ", idfestacion[i])
  modes
}, character(1))]
platform_dt <- platform_joined[, .(
  platform_id = paste0(tolower(source_code), ":", tolower(idanden)),
  station_id, source_code, source_mode,
  source_platform_id = idanden, source_station_id = idfestacion,
  source_station_code = codigoestacion, platform_name = denominacion,
  station_name_source = denominacionestacion, platform_number = numanden,
  platform_type = tipo, level = nivel,
  accessibility_source_value = accesominusvalidos,
  source_data_date = fechaactual, snapshot_date = cfg$phase3$station_snapshot,
  platform_x, platform_y
)]
platforms <- st_as_sf(platform_dt, coords = c("platform_x", "platform_y"), crs = cfg$project$crs, remove = FALSE)
platform_lonlat <- st_coordinates(st_transform(platforms, 4326))
platforms$longitude <- platform_lonlat[, 1]
platforms$latitude <- platform_lonlat[, 2]

# Normalized Phase 7 origin contract. Entrance origins are station-shared;
# platform origins remain source-mode specific. Fallback station points are
# retained only for the entrance method.
entrance_v2 <- preferred_origins
entrance_v2$point_method <- "entrance"
entrance_v2$source_mode <- "shared"
entrance_v2$source_record_id <- entrance_v2$origin_id
entrance_v2$snapshot_date <- cfg$phase3$station_snapshot
entrance_v2 <- entrance_v2[, c("origin_id", "station_id", "point_method", "source_mode", "source_code", "source_record_id", "origin_type", "is_fallback", "snapshot_date")]

platform_v2 <- platforms[, c("platform_id", "station_id", "source_mode", "source_code", "source_platform_id", "snapshot_date")]
names(platform_v2)[names(platform_v2) == "platform_id"] <- "origin_id"
names(platform_v2)[names(platform_v2) == "source_platform_id"] <- "source_record_id"
platform_v2$point_method <- "platform"
platform_v2$origin_type <- "official_platform"
platform_v2$is_fallback <- FALSE
platform_v2 <- platform_v2[, names(entrance_v2)]
station_origins_v2 <- rbind(entrance_v2, platform_v2)

fwrite(as.data.table(st_drop_geometry(stations)), file.path(cfg$paths$phase3_derived, "canonical_stations.csv"))
saveRDS(stations, file.path(cfg$paths$phase3_derived, "canonical_stations.rds"), compress = "xz")
fwrite(as.data.table(st_drop_geometry(entrances)), file.path(cfg$paths$phase3_derived, "station_entrances.csv"))
saveRDS(entrances, file.path(cfg$paths$phase3_derived, "station_entrances.rds"), compress = "xz")
fwrite(as.data.table(st_drop_geometry(preferred_origins)), file.path(cfg$paths$phase3_derived, "station_distance_origins.csv"))
saveRDS(preferred_origins, file.path(cfg$paths$phase3_derived, "station_distance_origins.rds"), compress = "xz")
fwrite(as.data.table(st_drop_geometry(station_reference_points)), file.path(cfg$paths$phase3_derived, "station_reference_points.csv"))
saveRDS(station_reference_points, file.path(cfg$paths$phase3_derived, "station_reference_points.rds"), compress = "xz")
fwrite(as.data.table(st_drop_geometry(platforms)), file.path(cfg$paths$phase3_derived, "station_platforms.csv"))
saveRDS(platforms, file.path(cfg$paths$phase3_derived, "station_platforms.rds"), compress = "xz")
fwrite(as.data.table(st_drop_geometry(station_origins_v2)), file.path(cfg$paths$phase3_derived, "station_origins_phase7.csv"))
saveRDS(station_origins_v2, file.path(cfg$paths$phase3_derived, "station_origins_phase7.rds"), compress = "xz")
write_table(services[order(station_id, match(mode, mode_order), match(line_id, line_order))], file.path(cfg$paths$phase3_derived, "station_services"))
write_table(source_map, file.path(cfg$paths$phase3_interim, "station_source_records"))
write_table(excluded, file.path(cfg$paths$phase3_interim, "excluded_source_records"))
write_table(unmatched_access, file.path(cfg$paths$phase3_interim, "excluded_entrance_records"))
fwrite(as.data.table(st_drop_geometry(mode_points)), file.path(cfg$paths$phase3_interim, "station_mode_reference_points.csv"))
saveRDS(mode_points, file.path(cfg$paths$phase3_interim, "station_mode_reference_points.rds"), compress = "xz")

cat("Built", nrow(stations), "canonical physical stations,", nrow(entrances), "official entrances and", nrow(platforms), "official platforms;", sum(!stations$has_official_entrance), "stations use the official station-point fallback.\n")
