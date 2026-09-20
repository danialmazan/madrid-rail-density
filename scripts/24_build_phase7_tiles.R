#!/usr/bin/env Rscript
source("R/utils.R")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(data.table))

cfg <- read_config()
out_dir <- cfg$paths$phase7_derived
tile_dir <- file.path(cfg$paths$phase7_frontend, "public", "tiles")
source_dir <- file.path(out_dir, "tile_sources")
lapply(c(tile_dir, source_dir), ensure_dir)
attrs <- fread(file.path(out_dir, "component_map_attributes_2025.csv.gz"))

distance_band <- function(x) {
  cut(x, breaks = c(-Inf, 200, 400, 600, 800, 1000, 1500, Inf), labels = FALSE, right = TRUE) - 1L
}
attrs[, `:=`(
  entrance_band = distance_band(entrance_distance_m),
  platform_band = distance_band(platform_distance_m),
  entrance_distance_m = round(entrance_distance_m),
  platform_distance_m = round(platform_distance_m)
)]

component_files <- list.files(file.path(cfg$paths$phase2_interim, "components_by_municipality"), pattern = "^components_[0-9]+\\.rds$", full.names = TRUE)
component_seq <- file.path(source_dir, "components.geojsons")
if (file.exists(component_seq)) unlink(component_seq)
first <- TRUE
for (path in component_files) {
  x <- readRDS(path)
  x <- merge(x[, "component_key"], attrs[, .(
    component_key,
    e_station = entrance_station_id, e_dist = entrance_distance_m, e_band = entrance_band,
    p_station = platform_station_id, p_dist = platform_distance_m, p_band = platform_band
  )], by = "component_key", all.x = FALSE)
  x <- st_transform(x, 4326)
  st_write(x, component_seq, driver = "GeoJSONSeq", append = !first, quiet = TRUE)
  first <- FALSE
  message("Tile source: ", basename(path), " (", nrow(x), " components)")
}

tippecanoe <- Sys.which("tippecanoe")
if (!nzchar(tippecanoe)) stop("tippecanoe is required to build Phase 7 PMTiles.")
run_tippecanoe <- function(args) {
  status <- system2(tippecanoe, args)
  if (status != 0) stop("tippecanoe failed with status ", status)
}
run_tippecanoe(c(
  "--force", "--output", file.path(tile_dir, "components.pmtiles"),
  "--layer", "components", "--minimum-zoom", "12", "--maximum-zoom", "16",
  "--drop-densest-as-needed", "--extend-zooms-if-still-dropping",
  "--include", "component_key", "--include", "e_station", "--include", "e_dist", "--include", "e_band",
  "--include", "p_station", "--include", "p_dist", "--include", "p_band",
  component_seq
))

# Generalized 2 km distance surface for regional zooms.
population <- readRDS(file.path(cfg$paths$phase2_derived, "component_population_2025.rds"))
pdt <- as.data.table(st_drop_geometry(population))[, .(component_key, x, y, estimated_population)]
pdt <- merge(pdt, attrs[, .(component_key, entrance_distance_m, platform_distance_m)], by = "component_key")
pdt[, `:=`(gx = floor(x / 2000) * 2000, gy = floor(y / 2000) * 2000)]
surface <- pdt[, .(
  population = sum(estimated_population),
  entrance_distance_m = weighted.mean(entrance_distance_m, estimated_population),
  platform_distance_m = weighted.mean(platform_distance_m, estimated_population)
), by = .(gx, gy)]
surface[, `:=`(
  entrance_band = distance_band(entrance_distance_m),
  platform_band = distance_band(platform_distance_m)
)]
surface_geometry <- st_sfc(lapply(seq_len(nrow(surface)), function(i) st_polygon(list(matrix(c(
  surface$gx[i], surface$gy[i], surface$gx[i] + 2000, surface$gy[i],
  surface$gx[i] + 2000, surface$gy[i] + 2000, surface$gx[i], surface$gy[i] + 2000,
  surface$gx[i], surface$gy[i]
), ncol = 2, byrow = TRUE)))), crs = cfg$project$crs)
surface_sf <- st_transform(st_sf(surface[, .(
  pop = round(population), e_dist = round(entrance_distance_m), p_dist = round(platform_distance_m),
  e_band = entrance_band, p_band = platform_band
)], geometry = surface_geometry), 4326)
surface_path <- file.path(source_dir, "surface.geojsons")
st_write(surface_sf, surface_path, driver = "GeoJSONSeq", delete_dsn = TRUE, quiet = TRUE)
run_tippecanoe(c(
  "--force", "--output", file.path(tile_dir, "surface.pmtiles"),
  "--layer", "surface", "--minimum-zoom", "5", "--maximum-zoom", "12",
  "--no-feature-limit", "--no-tile-size-limit", surface_path
))

# Stations, origins and station-origin connections share one small archive.
stations <- readRDS(file.path(cfg$paths$phase3_derived, "canonical_stations.rds"))
origins <- readRDS(file.path(cfg$paths$phase3_derived, "station_origins_phase7.rds"))
station_props <- as.data.table(st_drop_geometry(stations))[, .(station_id, station_name, modes, lines)]
stations_tile <- merge(stations[, "station_id"], station_props, by = "station_id")
origins_tile <- merge(origins, station_props[, .(station_id, station_name, modes)], by = "station_id")
station_index <- match(origins_tile$station_id, stations$station_id)
connections <- st_sf(
  station_id = origins_tile$station_id,
  point_method = origins_tile$point_method,
  geometry = st_sfc(lapply(seq_len(nrow(origins_tile)), function(i) st_linestring(rbind(
    st_coordinates(stations[station_index[i], ])[1, ], st_coordinates(origins_tile[i, ])[1, ]
  ))), crs = st_crs(stations))
)
write_seq <- function(x, name) {
  path <- file.path(source_dir, paste0(name, ".geojsons"))
  st_write(st_transform(x, 4326), path, driver = "GeoJSONSeq", delete_dsn = TRUE, quiet = TRUE)
  path
}
station_path <- write_seq(stations_tile, "stations")
origin_path <- write_seq(origins_tile, "origins")
connection_path <- write_seq(connections, "connections")
run_tippecanoe(c(
  "--force", "--output", file.path(tile_dir, "stations.pmtiles"),
  "-L", paste0("stations:", station_path),
  "-L", paste0("origins:", origin_path),
  "-L", paste0("connections:", connection_path),
  "--minimum-zoom", "5", "--maximum-zoom", "16", "--drop-rate", "0",
  "--no-feature-limit", "--no-tile-size-limit"
))
st_write(st_transform(origins_tile, 4326), file.path(cfg$paths$phase7_frontend, "public", "data", "station-origins.geojson"), delete_dsn = TRUE, quiet = TRUE)

cat("Built Phase 7 PMTiles:", paste(basename(list.files(tile_dir, pattern = "\\.pmtiles$")), collapse = ", "), "\n")
