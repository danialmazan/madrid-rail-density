#!/usr/bin/env Rscript
source("R/utils.R")
source("R/catchment_helpers.R")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(terra))

cfg <- read_config()
out_dir <- cfg$paths$phase7_derived
ensure_dir(out_dir)
curve_step <- as.numeric(cfg$phase7$curve_step_m)
hist_step <- as.numeric(cfg$phase7$histogram_step_m)
maximum_distance <- as.numeric(cfg$phase7$maximum_distance_m)
radii <- seq(0, maximum_distance, curve_step)

population <- readRDS(file.path(cfg$paths$phase2_derived, "component_population_2025.rds"))
stations <- readRDS(file.path(cfg$paths$phase3_derived, "canonical_stations.rds"))
services <- fread(file.path(cfg$paths$phase3_derived, "station_services.csv"))
origins <- readRDS(file.path(cfg$paths$phase3_derived, "station_origins_phase7.rds"))
points <- population[, c("component_key", "CUSEC", "CUMUN", "estimated_population")]
point_xy <- st_coordinates(points)
points_terra <- terra::vect(points)
point_dt <- as.data.table(st_drop_geometry(points))
point_dt[, `:=`(
  component_index = .I,
  geography = fifelse(CUMUN == cfg$phase7$madrid_municipality_code, "city", "region"),
  district_code = fifelse(CUMUN == cfg$phase7$madrid_municipality_code, substr(CUSEC, 6, 7), NA_character_)
)]

if (nrow(point_dt) != 644613L || uniqueN(point_dt$component_key) != nrow(point_dt)) stop("Population component contract changed.")
if (!all(c("entrance", "platform") %in% unique(origins$point_method))) stop("Both Phase 7 point methods are required.")

mode_station_ids <- list(
  metro = unique(services[mode == "metro", station_id]),
  cercanias = unique(services[mode == "cercanias", station_id]),
  mlt = unique(services[mode %in% c("metro_ligero", "tranvia_parla"), station_id])
)
mode_station_ids$metro_cercanias <- union(mode_station_ids$metro, mode_station_ids$cercanias)
mode_station_ids$all_rail <- stations$station_id

select_origins <- function(method, network) {
  x <- origins[origins$point_method == method, ]
  if (method == "platform") {
    keep_modes <- switch(network,
      metro = "metro",
      cercanias = "cercanias",
      mlt = c("metro_ligero", "tranvia_parla"),
      metro_cercanias = c("metro", "cercanias"),
      all_rail = c("metro", "metro_ligero", "cercanias", "tranvia_parla")
    )
    x <- x[x$source_mode %in% keep_modes, ]
  } else {
    x <- x[x$station_id %in% mode_station_ids[[network]], ]
  }
  x[order(x$station_id, x$origin_id), ]
}

nearest_deterministic <- function(origins_i) {
  # Ask an exact spatial index for several nearest candidates, then apply the
  # documented lexicographic key ourselves: distance first, station_id second.
  # If all k candidates tie, recompute that exceptional row against every
  # origin so even coincident multi-station records remain deterministic.
  origins_i <- origins_i[order(origins_i$station_id, origins_i$origin_id), ]
  origin_xy <- st_coordinates(origins_i)
  k <- min(8L, nrow(origins_i))
  candidate_index <- terra::nearby(points_terra, terra::vect(origins_i), k = k)
  candidate_index <- as.matrix(candidate_index[, -1, drop = FALSE])
  station_rank <- match(origins_i$station_id, sort(unique(origins_i$station_id)))
  best_index <- candidate_index[, 1]
  best_distance <- sqrt((point_xy[, 1] - origin_xy[best_index, 1])^2 + (point_xy[, 2] - origin_xy[best_index, 2])^2)
  tolerance <- 1e-9
  last_distance <- best_distance
  if (k > 1L) for (candidate_column in 2:k) {
    candidate <- candidate_index[, candidate_column]
    candidate_distance <- sqrt((point_xy[, 1] - origin_xy[candidate, 1])^2 + (point_xy[, 2] - origin_xy[candidate, 2])^2)
    better <- candidate_distance < best_distance - tolerance |
      (abs(candidate_distance - best_distance) <= tolerance & station_rank[candidate] < station_rank[best_index])
    best_index[better] <- candidate[better]
    best_distance[better] <- candidate_distance[better]
    if (candidate_column == k) last_distance <- candidate_distance
  }
  saturated <- which(abs(last_distance - best_distance) <= tolerance & k < nrow(origins_i))
  for (row in saturated) {
    distances <- sqrt((point_xy[row, 1] - origin_xy[, 1])^2 + (point_xy[row, 2] - origin_xy[, 2])^2)
    tied <- which(abs(distances - min(distances)) <= tolerance)
    chosen <- tied[which.min(station_rank[tied])]
    best_index[row] <- chosen
    best_distance[row] <- distances[chosen]
  }
  data.table(
    station_id = origins_i$station_id[best_index],
    distance_m = best_distance
  )
}

networks <- c("all_rail", "metro", "metro_cercanias", "cercanias", "mlt")
methods <- c("entrance", "platform")
nearest <- copy(point_dt)
for (method in methods) {
  for (network in networks) {
    message("Nearest distances: ", method, " / ", network)
    result <- nearest_deterministic(select_origins(method, network))
    nearest[, (paste(method, network, "station_id", sep = "_")) := result$station_id]
    nearest[, (paste(method, network, "distance_m", sep = "_")) := result$distance_m]
  }
}
saveRDS(nearest, file.path(out_dir, "component_nearest_station_phase7_2025.rds"), compress = "gzip")
if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(nearest, file.path(out_dir, "component_nearest_station_phase7_2025.parquet"), compression = "zstd")
}

weighted_median <- function(values, weights) {
  o <- order(values)
  values <- values[o]
  weights <- weights[o]
  values[which(cumsum(weights) >= sum(weights) / 2)[1]]
}

curve_rows <- list()
hist_rows <- list()
metric_rows <- list()
k_curve <- 0L
k_hist <- 0L
k_metric <- 0L
geographies <- c(list(region = seq_len(nrow(nearest)), city = which(nearest$CUMUN == cfg$phase7$madrid_municipality_code)),
  setNames(lapply(names(cfg$phase7$district_names), function(code) which(nearest$district_code == code)), paste0("district_", names(cfg$phase7$district_names))))

for (geography_id in names(geographies)) {
  rows <- geographies[[geography_id]]
  weights <- nearest$estimated_population[rows]
  denominator <- sum(weights)
  geography_type <- if (startsWith(geography_id, "district_")) "district" else geography_id
  geography_name <- if (geography_type == "district") unname(cfg$phase7$district_names[[sub("district_", "", geography_id)]]) else if (geography_type == "city") "Madrid city" else "Comunidad de Madrid"
  for (method in methods) {
    for (network in networks) {
      distances <- nearest[[paste(method, network, "distance_m", sep = "_")]][rows]
      o <- order(distances)
      sorted_distance <- distances[o]
      cumulative_weight <- cumsum(weights[o])
      positions <- findInterval(radii, sorted_distance)
      covered <- ifelse(positions > 0, cumulative_weight[pmax(positions, 1)], 0)
      k_curve <- k_curve + 1L
      curve_rows[[k_curve]] <- data.table(
        geography_type, geography_id, geography_name, point_method = method,
        rail_network = network, radius_m = radii,
        population = covered, population_share = covered / denominator,
        geography_population = denominator
      )
      median_distance <- weighted_median(distances, weights)
      mean_distance <- weighted.mean(distances, weights)
      k_metric <- k_metric + 1L
      metric_rows[[k_metric]] <- data.table(
        geography_type, geography_id, geography_name, point_method = method,
        rail_network = network, population = denominator,
        median_distance_m = median_distance, mean_distance_m = mean_distance
      )
      if (network %in% c("all_rail", "metro", "cercanias", "mlt")) {
        breaks <- seq(0, maximum_distance, hist_step)
        band <- findInterval(distances, breaks, rightmost.closed = TRUE)
        band[distances > maximum_distance] <- length(breaks) + 1L
        h <- data.table(band = band, population_weight = weights)[, .(population = sum(population_weight)), by = band]
        grid <- data.table(band = seq_len(length(breaks) + 1L))
        h <- merge(grid, h, by = "band", all.x = TRUE)
        h[is.na(population), population := 0]
        h[, `:=`(
          band_start_m = fifelse(band <= length(breaks), (band - 1) * hist_step, maximum_distance),
          band_end_m = fifelse(band <= length(breaks), pmin(band * hist_step, maximum_distance), NA_real_),
          overflow = band == length(breaks) + 1L,
          population_share = population / denominator
        )]
        k_hist <- k_hist + 1L
        hist_rows[[k_hist]] <- h[, .(
          geography_type, geography_id, geography_name, point_method = method,
          rail_network = network, band_start_m, band_end_m, overflow,
          population, population_share, geography_population = denominator
        )]
      }
    }
  }
}

coverage_curves <- rbindlist(curve_rows)
distance_histograms <- rbindlist(hist_rows)
area_metrics <- rbindlist(metric_rows)
setorder(coverage_curves, geography_type, geography_id, point_method, rail_network, radius_m)
setorder(distance_histograms, geography_type, geography_id, point_method, rail_network, band_start_m)
setorder(area_metrics, geography_type, geography_id, point_method, rail_network)
fwrite(coverage_curves, file.path(out_dir, "coverage_curves_2025.csv"))
fwrite(distance_histograms, file.path(out_dir, "distance_histograms_2025.csv"))
fwrite(area_metrics, file.path(out_dir, "area_distance_metrics_2025.csv"))

# Station catchments at 25 m resolution. Reuse the audited entrance distances;
# platform distances are new and are persisted for reproducibility.
entrance_distance_path <- file.path(cfg$paths$phase4_derived, "component_station_distances_preferred_1500m.rds")
entrance_distances <- readRDS(entrance_distance_path)
platform_distance_path <- file.path(out_dir, "component_station_distances_platform_1500m.rds")
if (file.exists(platform_distance_path)) {
  platform_distances <- readRDS(platform_distance_path)
} else {
  message("Calculating component-to-platform station catchments...")
  platform_distances <- component_station_distances(points, origins[origins$point_method == "platform", ], maximum_distance, "platform")
  platform_distances[, component_key := point_dt$component_key[component_index]]
  saveRDS(platform_distances, platform_distance_path, compress = "gzip")
}

aggregate_25m <- function(distances, method) {
  x <- copy(distances)
  x[, estimated_population := point_dt$estimated_population[component_index]]
  x[, radius_m := pmin(maximum_distance, ceiling(distance_m / curve_step) * curve_step)]
  incremental <- x[distance_m <= maximum_distance, .(
    incremental_population = sum(estimated_population),
    incremental_components = .N
  ), by = .(station_id, radius_m)]
  grid <- CJ(station_id = stations$station_id, radius_m = radii, unique = TRUE)
  grid <- merge(grid, incremental, by = c("station_id", "radius_m"), all.x = TRUE)
  grid[is.na(incremental_population), `:=`(incremental_population = 0, incremental_components = 0L)]
  setorder(grid, station_id, radius_m)
  grid[, `:=`(
    population = cumsum(incremental_population),
    residential_components = cumsum(incremental_components),
    point_method = method
  ), by = station_id]
  grid
}
station_curves <- rbindlist(list(aggregate_25m(entrance_distances, "entrance"), aggregate_25m(platform_distances, "platform")))

origin_areas <- function(method) {
  x <- origins[origins$point_method == method, ]
  groups <- split(seq_len(nrow(x)), x$station_id)
  multipoints <- st_sfc(lapply(groups, function(i) st_combine(st_geometry(x[i, ]))[[1]]), crs = st_crs(x))
  grouped <- st_sf(station_id = names(groups), geometry = multipoints)
  rbindlist(lapply(radii, function(radius) data.table(
    station_id = grouped$station_id, radius_m = radius,
    catchment_area_km2 = if (radius == 0) 0 else as.numeric(st_area(st_buffer(grouped, radius))) / 1e6,
    point_method = method
  )))
}
areas <- rbindlist(list(origin_areas("entrance"), origin_areas("platform")))
station_curves <- merge(station_curves, areas, by = c("station_id", "radius_m", "point_method"), all.x = TRUE)
station_meta <- as.data.table(st_drop_geometry(stations))[, .(
  station_id, station_name, modes, lines, municipality_code, municipality_name,
  official_entrance_count, entrance_fallback = !has_official_entrance,
  longitude, latitude
)]
station_curves <- merge(station_curves, station_meta, by = "station_id", all.x = TRUE)
station_curves[, `:=`(
  population_density_per_km2 = fifelse(catchment_area_km2 > 0, population / catchment_area_km2, NA_real_),
  station_geography = fifelse(grepl(cfg$phase7$madrid_municipality_code, municipality_code, fixed = TRUE), "city", "region")
)]
station_curves[, rank_region := frank(-population, ties.method = "min"), by = .(point_method, radius_m)]
station_curves[, rank_city := frank(-population, ties.method = "min"), by = .(point_method, radius_m, station_geography)][station_geography != "city", rank_city := NA_integer_]
setorder(station_curves, point_method, station_name, radius_m)
fwrite(station_curves, file.path(out_dir, "station_catchment_curves_2025.csv"))
saveRDS(station_curves, file.path(out_dir, "station_catchment_curves_2025.rds"), compress = "xz")

# Nearest-station population hinterlands use every resident, including those
# farther than 1,500 m from rail.
hinterland_rows <- list()
for (method in methods) {
  station_col <- paste(method, "all_rail", "station_id", sep = "_")
  distance_col <- paste(method, "all_rail", "distance_m", sep = "_")
  x <- nearest[, .(
    station_id = get(station_col), distance_m = get(distance_col),
    estimated_population, component_key
  )]
  h <- x[, .(
    nearest_station_population = sum(estimated_population),
    component_count = .N,
    median_distance_m = weighted_median(distance_m, estimated_population),
    mean_distance_m = weighted.mean(distance_m, estimated_population)
  ), by = station_id]
  # Keep a complete 357-station analytical surface even when a station receives
  # no nearest-resident assignment under one method.
  h <- merge(data.table(station_id = stations$station_id), h, by = "station_id", all.x = TRUE)
  h[is.na(nearest_station_population), `:=`(
    nearest_station_population = 0,
    component_count = 0L
  )]
  h[, point_method := method]
  hinterland_rows[[method]] <- h
}
hinterlands <- rbindlist(hinterland_rows)
hinterlands <- merge(hinterlands, station_meta, by = "station_id", all.x = TRUE)
hinterlands[, rank_region := frank(-nearest_station_population, ties.method = "min"), by = point_method]
hinterlands[, station_geography := fifelse(grepl(cfg$phase7$madrid_municipality_code, municipality_code, fixed = TRUE), "city", "region")]
hinterlands[station_geography == "city", rank_city := frank(-nearest_station_population, ties.method = "min"), by = point_method]
fwrite(hinterlands, file.path(out_dir, "station_hinterlands_2025.csv"))

# Compact component map attributes; geometries are added by the tile build.
component_map <- nearest[, .(
  component_key, CUSEC, CUMUN, district_code, estimated_population,
  entrance_station_id = entrance_all_rail_station_id,
  entrance_distance_m = entrance_all_rail_distance_m,
  platform_station_id = platform_all_rail_station_id,
  platform_distance_m = platform_all_rail_distance_m
)]
fwrite(component_map, file.path(out_dir, "component_map_attributes_2025.csv.gz"))

cat("Calculated Phase 7 analytics for", nrow(point_dt), "population components,", nrow(stations), "stations and", length(radii), "curve radii.\n")
