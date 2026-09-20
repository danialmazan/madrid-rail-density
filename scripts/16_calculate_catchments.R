#!/usr/bin/env Rscript
source("R/utils.R")
source("R/catchment_helpers.R")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(data.table))

cfg <- read_config()
out_dir <- cfg$paths$phase4_derived
ensure_dir(out_dir)
radii <- as.numeric(unlist(cfg$phase4$radii_m))
maximum_distance <- as.numeric(cfg$phase4$maximum_distance_m)
if (!identical(radii, seq(100, 1500, 100))) stop("Phase 4 radii must be 100 m increments from 100 to 1,500 m.")

population <- readRDS(file.path(cfg$paths$phase2_derived, "component_population_2025.rds"))
stations <- readRDS(file.path(cfg$paths$phase3_derived, "canonical_stations.rds"))
preferred_origins <- readRDS(file.path(cfg$paths$phase3_derived, "station_distance_origins.rds"))
reference_origins <- readRDS(file.path(cfg$paths$phase3_derived, "station_reference_points.rds"))
services <- fread(file.path(cfg$paths$phase3_derived, "station_services.csv"))
sections <- readRDS(file.path(cfg$paths$phase2_interim, "ine_sections_2025.rds"))

spatial_objects <- list(population = population, stations = stations, preferred_origins = preferred_origins, reference_origins = reference_origins)
crs_values <- vapply(spatial_objects, function(x) st_crs(x)$epsg, integer(1))
if (any(crs_values != cfg$project$crs)) stop("All Phase 4 spatial inputs must use EPSG:", cfg$project$crs)
if (any(st_geometry_type(population) != "POINT")) stop("Phase 2 population support must be POINT geometry.")

points <- population[, c("component_key", "CUSEC", "estimated_population")]
point_dt <- as.data.table(st_drop_geometry(points))
point_dt[, component_index := .I]
regional_population <- sum(point_dt$estimated_population)

message("Calculating entrance-first component-to-station distances...")
preferred_distances <- component_station_distances(points, preferred_origins, maximum_distance, "preferred")
message("Calculating station-point sensitivity distances...")
reference_distances <- component_station_distances(points, reference_origins, maximum_distance, "reference")

preferred_station <- aggregate_station_catchments(preferred_distances, point_dt$estimated_population, stations, radii, "preferred")
reference_station <- aggregate_station_catchments(reference_distances, point_dt$estimated_population, stations, radii, "reference_point")
station_catchments <- rbindlist(list(preferred_station, reference_station))

message("Building station catchment geometries and areas...")
region <- st_union(st_geometry(sections))
preferred_geometries <- catchment_geometries(preferred_origins, radii, region, "preferred")
reference_geometries <- catchment_geometries(reference_origins, radii, region, "reference_point")
catchment_geometries_sf <- rbind(preferred_geometries, reference_geometries)
area_dt <- as.data.table(st_drop_geometry(catchment_geometries_sf))[, .(station_id, radius_m, scenario, catchment_area_km2)]
station_catchments <- merge(station_catchments, area_dt, by = c("station_id", "radius_m", "scenario"), all.x = TRUE)
station_meta <- as.data.table(st_drop_geometry(stations))[, .(
  station_id, station_name, modes, lines, mode_count, line_count,
  official_entrance_count, has_official_entrance, preferred_origin_type
)]
station_catchments <- merge(station_catchments, station_meta, by = "station_id", all.x = TRUE)
station_catchments[, `:=`(
  population_density_per_km2 = fifelse(catchment_area_km2 > 0, population / catchment_area_km2, NA_real_),
  population_reference_date = cfg$phase2$population_reference_date,
  station_snapshot = cfg$phase3$station_snapshot,
  distance_crs = paste0("EPSG:", cfg$project$crs),
  distance_type = "straight_line_euclidean"
)]
setorder(station_catchments, scenario, station_name, radius_m)

preferred_distances[, component_key := point_dt$component_key[component_index]]
reference_distances[, component_key := point_dt$component_key[component_index]]
saveRDS(preferred_distances, file.path(out_dir, "component_station_distances_preferred_1500m.rds"), compress = "gzip")
saveRDS(reference_distances, file.path(out_dir, "component_station_distances_reference_1500m.rds"), compress = "gzip")
if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(preferred_distances, file.path(out_dir, "component_station_distances_preferred_1500m.parquet"), compression = "zstd")
  arrow::write_parquet(reference_distances, file.path(out_dir, "component_station_distances_reference_1500m.parquet"), compression = "zstd")
}
fwrite(station_catchments, file.path(out_dir, "station_catchments_2025.csv"))
saveRDS(station_catchments, file.path(out_dir, "station_catchments_2025.rds"), compress = "xz")
saveRDS(catchment_geometries_sf, file.path(out_dir, "station_catchment_geometries_2025.rds"), compress = "xz")

message("Calculating nearest-station distances for regional and network coverage...")
networks <- c("any_urban_rail", "metro", "metro_ligero", "cercanias", "tranvia_parla")
scenarios <- c("preferred", "reference_point")
nearest <- copy(point_dt)
for (scenario in scenarios) {
  origin_layer <- if (scenario == "preferred") preferred_origins else reference_origins
  for (network in networks) {
    station_ids <- if (network == "any_urban_rail") unique(stations$station_id) else unique(services[mode == network, station_id])
    network_origins <- origin_layer[origin_layer$station_id %in% station_ids, ]
    if (!nrow(network_origins)) stop("No distance origins for network: ", network)
    result <- nearest_network_distance(points, network_origins)
    nearest[, (paste(scenario, network, "station_id", sep = "_")) := result$nearest_station_id]
    nearest[, (paste(scenario, network, "distance_m", sep = "_")) := result$distance_m]
    message("Nearest distances: ", scenario, " / ", network)
  }
}
saveRDS(nearest, file.path(out_dir, "component_nearest_station_2025.rds"), compress = "gzip")
if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(nearest, file.path(out_dir, "component_nearest_station_2025.parquet"), compression = "zstd")
}

regional_coverage <- regional_coverage_table(nearest, radii, networks, scenarios, regional_population)
regional_coverage[, `:=`(
  population_reference_date = cfg$phase2$population_reference_date,
  distance_type = "straight_line_euclidean"
)]
setorder(regional_coverage, scenario, network, radius_m)
fwrite(regional_coverage, file.path(out_dir, "regional_network_coverage_2025.csv"))
saveRDS(regional_coverage, file.path(out_dir, "regional_network_coverage_2025.rds"), compress = "xz")

mode_networks <- c("metro", "metro_ligero", "cercanias", "tranvia_parla")
overlap <- network_overlap_table(nearest, radii, mode_networks, scenarios, regional_population)
setorder(overlap, scenario, network_a, network_b, radius_m)
fwrite(overlap, file.path(out_dir, "network_overlap_2025.csv"))
saveRDS(overlap, file.path(out_dir, "network_overlap_2025.rds"), compress = "xz")

station_sensitivity <- merge(
  station_catchments[scenario == "preferred"],
  station_catchments[scenario == "reference_point"],
  by = c("station_id", "radius_m"), suffixes = c("_preferred", "_reference")
)
station_sensitivity[, `:=`(
  population_difference = population_preferred - population_reference,
  absolute_population_difference = abs(population_preferred - population_reference),
  relative_population_difference = fifelse(population_reference > 0, (population_preferred - population_reference) / population_reference, NA_real_),
  area_difference_km2 = catchment_area_km2_preferred - catchment_area_km2_reference
)]
fwrite(station_sensitivity, file.path(out_dir, "station_catchment_sensitivity_2025.csv"))
saveRDS(station_sensitivity, file.path(out_dir, "station_catchment_sensitivity_2025.rds"), compress = "xz")

regional_sensitivity <- merge(
  regional_coverage[scenario == "preferred"],
  regional_coverage[scenario == "reference_point"],
  by = c("network", "radius_m"), suffixes = c("_preferred", "_reference")
)
regional_sensitivity[, `:=`(
  population_difference = population_preferred - population_reference,
  absolute_population_difference = abs(population_preferred - population_reference),
  relative_population_difference = fifelse(population_reference > 0, (population_preferred - population_reference) / population_reference, NA_real_)
)]
fwrite(regional_sensitivity, file.path(out_dir, "regional_coverage_sensitivity_2025.csv"))

cat("Calculated Phase 4 catchments for", nrow(stations), "stations,", length(radii), "radii and two geometry scenarios.\n")
