#!/usr/bin/env Rscript
source("R/utils.R")
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(sf))

cfg <- read_config()
out_dir <- cfg$paths$phase7_derived
frontend_public <- file.path(cfg$paths$phase7_frontend, "public")
data_dir <- file.path(frontend_public, "data")
download_dir <- file.path(frontend_public, "downloads")
lapply(c(frontend_public, data_dir, download_dir), ensure_dir)

curves <- fread(file.path(out_dir, "coverage_curves_2025.csv"))
histograms <- fread(file.path(out_dir, "distance_histograms_2025.csv"))
metrics <- fread(file.path(out_dir, "area_distance_metrics_2025.csv"))
station_curves <- fread(file.path(out_dir, "station_catchment_curves_2025.csv"))
hinterlands <- fread(file.path(out_dir, "station_hinterlands_2025.csv"))
stations_sf <- readRDS(file.path(cfg$paths$phase3_derived, "canonical_stations.rds"))
platforms <- readRDS(file.path(cfg$paths$phase3_derived, "station_platforms.rds"))
origins <- readRDS(file.path(cfg$paths$phase3_derived, "station_origins_phase7.rds"))
sections <- readRDS(file.path(cfg$paths$phase2_interim, "ine_sections_2025.rds"))

stations <- as.data.table(st_drop_geometry(stations_sf))[, .(
  station_id, station_name, modes, lines, municipality_code, municipality_name,
  longitude = round(longitude, 6), latitude = round(latitude, 6),
  official_entrance_count, has_official_entrance,
  entrance_fallback = !has_official_entrance
)]
platform_counts <- as.data.table(st_drop_geometry(platforms))[, .(official_platform_count = .N), by = station_id]
stations <- merge(stations, platform_counts, by = "station_id", all.x = TRUE)
stations[is.na(official_platform_count), official_platform_count := 0L]

round_payload <- function() {
  curves[, `:=`(population = round(population), population_share = round(population_share, 7), geography_population = round(geography_population))]
  histograms[, `:=`(population = round(population), population_share = round(population_share, 7), geography_population = round(geography_population))]
  metrics[, `:=`(population = round(population), median_distance_m = round(median_distance_m, 1), mean_distance_m = round(mean_distance_m, 1))]
  station_curves[, `:=`(
    population = round(population), catchment_area_km2 = round(catchment_area_km2, 5),
    population_density_per_km2 = round(population_density_per_km2),
    longitude = round(longitude, 6), latitude = round(latitude, 6)
  )]
  hinterlands[, `:=`(
    nearest_station_population = round(nearest_station_population),
    median_distance_m = round(median_distance_m, 1), mean_distance_m = round(mean_distance_m, 1),
    longitude = round(longitude, 6), latitude = round(latitude, 6)
  )]
}
round_payload()

region <- st_transform(st_as_sf(st_union(sections)), 4326)
city_sections <- sections[sections$CUMUN == cfg$phase7$madrid_municipality_code, ]
city <- st_transform(st_as_sf(st_union(city_sections)), 4326)
bbox_values <- function(x) {
  b <- st_bbox(x)
  unname(c(b$xmin, b$ymin, b$xmax, b$ymax))
}

districts <- data.table(
  district_code = names(cfg$phase7$district_names),
  district_id = paste0("district_", names(cfg$phase7$district_names)),
  district_name = unname(unlist(cfg$phase7$district_names))
)

payload <- list(
  meta = list(
    version = "phase7-2025-v1",
    population_reference_date = as.character(cfg$phase2$population_reference_date),
    catastro_snapshot = as.character(cfg$phase2$cat_snapshot),
    station_snapshot = as.character(cfg$phase3$station_snapshot),
    population = round(sum(curves[geography_id == "region" & point_method == "entrance" & rail_network == "all_rail" & radius_m == 1500, geography_population][1])),
    station_count = nrow(stations),
    entrance_count = sum(stations$official_entrance_count),
    platform_count = nrow(platforms),
    fallback_station_count = sum(stations$entrance_fallback),
    curve_step_m = as.integer(cfg$phase7$curve_step_m),
    histogram_step_m = as.integer(cfg$phase7$histogram_step_m),
    maximum_distance_m = as.integer(cfg$phase7$maximum_distance_m),
    default_radius_m = as.integer(cfg$phase7$default_radius_m),
    distance_type = "straight_line_euclidean",
    crs = paste0("EPSG:", cfg$project$crs)
  ),
  map = list(
    region_bbox = bbox_values(region), city_bbox = bbox_values(city),
    basemap_style = "https://tiles.openfreemap.org/styles/positron"
  ),
  districts = districts,
  stations = stations,
  coverage_curves = curves,
  distance_histograms = histograms,
  area_metrics = metrics,
  station_curves = station_curves,
  hinterlands = hinterlands
)

json <- jsonlite::toJSON(payload, dataframe = "rows", auto_unbox = TRUE, na = "null", digits = NA)
writeLines(json, file.path(data_dir, "phase7.json"), useBytes = TRUE)

origin_lonlat <- st_coordinates(st_transform(origins, 4326))
origin_download <- as.data.table(st_drop_geometry(origins))
origin_download[, `:=`(longitude = origin_lonlat[, 1], latitude = origin_lonlat[, 2])]
fwrite(origin_download, file.path(download_dir, "station-origins-phase7.csv"))

downloads <- c(
  coverage_curves = "coverage-curves-2025.csv",
  distance_histograms = "distance-histograms-2025.csv",
  area_distance_metrics = "area-distance-metrics-2025.csv",
  station_catchment_curves = "station-catchment-curves-2025.csv",
  station_hinterlands = "station-hinterlands-2025.csv"
)
sources <- c(
  file.path(out_dir, "coverage_curves_2025.csv"),
  file.path(out_dir, "distance_histograms_2025.csv"),
  file.path(out_dir, "area_distance_metrics_2025.csv"),
  file.path(out_dir, "station_catchment_curves_2025.csv"),
  file.path(out_dir, "station_hinterlands_2025.csv")
)
for (i in seq_along(downloads)) file.copy(sources[i], file.path(download_dir, downloads[i]), overwrite = TRUE)

if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(curves, file.path(download_dir, "coverage-curves-2025.parquet"), compression = "zstd")
  arrow::write_parquet(histograms, file.path(download_dir, "distance-histograms-2025.parquet"), compression = "zstd")
  arrow::write_parquet(metrics, file.path(download_dir, "area-distance-metrics-2025.parquet"), compression = "zstd")
  arrow::write_parquet(station_curves, file.path(download_dir, "station-catchment-curves-2025.parquet"), compression = "zstd")
  arrow::write_parquet(hinterlands, file.path(download_dir, "station-hinterlands-2025.parquet"), compression = "zstd")
}

cat("Built Phase 7 web payload:", format(file.info(file.path(data_dir, "phase7.json"))$size, big.mark = ","), "bytes.\n")
