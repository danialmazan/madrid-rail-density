#!/usr/bin/env Rscript
source("R/utils.R")
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(sf))

cfg <- read_config()
site_dir <- cfg$paths$site
asset_dir <- file.path(site_dir, "assets")
download_dir <- file.path(site_dir, "downloads")
derived_dir <- cfg$paths$phase6_derived
lapply(c(site_dir, asset_dir, download_dir, derived_dir), ensure_dir)

stations <- fread(file.path(cfg$paths$phase3_derived, "canonical_stations.csv"))
catchments <- fread(file.path(cfg$paths$phase4_derived, "station_catchments_2025.csv"))
regional <- fread(file.path(cfg$paths$phase4_derived, "regional_network_coverage_2025.csv"))
overlap <- fread(file.path(cfg$paths$phase4_derived, "network_overlap_2025.csv"))
regional_sensitivity <- fread(file.path(cfg$paths$phase4_derived, "regional_coverage_sensitivity_2025.csv"))
method_sensitivity <- fread(file.path(cfg$paths$phase5_derived, "regional_population_method_sensitivity_2025.csv"))
threshold_sensitivity <- fread(file.path(cfg$paths$validation, "phase5", "threshold_sensitivity_summary.csv"))
sections <- readRDS(file.path(cfg$paths$phase2_interim, "ine_sections_2025.rds"))

radii <- as.integer(unlist(cfg$phase4$radii_m))
publication_radii <- as.integer(unlist(cfg$phase6$publication_radii_m))
default_radius <- as.integer(cfg$phase6$default_radius_m)

preferred <- catchments[scenario == "preferred", .(
  station_id, radius_m, population = round(population), density = round(population_density_per_km2)
)]
reference <- catchments[scenario == "reference_point", .(
  station_id, radius_m, reference_population = round(population)
)]
station_curves <- merge(preferred, reference, by = c("station_id", "radius_m"), all.x = TRUE)

station_meta <- stations[, .(
  station_id, station_name, modes, lines, municipality_name,
  longitude = round(longitude, 6), latitude = round(latitude, 6),
  official_entrance_count, has_official_entrance,
  fallback = preferred_origin_type != "official_entrance"
)]

# A compact official-region silhouette in the same normalized coordinate space as stations.
region <- st_union(st_make_valid(sections))
region <- st_simplify(region, dTolerance = as.numeric(cfg$phase6$boundary_simplification_m), preserveTopology = TRUE)
bbox <- st_bbox(region)
map_w <- as.numeric(cfg$phase6$map_width)
map_h <- as.numeric(cfg$phase6$map_height)
pad <- 28
sx <- (map_w - 2 * pad) / as.numeric(bbox$xmax - bbox$xmin)
sy <- (map_h - 2 * pad) / as.numeric(bbox$ymax - bbox$ymin)
scale <- min(sx, sy)
draw_w <- as.numeric(bbox$xmax - bbox$xmin) * scale
draw_h <- as.numeric(bbox$ymax - bbox$ymin) * scale
offset_x <- (map_w - draw_w) / 2
offset_y <- (map_h - draw_h) / 2
project_xy <- function(x, y) list(
  x = offset_x + (x - bbox$xmin) * scale,
  y = offset_y + (bbox$ymax - y) * scale
)

coords <- st_coordinates(region)
level_cols <- grep("^L[0-9]+$", colnames(coords), value = TRUE)
ring_id <- do.call(paste, c(as.data.frame(coords[, level_cols, drop = FALSE]), sep = ":"))
rings <- split(as.data.frame(coords[, c("X", "Y"), drop = FALSE]), ring_id)
boundary_path <- paste(vapply(rings, function(ring) {
  p <- project_xy(ring$X, ring$Y)
  paste0("M", paste(sprintf("%.1f,%.1f", p$x, p$y), collapse = "L"), "Z")
}, character(1)), collapse = "")

station_xy <- project_xy(stations$reference_x, stations$reference_y)
station_meta[, `:=`(map_x = round(station_xy$x, 1), map_y = round(station_xy$y, 1))]

regional_curves <- regional[, .(
  scenario, network, radius_m, population = round(population), share = round(population_share, 6)
)]
metro_cercanias <- overlap[
  scenario == "preferred" & network_a == "metro" & network_b == "cercanias",
  .(radius_m, metro_only = round(population_a_only), cercanias_only = round(population_b_only),
    both = round(population_both), union = round(population_union), neither = round(population_neither))
]
area_sensitivity <- method_sensitivity[
  network == "any_urban_rail" & population_variant %in% c("primary_dwellings", "residential_area"),
  .(radius_m, population_variant, population = round(population), share = round(population_share, 6))
]
threshold <- threshold_sensitivity[, .(
  radius_m, median_band_population = round(median_band_population),
  median_band_share = round(median_band_share, 6), p95_band_share = round(p95_band_share, 6)
)]

payload <- list(
  meta = list(
    population = 7113886L,
    population_reference_date = "2025-01-01",
    catastro_snapshot = as.character(cfg$phase2$cat_snapshot),
    station_snapshot = as.character(cfg$phase3$station_snapshot),
    station_count = nrow(stations),
    official_entrance_count = sum(stations$official_entrance_count),
    entrance_station_count = sum(stations$has_official_entrance),
    fallback_station_count = sum(!stations$has_official_entrance),
    default_radius_m = default_radius,
    radii_m = radii,
    publication_radii_m = publication_radii,
    distance = "straight_line_euclidean",
    crs = "EPSG:25830"
  ),
  map = list(width = map_w, height = map_h, boundary_path = boundary_path),
  stations = station_meta,
  station_curves = station_curves,
  regional = regional_curves,
  metro_cercanias_overlap = metro_cercanias,
  area_sensitivity = area_sensitivity,
  threshold_sensitivity = threshold
)

json <- jsonlite::toJSON(payload, dataframe = "rows", auto_unbox = TRUE, na = "null", digits = NA)
writeLines(json, file.path(derived_dir, "phase6_data.json"), useBytes = TRUE)
writeLines(paste0("window.RAIL_DATA=", json, ";"), file.path(site_dir, "data.js"), useBytes = TRUE)

# Public downloads are compact, explicit, and retain the fallback flag.
station_download <- merge(
  station_meta[, .(station_id, station_name, modes, lines, municipality_name, longitude, latitude,
                   official_entrance_count, has_official_entrance, surface_stop_fallback = fallback)],
  catchments[scenario == "preferred", .(station_id, radius_m, population, population_density_per_km2,
                                         residential_components, distance_type)],
  by = "station_id"
)
setorder(station_download, station_name, radius_m)
fwrite(station_download, file.path(download_dir, "station-catchments-2025.csv"))
fwrite(station_meta[, .(station_id, station_name, modes, lines, municipality_name, longitude, latitude,
                        official_entrance_count, has_official_entrance, surface_stop_fallback = fallback)],
       file.path(download_dir, "station-metadata.csv"))
fwrite(regional, file.path(download_dir, "regional-network-coverage-2025.csv"))
fwrite(overlap, file.path(download_dir, "network-overlap-2025.csv"))

cat("Built Phase 6 web payload with", nrow(stations), "stations and", nrow(station_curves), "station-radius rows.\n")
