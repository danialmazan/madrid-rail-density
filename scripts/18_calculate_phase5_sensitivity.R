#!/usr/bin/env Rscript
source("R/utils.R")
source("R/catchment_helpers.R")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(data.table))

cfg <- read_config()
out_dir <- cfg$paths$phase5_derived
ensure_dir(out_dir)
radii <- as.numeric(unlist(cfg$phase4$radii_m))
selected_radii <- as.numeric(unlist(cfg$phase5$selected_radii_m))
band <- as.numeric(cfg$phase5$threshold_band_m)

population <- readRDS(file.path(cfg$paths$phase2_derived, "component_population_2025.rds"))
pop <- as.data.table(st_drop_geometry(population))
pop[, component_index := .I]
stations <- readRDS(file.path(cfg$paths$phase3_derived, "canonical_stations.rds"))
station_meta <- as.data.table(st_drop_geometry(stations))[, .(station_id, station_name, modes, lines, official_entrance_count, has_official_entrance, source_point_max_separation_m)]
preferred_distances <- readRDS(file.path(cfg$paths$phase4_derived, "component_station_distances_preferred_1500m.rds"))
nearest <- readRDS(file.path(cfg$paths$phase4_derived, "component_nearest_station_2025.rds"))
phase4_sensitivity <- fread(file.path(cfg$paths$phase4_derived, "station_catchment_sensitivity_2025.csv"))
boundary <- fread(file.path(cfg$paths$validation, "phase2", "point_vs_boundary_split_sensitivity.csv.gz"))

normalized_population <- function(weight) {
  x <- data.table(CUSEC = pop$CUSEC, section_population = pop$section_population, weight = weight)
  x[, section_weight := sum(weight), by = CUSEC]
  if (any(x$section_population > 0 & x$section_weight <= 0)) stop("Sensitivity weight leaves a populated section unsupported.")
  x[, section_population * weight / section_weight]
}

weights <- data.table(
  component_index = pop$component_index,
  component_key = pop$component_key,
  CUSEC = pop$CUSEC,
  primary_dwellings = pop$estimated_population,
  residential_area = pop$estimated_population_area,
  component_footprint = normalized_population(pop$dwellings_footprint),
  component_equal = normalized_population(pop$dwellings_equal),
  component_volume_all = normalized_population(pop$dwellings_volume_all)
)
boundary_index <- match(weights$component_key, boundary$component_key)
if (anyNA(boundary_index)) stop("Phase 2 boundary sensitivity does not cover every population component.")
weights[, section_boundary_area_split := boundary$split_population[boundary_index]]
variant_names <- as.character(unlist(cfg$phase5$population_variants))
if (!all(variant_names %in% names(weights))) stop("Configured Phase 5 population variant is missing.")

saveRDS(weights, file.path(out_dir, "component_population_sensitivity_weights_2025.rds"), compress = "gzip")

station_variant_rows <- lapply(variant_names, function(variant) {
  message("Station population sensitivity: ", variant)
  x <- aggregate_station_catchments(preferred_distances, weights[[variant]], stations, radii, variant)
  setnames(x, "scenario", "population_variant")
  x
})
station_variants <- rbindlist(station_variant_rows)
station_variants <- merge(station_variants, station_meta, by = "station_id", all.x = TRUE)
setorder(station_variants, population_variant, station_name, radius_m)
fwrite(station_variants, file.path(out_dir, "station_population_method_sensitivity_2025.csv"))
saveRDS(station_variants, file.path(out_dir, "station_population_method_sensitivity_2025.rds"), compress = "xz")

primary_station <- station_variants[population_variant == "primary_dwellings", .(
  station_id, radius_m, primary_population = population
)]
primary_station[, primary_rank := frank(-primary_population, ties.method = "min"), by = radius_m]
station_comparison <- merge(station_variants, primary_station, by = c("station_id", "radius_m"), all.x = TRUE)
station_comparison[, `:=`(
  population_difference = population - primary_population,
  absolute_population_difference = abs(population - primary_population),
  relative_population_difference = fifelse(primary_population > 0, (population - primary_population) / primary_population, NA_real_),
  variant_rank = frank(-population, ties.method = "min")
), by = .(population_variant, radius_m)]
fwrite(station_comparison, file.path(out_dir, "station_population_method_comparison_2025.csv"))

ranking_metrics <- station_comparison[, .(
  spearman_rank_correlation = cor(primary_rank, variant_rank, method = "spearman"),
  top10_overlap_count = sum(station_id[variant_rank <= 10] %in% station_id[primary_rank <= 10]),
  top10_overlap_share = sum(station_id[variant_rank <= 10] %in% station_id[primary_rank <= 10]) / 10,
  median_absolute_population_difference = median(absolute_population_difference),
  p95_absolute_population_difference = as.numeric(quantile(absolute_population_difference, 0.95)),
  maximum_absolute_population_difference = max(absolute_population_difference)
), by = .(population_variant, radius_m)]
fwrite(ranking_metrics, file.path(out_dir, "station_ranking_sensitivity_2025.csv"))

networks <- c("any_urban_rail", "metro", "metro_ligero", "cercanias", "tranvia_parla")
regional_rows <- list()
k <- 0L
for (variant in variant_names) {
  for (network in networks) {
    distance_column <- paste("preferred", network, "distance_m", sep = "_")
    for (radius in radii) {
      k <- k + 1L
      value <- sum(weights[[variant]][nearest[[distance_column]] <= radius])
      regional_rows[[k]] <- data.table(
        population_variant = variant, network = network, radius_m = radius,
        population = value, population_share = value / sum(weights[[variant]])
      )
    }
  }
}
regional_variants <- rbindlist(regional_rows)
primary_regional <- regional_variants[population_variant == "primary_dwellings", .(network, radius_m, primary_population = population, primary_share = population_share)]
regional_comparison <- merge(regional_variants, primary_regional, by = c("network", "radius_m"), all.x = TRUE)
regional_comparison[, `:=`(
  population_difference = population - primary_population,
  population_share_difference = population_share - primary_share
)]
fwrite(regional_comparison, file.path(out_dir, "regional_population_method_sensitivity_2025.csv"))

variant_conservation <- rbindlist(lapply(variant_names, function(variant) {
  section <- data.table(CUSEC = weights$CUSEC, population = weights[[variant]])[, .(allocated_population = sum(population)), by = CUSEC]
  target <- unique(pop[, .(CUSEC, section_population)])
  section <- merge(section, target, by = "CUSEC")
  data.table(
    population_variant = variant,
    regional_population = sum(weights[[variant]]),
    conservation_domain = if (variant == "section_boundary_area_split") "regional_and_fragment_sections" else "original_point_assigned_sections",
    maximum_original_section_residual = if (variant == "section_boundary_area_split") NA_real_ else max(abs(section$allocated_population - section$section_population)),
    component_total_variation_share = sum(abs(weights[[variant]] - weights$primary_dwellings)) / (2 * sum(weights$primary_dwellings))
  )
}))
fwrite(variant_conservation, file.path(out_dir, "population_variant_conservation.csv"))

# Exact +/- 50 m threshold bands around the publication radii.
thresholds <- sort(unique(c(selected_radii - band, selected_radii, selected_radii + band)))
distance_population <- copy(preferred_distances)
distance_population[, estimated_population := weights$primary_dwellings[component_index]]
threshold_rows <- lapply(thresholds, function(threshold) {
  x <- distance_population[distance_m <= threshold, .(population = sum(estimated_population), residential_components = .N), by = station_id]
  grid <- data.table(station_id = stations$station_id)
  x <- merge(grid, x, by = "station_id", all.x = TRUE)
  x[is.na(population), `:=`(population = 0, residential_components = 0L)]
  x[, threshold_m := threshold]
  x
})
threshold_station <- rbindlist(threshold_rows)
threshold_station <- merge(threshold_station, station_meta, by = "station_id")
fwrite(threshold_station, file.path(out_dir, "station_threshold_50m_sensitivity_2025.csv"))

threshold_comparison <- rbindlist(lapply(selected_radii, function(radius) {
  low <- threshold_station[threshold_m == radius - band, .(station_id, lower_population = population)]
  center <- threshold_station[threshold_m == radius, .(station_id, center_population = population)]
  high <- threshold_station[threshold_m == radius + band, .(station_id, upper_population = population)]
  x <- Reduce(function(a, b) merge(a, b, by = "station_id"), list(low, center, high))
  x[, `:=`(
    radius_m = radius,
    population_in_lower_50m = center_population - lower_population,
    population_in_upper_50m = upper_population - center_population,
    population_in_100m_band = upper_population - lower_population,
    band_share_of_center = fifelse(center_population > 0, (upper_population - lower_population) / center_population, NA_real_)
  )]
  x
}))
threshold_comparison <- merge(threshold_comparison, station_meta, by = "station_id")
fwrite(threshold_comparison, file.path(out_dir, "station_threshold_band_comparison_2025.csv"))

regional_threshold <- rbindlist(lapply(thresholds, function(threshold) {
  data.table(
    threshold_m = threshold,
    population = sum(weights$primary_dwellings[nearest$preferred_any_urban_rail_distance_m <= threshold])
  )
}))
regional_threshold[, population_share := population / sum(weights$primary_dwellings)]
fwrite(regional_threshold, file.path(out_dir, "regional_threshold_50m_sensitivity_2025.csv"))

# Consolidate entrance/reference sensitivity into the Phase 5 naming scheme.
entrance_metrics <- phase4_sensitivity[, .(
  spearman_rank_correlation = cor(population_preferred, population_reference, method = "spearman"),
  top10_overlap_count = sum(station_id[frank(-population_preferred, ties.method = "min") <= 10] %in% station_id[frank(-population_reference, ties.method = "min") <= 10]),
  median_absolute_population_difference = median(absolute_population_difference),
  p95_absolute_population_difference = as.numeric(quantile(absolute_population_difference, 0.95)),
  maximum_absolute_population_difference = max(absolute_population_difference)
), by = radius_m]
fwrite(entrance_metrics, file.path(out_dir, "entrance_reference_sensitivity_summary_2025.csv"))

cat("Calculated Phase 5 sensitivity outputs for", length(variant_names), "population variants and", length(selected_radii), "publication radii.\n")
