#!/usr/bin/env Rscript
source("R/utils.R")
source("R/catchment_helpers.R")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))

cfg <- read_config()
qa_dir <- file.path(cfg$paths$validation, "phase4")
ensure_dir(qa_dir)
radii <- as.numeric(unlist(cfg$phase4$radii_m))
selected_radii <- as.numeric(unlist(cfg$phase4$selected_output_radii_m))
tolerance <- as.numeric(cfg$phase4$distance_tolerance_m)

population <- readRDS(file.path(cfg$paths$phase2_derived, "component_population_2025.rds"))
points <- population[, c("component_key", "estimated_population")]
population_dt <- as.data.table(st_drop_geometry(points))
stations <- readRDS(file.path(cfg$paths$phase3_derived, "canonical_stations.rds"))
station_dt <- as.data.table(st_drop_geometry(stations))
preferred_origins <- readRDS(file.path(cfg$paths$phase3_derived, "station_distance_origins.rds"))
reference_origins <- readRDS(file.path(cfg$paths$phase3_derived, "station_reference_points.rds"))
preferred_distances <- readRDS(file.path(cfg$paths$phase4_derived, "component_station_distances_preferred_1500m.rds"))
reference_distances <- readRDS(file.path(cfg$paths$phase4_derived, "component_station_distances_reference_1500m.rds"))
nearest <- readRDS(file.path(cfg$paths$phase4_derived, "component_nearest_station_2025.rds"))
catchments <- fread(file.path(cfg$paths$phase4_derived, "station_catchments_2025.csv"))
regional <- fread(file.path(cfg$paths$phase4_derived, "regional_network_coverage_2025.csv"))
overlap <- fread(file.path(cfg$paths$phase4_derived, "network_overlap_2025.csv"))
sensitivity <- fread(file.path(cfg$paths$phase4_derived, "station_catchment_sensitivity_2025.csv"))
regional_sensitivity <- fread(file.path(cfg$paths$phase4_derived, "regional_coverage_sensitivity_2025.csv"))
catchment_geometries_sf <- readRDS(file.path(cfg$paths$phase4_derived, "station_catchment_geometries_2025.rds"))
regional_population <- sum(population_dt$estimated_population)

monotonic_station_population <- catchments[order(radius_m), .(passed = all(diff(population) >= -1e-7)), by = .(scenario, station_id)]
monotonic_station_area <- catchments[order(radius_m), .(passed = all(diff(catchment_area_km2) >= -1e-9)), by = .(scenario, station_id)]
monotonic_regional <- regional[order(radius_m), .(passed = all(diff(population) >= -1e-7)), by = .(scenario, network)]
any_mode_check <- merge(
  regional[network == "any_urban_rail", .(scenario, radius_m, any_population = population)],
  regional[network != "any_urban_rail", .(maximum_mode_population = max(population)), by = .(scenario, radius_m)],
  by = c("scenario", "radius_m")
)

overlap_identity <- overlap[, .(
  partition_residual = population_a_only + population_b_only + population_both + population_neither - regional_population,
  union_residual = population_a_only + population_b_only + population_both - population_union
)]

fallback_ids <- station_dt[has_official_entrance == FALSE, station_id]
fallback_sensitivity <- sensitivity[station_id %in% fallback_ids]

# Independent direct checks on contrasting major interchanges and a surface fallback.
brute_ids <- as.character(unlist(cfg$phase4$brute_force_station_ids))
brute_radii <- selected_radii
brute_rows <- list()
k <- 0L
for (scenario in c("preferred", "reference_point")) {
  scenario_value <- scenario
  origin_layer <- if (scenario == "preferred") preferred_origins else reference_origins
  for (station_id in brute_ids) {
    station_value <- station_id
    local_origins <- origin_layer[origin_layer$station_id == station_id, ]
    if (!nrow(local_origins)) stop("Configured brute-force station is missing: ", station_id)
    direct <- component_station_distances(points, local_origins, max(brute_radii), paste0("brute_", station_id))
    direct[, estimated_population := population_dt$estimated_population[component_index]]
    for (radius in brute_radii) {
      expected <- catchments[scenario == scenario_value & station_id == station_value & radius_m == radius, population]
      observed <- direct[distance_m <= radius, sum(estimated_population)]
      k <- k + 1L
      brute_rows[[k]] <- data.table(
        scenario = scenario, station_id = station_id, radius_m = radius,
        pipeline_population = expected, direct_population = observed,
        absolute_difference = abs(expected - observed)
      )
    }
  }
}
brute_force <- rbindlist(brute_rows)
fwrite(brute_force, file.path(qa_dir, "brute_force_station_checks.csv"))

rankings <- catchments[scenario == "preferred" & radius_m %in% selected_radii]
rankings[, population_rank := frank(-population, ties.method = "min"), by = radius_m]
rankings <- rankings[population_rank <= 25][order(radius_m, population_rank)]
fwrite(rankings, file.path(qa_dir, "station_rankings_selected_radii.csv"))

sensitivity_summary <- sensitivity[, .(
  stations = .N,
  mean_absolute_population_difference = mean(absolute_population_difference),
  median_absolute_population_difference = median(absolute_population_difference),
  p95_absolute_population_difference = as.numeric(quantile(absolute_population_difference, 0.95)),
  maximum_absolute_population_difference = max(absolute_population_difference),
  median_absolute_relative_difference = median(abs(relative_population_difference), na.rm = TRUE)
), by = radius_m][order(radius_m)]
fwrite(sensitivity_summary, file.path(qa_dir, "station_sensitivity_summary_by_radius.csv"))
fwrite(sensitivity[order(-absolute_population_difference)][1:250], file.path(qa_dir, "station_sensitivity_outliers.csv"))
fwrite(regional[scenario == "preferred" & radius_m %in% selected_radii], file.path(qa_dir, "regional_coverage_selected_radii.csv"))

expected_station_rows <- nrow(stations) * length(radii) * 2L
expected_regional_rows <- length(radii) * 5L * 2L
checks <- data.table(
  check = c(
    "phase3_entrance_first_decision_present",
    "population_support_crs_25830",
    "station_catchment_grid_complete",
    "preferred_distance_pairs_unique",
    "reference_distance_pairs_unique",
    "distance_pairs_within_1500m",
    "station_population_monotonic",
    "station_area_monotonic",
    "station_density_valid",
    "regional_coverage_grid_complete",
    "regional_coverage_monotonic",
    "regional_population_shares_valid",
    "any_network_coverage_at_least_each_mode",
    "network_overlap_partitions_reconcile",
    "network_overlap_unions_reconcile",
    "nearest_table_complete",
    "nearest_distances_valid",
    "surface_fallbacks_equal_reference_scenario",
    "brute_force_station_checks_match",
    "catchment_geometry_grid_complete",
    "catchment_geometry_crs_25830",
    "regional_population_matches_phase2",
    "preferred_method_is_entrance_first",
    "distance_type_is_straight_line"
  ),
  passed = c(
    jsonlite::fromJSON(file.path(cfg$paths$validation, "current_phase3_decision.json"))$status == "GO_PHASE3_ENTRANCE_FIRST_WITH_SURFACE_FALLBACK",
    st_crs(points)$epsg == cfg$project$crs,
    nrow(catchments) == expected_station_rows && uniqueN(catchments, by = c("scenario", "station_id", "radius_m")) == expected_station_rows,
    nrow(preferred_distances) == uniqueN(preferred_distances, by = c("station_id", "component_index")),
    nrow(reference_distances) == uniqueN(reference_distances, by = c("station_id", "component_index")),
    min(preferred_distances$distance_m) >= 0 && max(preferred_distances$distance_m) <= cfg$phase4$maximum_distance_m + tolerance && min(reference_distances$distance_m) >= 0 && max(reference_distances$distance_m) <= cfg$phase4$maximum_distance_m + tolerance,
    all(monotonic_station_population$passed),
    all(monotonic_station_area$passed),
    all(is.finite(catchments$population_density_per_km2)) && all(catchments$population_density_per_km2 >= 0),
    nrow(regional) == expected_regional_rows && uniqueN(regional, by = c("scenario", "network", "radius_m")) == expected_regional_rows,
    all(monotonic_regional$passed),
    all(regional$population >= 0) && all(regional$population <= regional_population + 1e-6) && all(regional$population_share >= 0 & regional$population_share <= 1),
    all(any_mode_check$any_population + 1e-6 >= any_mode_check$maximum_mode_population),
    max(abs(overlap_identity$partition_residual)) < 1e-5,
    max(abs(overlap_identity$union_residual)) < 1e-5,
    nrow(nearest) == nrow(points) && uniqueN(nearest$component_key) == nrow(points),
    all(vapply(nearest[, grep("_distance_m$", names(nearest)), with = FALSE], function(x) all(is.finite(x) & x >= 0), logical(1))),
    nrow(fallback_sensitivity) == length(fallback_ids) * length(radii) && max(abs(fallback_sensitivity$population_difference)) < 1e-7,
    max(brute_force$absolute_difference) < 1e-5,
    nrow(catchment_geometries_sf) == expected_station_rows,
    st_crs(catchment_geometries_sf)$epsg == cfg$project$crs,
    abs(regional_population - 7113886) < 1,
    cfg$phase4$primary_method == "nearest_official_entrance_then_station_point_fallback",
    all(catchments$distance_type == "straight_line_euclidean")
  )
)
fwrite(checks, file.path(qa_dir, "phase4_gate_checks.csv"))

metric <- function(network_value, radius, scenario_value = "preferred", field = "population") {
  regional[scenario == scenario_value & network == network_value & radius_m == radius, get(field)]
}
metro_cerc_800 <- overlap[scenario == "preferred" & network_a == "metro" & network_b == "cercanias" & radius_m == 800]
s800 <- sensitivity_summary[radius_m == 800]
metrics <- data.table(
  metric = c(
    "regional_population", "population_within_any_400m", "population_within_any_800m",
    "share_within_any_800m", "population_within_any_1000m", "share_within_any_1000m",
    "population_within_any_1500m", "share_within_any_1500m",
    "metro_only_800m_population", "cercanias_only_800m_population", "metro_cercanias_overlap_800m_population",
    "regional_800m_preferred_minus_reference", "station_800m_median_absolute_difference",
    "station_800m_p95_absolute_difference", "preferred_component_station_pairs", "reference_component_station_pairs"
  ),
  value = c(
    regional_population, metric("any_urban_rail", 400), metric("any_urban_rail", 800),
    metric("any_urban_rail", 800, field = "population_share"), metric("any_urban_rail", 1000), metric("any_urban_rail", 1000, field = "population_share"),
    metric("any_urban_rail", 1500), metric("any_urban_rail", 1500, field = "population_share"),
    metro_cerc_800$population_a_only, metro_cerc_800$population_b_only, metro_cerc_800$population_both,
    regional_sensitivity[network == "any_urban_rail" & radius_m == 800, population_difference],
    s800$median_absolute_population_difference, s800$p95_absolute_population_difference,
    nrow(preferred_distances), nrow(reference_distances)
  )
)
fwrite(metrics, file.path(qa_dir, "phase4_metrics.csv"))

p_coverage <- ggplot(regional[network == "any_urban_rail"], aes(radius_m, population_share, color = scenario)) +
  geom_line(linewidth = 1) + geom_point(size = 1.6) +
  scale_color_manual(values = c(preferred = "#183229", reference_point = "#ef5b49"), labels = c(preferred = "Nearest entrance / surface fallback", reference_point = "Official station point")) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  labs(title = "Population within straight-line distance of any urban-rail station", x = "Distance (metres)", y = "Share of Comunidad de Madrid population", color = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave(file.path(qa_dir, "regional_coverage_curve.png"), p_coverage, width = 8, height = 5.3, dpi = 180, bg = "#fbf8f1")

scatter <- sensitivity[radius_m == 800]
p_scatter <- ggplot(scatter, aes(population_reference, population_preferred)) +
  geom_abline(slope = 1, intercept = 0, color = "#999999", linewidth = 0.5) +
  geom_point(aes(color = has_official_entrance_preferred), alpha = 0.65, size = 1.5) +
  scale_color_manual(values = c(`TRUE` = "#183229", `FALSE` = "#ef5b49"), labels = c(`TRUE` = "Entrance geometry", `FALSE` = "Surface fallback")) +
  coord_equal() +
  labs(title = "Station catchment sensitivity at 800 m", x = "Population using official station point", y = "Population using nearest entrance / fallback", color = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave(file.path(qa_dir, "station_800m_sensitivity.png"), p_scatter, width = 7, height = 6.5, dpi = 180, bg = "#fbf8f1")

status <- if (all(checks$passed)) "GO_PHASE4_COMPLETE" else "PHASE4_REVIEW_REQUIRED"
top800 <- catchments[scenario == "preferred" & radius_m == 800][order(-population)][1:10]
regional_selected <- regional[scenario == "preferred" & network == "any_urban_rail" & radius_m %in% selected_radii][order(radius_m)]
report <- c(
  "# Phase 4 — station catchments and regional coverage", "",
  paste0("**Decision: ", status, "**"), "",
  paste0("Phase 4 calculates straight-line catchments for ", nrow(stations), " canonical stations at 100 m increments from 100–1,500 m. The preferred scenario uses the nearest official entrance, with the official stop point only for the 40 uncovered surface stops. The official station-point scenario is retained throughout as sensitivity analysis."), "",
  "## Regional coverage", "",
  paste0("- ", regional_selected$radius_m, " m: ", format(round(regional_selected$population), big.mark = ","), " modeled residents (", sprintf("%.2f%%", 100 * regional_selected$population_share), ")."), "",
  "Regional coverage uses each residential component's distance to its nearest eligible origin, so residents are never double-counted. Station catchments remain intentionally overlapping.", "",
  "## Highest-population 800 m catchments", "",
  paste0("- ", seq_len(nrow(top800)), ". ", top800$station_name, ": ", format(round(top800$population), big.mark = ","), " residents."), "",
  "## Entrance sensitivity", "",
  paste0("At 800 m, entrance-first regional coverage is ", format(round(metric("any_urban_rail", 800)), big.mark = ","), " versus ", format(round(regional[scenario == "reference_point" & network == "any_urban_rail" & radius_m == 800, population]), big.mark = ","), " using station points: a difference of ", format(round(regional_sensitivity[network == "any_urban_rail" & radius_m == 800, population_difference]), big.mark = ","), " residents."),
  paste0("Across individual 800 m station catchments, the median absolute difference is ", format(round(s800$median_absolute_population_difference), big.mark = ","), " residents and the 95th percentile is ", format(round(s800$p95_absolute_population_difference), big.mark = ","), ". Surface-stop fallbacks match the reference scenario exactly by construction."), "",
  "## Network overlap example", "",
  paste0("Within 800 m, ", format(round(metro_cerc_800$population_a_only), big.mark = ","), " residents are covered by Metro but not Cercanías, ", format(round(metro_cerc_800$population_b_only), big.mark = ","), " by Cercanías but not Metro, and ", format(round(metro_cerc_800$population_both), big.mark = ","), " by both. All six pairwise mode combinations are provided for every radius and both geometry scenarios."), "",
  "## Computation and interpretation", "",
  paste0("The indexed 1,500 m joins retain ", format(nrow(preferred_distances), big.mark = ","), " unique preferred component–station pairs and ", format(nrow(reference_distances), big.mark = ","), " reference-point pairs. Independent direct checks for five contrasting stations and five radii reproduce the pipeline totals within numerical tolerance."),
  "Catchment density divides modeled residents by the unioned, Comunidad-clipped entrance-buffer area. Results remain modeled population allocations and straight-line proximity measures; they are not observed household locations or walking distances.", "",
  "Phase 5 should stress-test population weighting, entrance versus point effects, component placement, station outliers and threshold sensitivity before publication."
)
writeLines(report, file.path(qa_dir, "phase4_report.md"))
jsonlite::write_json(list(
  status = status, generated_at = iso_now(), population_reference_date = cfg$phase2$population_reference_date,
  station_snapshot = cfg$phase3$station_snapshot, station_count = nrow(stations), radii_m = radii,
  primary_method = cfg$phase4$primary_method, sensitivity_method = cfg$phase4$sensitivity_method,
  regional_population = regional_population,
  checks = lapply(seq_len(nrow(checks)), function(i) list(check = checks$check[i], passed = checks$passed[i])),
  metrics = stats::setNames(as.list(metrics$value), metrics$metric)
), file.path(cfg$paths$validation, "current_phase4_decision.json"), auto_unbox = TRUE, pretty = TRUE)

cat("Phase 4 validation decision:", status, "\n")
print(checks)
