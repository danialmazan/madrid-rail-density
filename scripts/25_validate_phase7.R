#!/usr/bin/env Rscript
source("R/utils.R")
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(sf))

cfg <- read_config()
out <- cfg$paths$phase7_derived
site <- cfg$paths$phase7_site
qa <- file.path(cfg$paths$validation, "phase7")
ensure_dir(qa)

stations <- readRDS(file.path(cfg$paths$phase3_derived, "canonical_stations.rds"))
station_dt <- as.data.table(st_drop_geometry(stations))
platforms <- readRDS(file.path(cfg$paths$phase3_derived, "station_platforms.rds"))
platform_dt <- as.data.table(st_drop_geometry(platforms))
origins <- readRDS(file.path(cfg$paths$phase3_derived, "station_origins_phase7.rds"))
origin_dt <- as.data.table(st_drop_geometry(origins))
nearest <- readRDS(file.path(out, "component_nearest_station_phase7_2025.rds"))
curves <- fread(file.path(out, "coverage_curves_2025.csv"))
hist <- fread(file.path(out, "distance_histograms_2025.csv"))
area <- fread(file.path(out, "area_distance_metrics_2025.csv"))
station_curves <- fread(file.path(out, "station_catchment_curves_2025.csv"))
hinterlands <- fread(file.path(out, "station_hinterlands_2025.csv"))
manifest <- fread(file.path(cfg$paths$rail_phase3_raw, "source_manifest.csv"))
excluded <- fread(file.path(cfg$paths$phase3_interim, "excluded_source_records.csv"))

origin_coordinates <- st_coordinates(origins)
origin_coordinate_dt <- copy(origin_dt)
origin_coordinate_dt[, `:=`(origin_x = origin_coordinates[, 1], origin_y = origin_coordinates[, 2])]
duplicate_origin_coordinates <- origin_coordinate_dt[, .(
  origin_records = .N,
  canonical_stations = uniqueN(station_id),
  station_ids = paste(sort(unique(station_id)), collapse = "|")
), by = .(point_method, origin_x, origin_y)][origin_records > 1]
fwrite(duplicate_origin_coordinates, file.path(qa, "duplicate_origin_coordinates.csv"))

population_total <- sum(nearest$estimated_population)
city_total <- nearest[CUMUN == cfg$phase7$madrid_municipality_code, sum(estimated_population)]
district_totals <- nearest[CUMUN == cfg$phase7$madrid_municipality_code, .(population = sum(estimated_population)), by = district_code]
duplicate_platform_coordinates <- platform_dt[, .N, by = .(platform_x, platform_y)][N > 1, sum(N)]

monotonic_curves <- curves[order(radius_m), all(diff(population) >= -1e-6), by = .(geography_id, point_method, rail_network)]$V1
monotonic_station <- station_curves[order(radius_m), all(diff(population) >= -1e-6), by = .(station_id, point_method)]$V1
hist_reconcile <- hist[, .(bands = sum(population), denominator = unique(geography_population)), by = .(geography_id, point_method, rail_network)]
nested <- dcast(curves, geography_id + point_method + radius_m ~ rail_network, value.var = "population")

median_check <- merge(
  area[, .(geography_id, point_method, rail_network, median_distance_m)],
  curves[population_share >= .5, .(crossing_radius_m = min(radius_m)), by = .(geography_id, point_method, rail_network)],
  by = c("geography_id", "point_method", "rail_network")
)

weighted_median <- function(values, weights) {
  ordering <- order(values)
  values <- values[ordering]
  weights <- weights[ordering]
  values[which(cumsum(weights) >= sum(weights) / 2)[1]]
}
hinterland_recomputed <- rbindlist(lapply(c("entrance", "platform"), function(method) {
  station_column <- paste(method, "all_rail", "station_id", sep = "_")
  distance_column <- paste(method, "all_rail", "distance_m", sep = "_")
  nearest[, .(
    recomputed_population = sum(estimated_population),
    recomputed_components = .N,
    recomputed_median = weighted_median(get(distance_column), estimated_population),
    recomputed_mean = weighted.mean(get(distance_column), estimated_population)
  ), by = .(station_id = get(station_column))][, point_method := method]
}))
hinterland_check <- merge(hinterlands, hinterland_recomputed, by = c("station_id", "point_method"))
hinterland_statistics_recompute <- nrow(hinterland_check) == nrow(hinterland_recomputed) &&
  max(abs(hinterland_check$nearest_station_population - hinterland_check$recomputed_population)) < 1e-5 &&
  all(hinterland_check$component_count == hinterland_check$recomputed_components) &&
  max(abs(hinterland_check$median_distance_m - hinterland_check$recomputed_median)) < 1e-7 &&
  max(abs(hinterland_check$mean_distance_m - hinterland_check$recomputed_mean)) < 1e-7

tie_point <- st_sfc(st_point(c(0, 0)), crs = cfg$project$crs)
tie_origins <- st_sf(
  station_id = c("station_b", "station_a"),
  origin_id = c("origin_b", "origin_a"),
  geometry = st_sfc(st_point(c(-1, 0)), st_point(c(1, 0)), crs = cfg$project$crs)
)
tie_origins <- tie_origins[order(tie_origins$station_id, tie_origins$origin_id), ]
tie_candidates <- terra::nearby(terra::vect(tie_point), terra::vect(tie_origins), k = 2L)
tie_distances <- as.numeric(st_distance(tie_point, tie_origins[as.integer(tie_candidates[1, -1]), ], by_element = FALSE))
tie_ids <- tie_origins$station_id[as.integer(tie_candidates[1, -1])]
deterministic_tie_break <- tie_ids[order(tie_distances, tie_ids)][1] == "station_a"

outside_city_station_ids <- station_dt[municipality_code != cfg$phase7$madrid_municipality_code, station_id]
city_residents_using_outside_station <- nearest[CUMUN == cfg$phase7$madrid_municipality_code, sum(entrance_all_rail_station_id %in% outside_city_station_ids)]
entrance_pairs <- readRDS(file.path(cfg$paths$phase4_derived, "component_station_distances_preferred_1500m.rds"))
city_station_ids <- station_dt[municipality_code == cfg$phase7$madrid_municipality_code, station_id]
outside_residents_in_city_station_catchments <- entrance_pairs[
  station_id %in% city_station_ids & nearest$CUMUN[component_index] != cfg$phase7$madrid_municipality_code,
  .N
]

arganda_merge <- station_dt[station_name == "Puerta de Arganda–Vicálvaro"]
identity_ok <- nrow(arganda_merge) == 1L && grepl("metro", arganda_merge$modes) && grepl("cercanias", arganda_merge$modes) &&
  station_dt[station_name == "Vicalvaro", .N] == 1L &&
  station_dt[station_name %in% c("Callao", "Santo Domingo"), uniqueN(station_id)] == 2L

required_site <- file.path(site, c(
  "index.html", "data/phase7.json", "tiles/components.pmtiles", "tiles/surface.pmtiles", "tiles/stations.pmtiles",
  "downloads/station-origins-phase7.csv", "downloads/coverage-curves-2025.csv",
  "downloads/distance-histograms-2025.csv", "downloads/area-distance-metrics-2025.csv",
  "downloads/station-catchment-curves-2025.csv", "downloads/station-hinterlands-2025.csv",
  "downloads/coverage-curves-2025.parquet", "downloads/distance-histograms-2025.parquet",
  "downloads/area-distance-metrics-2025.parquet", "downloads/station-catchment-curves-2025.parquet",
  "downloads/station-hinterlands-2025.parquet"
))
tile_paths <- file.path(site, "tiles", c("components.pmtiles", "surface.pmtiles", "stations.pmtiles"))
pmtiles_report <- file.path(qa, "pmtiles_validation.json")
pmtiles_output <- system2("node", c("scripts/25b_validate_pmtiles.mjs", site, pmtiles_report), stdout = TRUE, stderr = TRUE)
pmtiles_status <- attr(pmtiles_output, "status")
pmtiles_metadata_valid <- (is.null(pmtiles_status) || pmtiles_status == 0L) && file.exists(pmtiles_report) && isTRUE(jsonlite::fromJSON(pmtiles_report)$passed)

payload <- jsonlite::fromJSON(file.path(site, "data", "phase7.json"))
html <- paste(readLines(file.path(site, "index.html"), warn = FALSE), collapse = "\n")
asset_files <- list.files(file.path(site, "assets"), full.names = TRUE)
js_files <- asset_files[grepl("\\.js$", asset_files)]
css_files <- asset_files[grepl("\\.css$", asset_files)]
js <- paste(unlist(lapply(js_files, readLines, warn = FALSE)), collapse = "\n")
css <- paste(unlist(lapply(css_files, readLines, warn = FALSE)), collapse = "\n")
i18n_source <- paste(readLines(file.path(cfg$paths$phase7_frontend, "src", "i18n.ts"), warn = FALSE), collapse = "\n")
maps_source <- paste(readLines(file.path(cfg$paths$phase7_frontend, "src", "maps.ts"), warn = FALSE), collapse = "\n")

legacy_decisions <- c(
  current_method_decision = "GO_MODIFIED_COMPONENT_ALLOCATION",
  current_phase2_decision = "GO_PHASE2_COMPLETE",
  current_phase3_decision = "GO_PHASE3_ENTRANCE_FIRST_WITH_SURFACE_FALLBACK",
  current_phase4_decision = "GO_PHASE4_COMPLETE",
  current_phase5_decision = "GO_PHASE5_PUBLICATION_READY_WITH_CAVEATS",
  current_phase6_decision = "GO_PHASE6_SITE_READY"
)
legacy_gates_green <- all(vapply(names(legacy_decisions), function(name) {
  path <- file.path(cfg$paths$validation, paste0(name, ".json"))
  file.exists(path) && identical(jsonlite::fromJSON(path)$status, unname(legacy_decisions[name]))
}, logical(1)))

browser_qa <- file.path(qa, "browser_qa.md")
hash_targets <- c(file.path(site, "index.html"), js_files, css_files, file.path(site, "data", "phase7.json"), tile_paths)
hashes <- if (all(file.exists(hash_targets))) vapply(hash_targets, sha256_file, character(1)) else character()
browser_text <- if (file.exists(browser_qa)) paste(readLines(browser_qa, warn = FALSE), collapse = "\n") else ""
browser_current <- length(hashes) == length(hash_targets) && all(vapply(hashes, grepl, logical(1), x = browser_text, fixed = TRUE)) && grepl("PASS", browser_text, fixed = TRUE)

checks <- data.table(
  check = c(
    "source_manifest_complete", "platform_ids_unique", "platform_geometries_nonempty", "platform_station_resolution_complete",
    "out_of_region_cercanias_exclusions_explicit", "source_mode_attribution_complete", "normalized_origins_complete", "entrance_fallbacks_explicit",
    "duplicated_origin_coordinates_reported",
    "population_components_unique", "regional_population_reconciles", "city_population_reconciles",
    "districts_complete_and_reconcile", "coverage_grid_complete", "coverage_curves_monotonic",
    "histograms_reconcile", "exact_median_matches_display_crossing", "nested_curves_ordered",
    "nested_final_equals_all_rail", "station_catchment_grid_complete", "station_catchments_monotonic",
    "hinterland_grid_complete", "hinterland_population_reconciles", "hinterland_statistics_recompute", "deterministic_tie_break_fixture", "station_identity_regressions",
    "resident_boundary_semantics", "station_boundary_semantics", "required_build_outputs_exist",
    "pmtiles_metadata_and_feature_counts", "map_zoom_visibility_contract", "frontend_payload_complete", "bilingual_copy_present",
    "straight_line_labels_present", "responsive_and_reduced_motion_present", "phase1_to_phase6_gates_retained", "browser_qa_matches_current_assets"
  ),
  passed = c(
    nrow(manifest) == 9L && all(c("source_date", "retrieval_time", "sha256", "row_count", "licence_url") %in% names(manifest)) && all(nzchar(manifest$sha256)) && all(manifest$row_count > 0),
    nrow(platform_dt) == uniqueN(platform_dt$platform_id), !any(st_is_empty(platforms)),
    !anyNA(platform_dt$station_id) && uniqueN(platform_dt$station_id) == 357L,
    excluded[exclusion_reason == "cercanias_outside_comunidad_de_madrid", .N] == 19L,
    !anyNA(platform_dt$source_mode) && all(platform_dt$source_mode %in% c("metro", "cercanias", "metro_ligero", "tranvia_parla")),
    all(c("entrance", "platform") %in% unique(origin_dt$point_method)) && all(origin_dt[, uniqueN(station_id), by = point_method]$V1 == 357L),
    origin_dt[point_method == "entrance" & is_fallback == TRUE, .N] == 40L && all(origin_dt[point_method == "entrance" & is_fallback == TRUE, origin_type] == "official_station_point_fallback"),
    file.exists(file.path(qa, "duplicate_origin_coordinates.csv")),
    nrow(nearest) == 644613L && uniqueN(nearest$component_key) == nrow(nearest),
    abs(population_total - 7113886) < 1e-5, abs(city_total - 3477497) < 1e-5,
    nrow(district_totals) == 21L && all(sort(district_totals$district_code) == sprintf("%02d", 1:21)) && abs(sum(district_totals$population) - city_total) < 1e-5,
    nrow(curves) == 23L * 2L * 5L * 61L && uniqueN(curves, by = c("geography_id", "point_method", "rail_network", "radius_m")) == nrow(curves),
    all(monotonic_curves), max(abs(hist_reconcile$bands - hist_reconcile$denominator)) < 1e-5,
    nrow(area) == 23L * 2L * 5L && nrow(median_check) == nrow(area[median_distance_m <= 1500]) && all(median_check$crossing_radius_m == ceiling(median_check$median_distance_m / 25) * 25),
    all(nested$metro <= nested$metro_cercanias + 1e-6 & nested$metro_cercanias <= nested$all_rail + 1e-6),
    all(curves[rail_network == "all_rail", population] == nested$all_rail),
    nrow(station_curves) == 357L * 2L * 61L && uniqueN(station_curves, by = c("station_id", "point_method", "radius_m")) == nrow(station_curves),
    all(monotonic_station), nrow(hinterlands) == 357L * 2L && uniqueN(hinterlands, by = c("station_id", "point_method")) == nrow(hinterlands),
    all(abs(hinterlands[, .(assigned = sum(nearest_station_population)), by = point_method]$assigned - population_total) < 1e-5),
    hinterland_statistics_recompute,
    deterministic_tie_break,
    identity_ok, city_residents_using_outside_station > 0, outside_residents_in_city_station_catchments > 0,
    all(file.exists(required_site)) && all(file.info(required_site)$size > 0),
    pmtiles_metadata_valid,
    grepl("maxzoom:13", maps_source, fixed = TRUE) && grepl("minzoom:12", maps_source, fixed = TRUE) && grepl("connection-lines", maps_source, fixed = TRUE) && grepl("origin-points", maps_source, fixed = TRUE),
    nrow(payload$stations) == 357L && nrow(payload$coverage_curves) == nrow(curves) && nrow(payload$hinterlands) == 714L,
    grepl("data-lang=", html, fixed = TRUE) && grepl("An estimate with explicit boundaries", i18n_source, fixed = TRUE) && grepl("Una estimación con límites explícitos", i18n_source, fixed = TRUE),
    grepl("Straight-line distance", i18n_source, fixed = TRUE) && grepl("Distancia en línea recta", i18n_source, fixed = TRUE),
    grepl("@media", css, fixed = TRUE) && grepl("prefers-reduced-motion", css, fixed = TRUE), legacy_gates_green, browser_current
  )
)

fwrite(checks, file.path(qa, "phase7_gate_checks.csv"))
status <- if (all(checks$passed)) "GO_PHASE7_LOCAL_STATIC_BUILD" else "PHASE7_REVIEW_REQUIRED"
metrics <- data.table(
  metric = c("population_components", "regional_population", "city_population", "districts", "canonical_stations", "official_entrances", "in_scope_platforms", "platform_duplicate_coordinate_records", "entrance_fallback_stations", "coverage_rows", "histogram_rows", "station_curve_rows", "hinterland_rows", "city_residents_assigned_outside_station", "outside_residents_in_city_station_catchments"),
  value = c(nrow(nearest), population_total, city_total, nrow(district_totals), nrow(station_dt), sum(station_dt$official_entrance_count), nrow(platform_dt), duplicate_platform_coordinates, sum(!station_dt$has_official_entrance), nrow(curves), nrow(hist), nrow(station_curves), nrow(hinterlands), city_residents_using_outside_station, outside_residents_in_city_station_catchments)
)
fwrite(metrics, file.path(qa, "phase7_metrics.csv"))
writeLines(c(
  "# Phase 7 — local static explorer", "", paste0("**Decision: ", status, "**"), "",
  "Phase 7 preserves the audited 2025 population allocation and canonical 357-station model while adding entrance/platform origins, exact resident distances, districts, station catchments, deterministic hinterlands, PMTiles, and a typed bilingual frontend.", "",
  "## Boundary semantics", "",
  paste0("- ", format(city_residents_using_outside_station, big.mark = ","), " Madrid-city components select a station outside the municipality under the entrance method."),
  paste0("- ", format(outside_residents_in_city_station_catchments, big.mark = ","), " component–station pairs show that city-station catchments retain residents outside the municipality."), "",
  "## Tile contract", "",
  "- Generalized surface: zooms 5–12.", "- Simplified residential components: zooms 12–16.", "- Central stations: zooms 5–16; origins and constellation lines become visible from zoom 12.", "",
  "Deployment, repository creation, commit and publication remain outside Phase 7 scope."
), file.path(qa, "phase7_report.md"))
jsonlite::write_json(list(status = status, generated_at = iso_now(), checks = lapply(seq_len(nrow(checks)), function(i) list(check = checks$check[i], passed = checks$passed[i])), metrics = stats::setNames(as.list(metrics$value), metrics$metric), browser_qa = if (browser_current) "passed" else "pending"), file.path(cfg$paths$validation, "current_phase7_decision.json"), auto_unbox = TRUE, pretty = TRUE)
cat("Phase 7 validation decision:", status, "\n")
print(checks)
if (!all(checks$passed)) stop("Phase 7 validation gates failed: ", paste(checks[passed == FALSE, check], collapse = ", "))
