#!/usr/bin/env Rscript
source("R/utils.R")
source("R/catastro_io.R")
source("R/dwelling_rules.R")
source("R/spatial_allocation.R")
source("R/station_helpers.R")

assert <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)

line14 <- paste(rep(" ", 1000), collapse = "")
substr(line14, 1, 2) <- "14"
substr(line14, 31, 44) <- "1234567VK4713A"
substr(line14, 45, 48) <- "0001"
substr(line14, 51, 54) <- "0007"
substr(line14, 55, 58) <- "0002"
substr(line14, 65, 67) <- "03 "
substr(line14, 68, 70) <- "A  "
substr(line14, 71, 73) <- "V  "
substr(line14, 84, 90) <- "0000085"
substr(line14, 105, 109) <- "01115"
x14 <- clean_cat_table(slice_fields(line14, cat_schemas()[["14"]]), "14")
assert(x14$parcel_ref == "1234567VK4713A", "CAT parcel slicing failed")
assert(x14$area_m2 == 85, "CAT area slicing failed")
assert(x14$typology == "01115", "CAT typology slicing failed")

line15 <- paste(rep(" ", 1000), collapse = "")
substr(line15, 1, 2) <- "15"
substr(line15, 31, 44) <- "1234567VK4713A"
substr(line15, 45, 48) <- "0001"
substr(line15, 368, 371) <- "0001"
substr(line15, 428, 428) <- "V"
substr(line15, 442, 451) <- "0000000085"
substr(line15, 462, 470) <- "050000000"
x15 <- clean_cat_table(slice_fields(line15, cat_schemas()[["15"]]), "15")
assert(abs(x15$ownership_coefficient - 50) < 1e-9, "CAT ownership coefficient scaling failed")

cfg <- read_config()
qualified <- qualify_residential_construction(x14, cfg)
assert(qualified$is_residential, "Residential qualification failed")
assert(qualified$housing_form == "collective", "Collective typology classification failed")

poly1 <- sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)))
poly2 <- sf::st_polygon(list(matrix(c(2,0, 3,0, 3,1, 2,1, 2,0), ncol = 2, byrow = TRUE)))
building <- sf::st_sf(municipality_code = "test", parcel_ref = "p1", current_use = "1_residential", number_building_units = 10, number_dwellings = 10, official_area_m2 = 1000, patch_count = 2L, geometry = sf::st_sfc(sf::st_multipolygon(list(unclass(poly1), unclass(poly2))), crs = 25830))
components <- explode_building_components(building)
assert(nrow(components) == 2, "Multipart explosion failed")
assert(abs(sum(components$allocated_dwellings_footprint) - 10) < 1e-9, "Component allocation does not preserve dwellings")
assert(all(abs(components$footprint_share - 0.5) < 1e-9), "Footprint shares are wrong")

# Regression guards for the Phase 1 disconnected-footprint diagnostic: casting
# the bare MULTIPOLYGON sfg retains only its first polygon, while unioning one
# multipart feature without first casting its patches does not dissolve them.
diagnostic_parts <- connected_polygon_parts(sf::st_geometry(building)[[1]], 25830)
assert(length(diagnostic_parts) == 2, "Disconnected-component diagnostic dropped polygon parts")

touching_poly <- sf::st_polygon(list(matrix(c(1,0, 2,0, 2,1, 1,1, 1,0), ncol = 2, byrow = TRUE)))
touching_multi <- sf::st_multipolygon(list(unclass(poly1), unclass(touching_poly)))
touching_parts <- connected_polygon_parts(touching_multi, 25830)
assert(length(touching_parts) == 1, "Touching polygon patches were not dissolved into one component")

allocation_input <- data.table::data.table(
  parcel_ref = rep("p", 3), component_id = 1:3,
  footprint_m2 = c(100, 50, 10), volume_proxy = c(300, 100, 3),
  parcel_dwellings = 40
)
allocation <- allocate_component_methods(allocation_input)
for (nm in c("dwellings_volume", "dwellings_volume_all", "dwellings_footprint", "dwellings_equal", "dwellings_largest")) {
  assert(abs(sum(allocation[[nm]]) - 40) < 1e-9, paste(nm, "does not conserve parcel dwellings"))
}
assert(allocation[component_id == 3, minor_ancillary], "Minor ancillary component was not flagged")
assert(allocation[component_id == 3, dwellings_volume] == 0, "Baseline volume allocation did not exclude minor ancillary component")

cat15 <- data.table::data.table(
  parcel_ref = c("horizontal", "horizontal", "vertical", "single"),
  cargo = c("1", "2", "1", "1"),
  division_order = c("1", "2", NA, NA),
  ownership_coefficient = c(50, 50, NA, NA)
)
cat14 <- data.table::data.table(
  parcel_ref = c("horizontal", "horizontal", "vertical", "vertical", "vertical", "single", "single"),
  cargo = c("1", "2", "1", "1", "1", "1", "1"),
  block = NA_character_,
  stair = NA_character_,
  floor = c("1", "2", "1", "1", "2", "1", "2"),
  door = c("A", "A", "A", "B", "A", NA, NA),
  destination = "V",
  area_m2 = c(80, 90, 70, 75, 80, 60, 65),
  typology = c("01115", "01115", "01115", "01115", "01115", "01215", "01215")
)
derived <- derive_residential_parcels(cat14, cat15, cfg)
assert(derived[parcel_ref == "horizontal", reconstructed_dwellings] == 2, "Collective property-unit count failed")
assert(derived[parcel_ref == "vertical", reconstructed_dwellings] == 3, "Vertical-property reconstruction failed")
assert(derived[parcel_ref == "single", reconstructed_dwellings] == 1, "Single-family aggregation failed")
assert(derived[parcel_ref == "vertical", dwelling_rule] == "vertical_collective_location_reconstruction", "Vertical-property rule was bypassed by a coefficient field")

phase1b_points <- file.path(cfg$paths$interim, "phase1b_component_points.rds")
if (file.exists(phase1b_points)) {
  p <- readRDS(phase1b_points)
  methods <- grep("^dwellings_", names(p), value = TRUE)
  conserved <- p[, c(list(target = unique(parcel_dwellings)), lapply(.SD, sum)), by = parcel_ref, .SDcols = methods]
  for (nm in methods) assert(all(abs(conserved[[nm]] - conserved$target) < 1e-8), paste("Phase 1b method does not conserve dwellings:", nm))
  assert(!anyNA(p$x) && !anyNA(p$y), "Phase 1b component points contain missing coordinates")
}

phase1b_status <- file.path(cfg$paths$validation, "phase1b", "phase1b_status.json")
if (file.exists(phase1b_status)) {
  s <- jsonlite::fromJSON(phase1b_status)
  m <- data.table::fread(file.path(cfg$paths$validation, "phase1b", "sensitivity_metrics.csv"))
  gate <- m[method %in% s$decision_methods]
  assert((s$status == "GO_MODIFIED_COMPONENT_ALLOCATION") == all(gate$share_within_tolerance >= s$required_cell_share), "Phase 1b status contradicts sensitivity metrics")
}

phase2_population <- file.path(cfg$paths$phase2_derived, "component_population_2025.rds")
if (file.exists(phase2_population)) {
  p2 <- readRDS(phase2_population)
  d2 <- data.table::as.data.table(sf::st_drop_geometry(p2))
  r2 <- d2[, .(control = unique(section_population), allocated = sum(estimated_population)), by = CUSEC]
  assert(max(abs(r2$allocated - r2$control)) < 1e-6, "Phase 2 section population controls are not conserved")
  assert(!anyNA(d2$estimated_population) && all(d2$estimated_population >= 0), "Phase 2 contains invalid population estimates")
  assert(all(c("population_reference_date", "population_method_version", "section_support_quality") %in% names(d2)), "Phase 2 provenance or QA fields are missing")
  assert(all(!is.na(d2$weight_source)), "Phase 2 has unflagged weight sources")
}

phase2_status <- file.path(cfg$paths$validation, "current_phase2_decision.json")
if (file.exists(phase2_status)) {
  s2 <- jsonlite::fromJSON(phase2_status)
  g2 <- data.table::fread(file.path(cfg$paths$validation, "phase2", "phase2_gate_checks.csv"))
  assert((s2$status == "GO_PHASE2_COMPLETE") == all(g2$passed), "Phase 2 decision contradicts its gate checks")
}

phase3_stations <- file.path(cfg$paths$phase3_derived, "canonical_stations.rds")
if (file.exists(phase3_stations)) {
  s3 <- readRDS(phase3_stations)
  e3 <- readRDS(file.path(cfg$paths$phase3_derived, "station_entrances.rds"))
  o3 <- readRDS(file.path(cfg$paths$phase3_derived, "station_distance_origins.rds"))
  d3 <- data.table::as.data.table(sf::st_drop_geometry(s3))
  e3_dt <- data.table::as.data.table(sf::st_drop_geometry(e3))
  o3_dt <- data.table::as.data.table(sf::st_drop_geometry(o3))
  v3 <- data.table::fread(file.path(cfg$paths$phase3_derived, "station_services.csv"))
  m3 <- data.table::fread(file.path(cfg$paths$phase3_interim, "station_source_records.csv"))
  assert(inherits(s3, "sf") && sf::st_crs(s3)$epsg == cfg$project$crs, "Phase 3 canonical station geometry or CRS is invalid")
  assert(nrow(d3) == 357L && data.table::uniqueN(d3$station_id) == 357L, "Phase 3 station count or IDs changed")
  assert(nrow(m3) == 440L && nrow(m3) == data.table::uniqueN(m3, by = c("source_code", "source_station_id")), "Phase 3 source mapping is incomplete or duplicated")
  assert(inherits(e3, "sf") && nrow(e3) == 1020L && data.table::uniqueN(e3_dt$entrance_id) == 1020L, "Phase 3 official entrance layer is incomplete or duplicated")
  assert(inherits(o3, "sf") && nrow(o3) == 1060L && data.table::uniqueN(o3_dt$station_id) == 357L, "Phase 3 preferred distance origins are invalid")
  assert(sum(d3$has_official_entrance) == 317L && sum(!d3$has_official_entrance) == 40L, "Phase 3 entrance/fallback station split changed")
  assert(all(o3_dt[is_fallback == TRUE, origin_type] == "official_station_point_fallback"), "Phase 3 fallback origins are not explicitly identified")
  expected_modes <- data.table::data.table(mode = c("metro", "metro_ligero", "cercanias", "tranvia_parla"), stations = c(241L, 37L, 92L, 19L))
  actual_modes <- v3[, .(stations = data.table::uniqueN(station_id)), by = mode]
  actual_modes <- merge(expected_modes, actual_modes, by = "mode", suffixes = c("_expected", "_actual"))
  assert(nrow(actual_modes) == 4L && all(actual_modes$stations_expected == actual_modes$stations_actual), "Phase 3 mode counts changed")
  assert(d3[station_name %in% c("Callao", "Santo Domingo"), data.table::uniqueN(station_id)] == 2L, "Callao and Santo Domingo were incorrectly merged")
}

phase3_status <- file.path(cfg$paths$validation, "current_phase3_decision.json")
if (file.exists(phase3_status)) {
  s3_status <- jsonlite::fromJSON(phase3_status)
  g3 <- data.table::fread(file.path(cfg$paths$validation, "phase3", "phase3_gate_checks.csv"))
  assert((s3_status$status == "GO_PHASE3_ENTRANCE_FIRST_WITH_SURFACE_FALLBACK") == all(g3$passed), "Phase 3 decision contradicts its gate checks")
}

phase4_catchments <- file.path(cfg$paths$phase4_derived, "station_catchments_2025.rds")
if (file.exists(phase4_catchments)) {
  c4 <- data.table::as.data.table(readRDS(phase4_catchments))
  r4 <- data.table::fread(file.path(cfg$paths$phase4_derived, "regional_network_coverage_2025.csv"))
  o4 <- data.table::fread(file.path(cfg$paths$phase4_derived, "network_overlap_2025.csv"))
  n4 <- readRDS(file.path(cfg$paths$phase4_derived, "component_nearest_station_2025.rds"))
  assert(nrow(c4) == 357L * 15L * 2L, "Phase 4 station catchment grid is incomplete")
  assert(data.table::uniqueN(c4, by = c("scenario", "station_id", "radius_m")) == nrow(c4), "Phase 4 station catchment keys are duplicated")
  assert(all(c4[order(radius_m), all(diff(population) >= -1e-7), by = .(scenario, station_id)]$V1), "Phase 4 station catchments are not cumulative")
  assert(nrow(r4) == 15L * 5L * 2L && all(r4$population_share >= 0 & r4$population_share <= 1), "Phase 4 regional coverage is invalid")
  assert(nrow(n4) == 644613L && data.table::uniqueN(n4$component_key) == nrow(n4), "Phase 4 nearest-station table is incomplete")
  assert(max(abs(o4$population_a_only + o4$population_b_only + o4$population_both + o4$population_neither - o4$regional_population)) < 1e-5, "Phase 4 overlap partitions do not reconcile")
}

phase4_status <- file.path(cfg$paths$validation, "current_phase4_decision.json")
if (file.exists(phase4_status)) {
  s4 <- jsonlite::fromJSON(phase4_status)
  g4 <- data.table::fread(file.path(cfg$paths$validation, "phase4", "phase4_gate_checks.csv"))
  assert((s4$status == "GO_PHASE4_COMPLETE") == all(g4$passed), "Phase 4 decision contradicts its gate checks")
}

phase5_station <- file.path(cfg$paths$phase5_derived, "station_population_method_sensitivity_2025.rds")
if (file.exists(phase5_station)) {
  s5_data <- data.table::as.data.table(readRDS(phase5_station))
  r5_data <- data.table::fread(file.path(cfg$paths$phase5_derived, "regional_population_method_sensitivity_2025.csv"))
  c5_data <- data.table::fread(file.path(cfg$paths$phase5_derived, "population_variant_conservation.csv"))
  t5_data <- data.table::fread(file.path(cfg$paths$phase5_derived, "station_threshold_band_comparison_2025.csv"))
  variants5 <- as.character(unlist(cfg$phase5$population_variants))
  assert(nrow(s5_data) == 357L * 15L * length(variants5), "Phase 5 station sensitivity grid is incomplete")
  assert(nrow(r5_data) == length(variants5) * 5L * 15L, "Phase 5 regional sensitivity grid is incomplete")
  assert(max(abs(c5_data$regional_population - 7113886)) < 1e-6, "Phase 5 population variants do not conserve the regional total")
  assert(nrow(t5_data) == 357L * length(unlist(cfg$phase5$selected_radii_m)), "Phase 5 threshold sensitivity grid is incomplete")
  assert(all(t5_data$population_in_100m_band >= -1e-7), "Phase 5 threshold bands contain negative population")
}

phase5_status <- file.path(cfg$paths$validation, "current_phase5_decision.json")
if (file.exists(phase5_status)) {
  s5 <- jsonlite::fromJSON(phase5_status)
  g5 <- data.table::fread(file.path(cfg$paths$validation, "phase5", "phase5_gate_checks.csv"))
  ledger5 <- data.table::fread(file.path(cfg$paths$validation, "phase5", "methodology_ledger.csv"))
  assert((s5$status == "GO_PHASE5_PUBLICATION_READY_WITH_CAVEATS") == all(g5$passed), "Phase 5 decision contradicts its gate checks")
  assert(nrow(ledger5) >= 10L && all(nzchar(ledger5$publication_requirement)), "Phase 5 methodology ledger is incomplete")
}

phase6_status <- file.path(cfg$paths$validation, "current_phase6_decision.json")
if (file.exists(phase6_status)) {
  s6 <- jsonlite::fromJSON(phase6_status)
  g6 <- data.table::fread(file.path(cfg$paths$validation, "phase6", "phase6_gate_checks.csv"))
  web6 <- jsonlite::fromJSON(file.path(cfg$paths$phase6_derived, "phase6_data.json"))
  assert((s6$status == "GO_PHASE6_SITE_READY") == all(g6$passed), "Phase 6 decision contradicts its gate checks")
  assert(nrow(web6$stations) == 357L && nrow(web6$station_curves) == 357L * 15L, "Phase 6 web payload is incomplete")
  assert(sum(web6$stations$fallback) == 40L, "Phase 6 web payload lost surface-stop fallback flags")
}

phase7_path <- file.path(cfg$paths$phase7_derived, "coverage_curves_2025.csv")
if (file.exists(phase7_path)) {
  p7 <- readRDS(file.path(cfg$paths$phase3_derived, "station_platforms.rds"))
  o7 <- readRDS(file.path(cfg$paths$phase3_derived, "station_origins_phase7.rds"))
  c7 <- data.table::fread(phase7_path)
  h7 <- data.table::fread(file.path(cfg$paths$phase7_derived, "distance_histograms_2025.csv"))
  s7 <- data.table::fread(file.path(cfg$paths$phase7_derived, "station_catchment_curves_2025.csv"))
  z7 <- data.table::fread(file.path(cfg$paths$phase7_derived, "station_hinterlands_2025.csv"))
  p7_dt <- data.table::as.data.table(sf::st_drop_geometry(p7))
  o7_dt <- data.table::as.data.table(sf::st_drop_geometry(o7))
  assert(nrow(p7_dt) == 955L && data.table::uniqueN(p7_dt$platform_id) == 955L, "Phase 7 in-scope platform contract changed")
  assert(all(p7_dt$source_mode %in% c("metro", "cercanias", "metro_ligero", "tranvia_parla")), "Phase 7 platform mode attribution is incomplete")
  assert(all(o7_dt[, data.table::uniqueN(station_id), by = point_method]$V1 == 357L), "Phase 7 origins do not cover every canonical station")
  assert(nrow(c7) == 23L * 2L * 5L * 61L, "Phase 7 coverage grid is incomplete")
  assert(all(c7[order(radius_m), all(diff(population) >= -1e-6), by = .(geography_id, point_method, rail_network)]$V1), "Phase 7 curves are not cumulative")
  h7_sum <- h7[, .(population = sum(population), denominator = unique(geography_population)), by = .(geography_id, point_method, rail_network)]
  assert(max(abs(h7_sum$population - h7_sum$denominator)) < 1e-5, "Phase 7 histogram bands do not reconcile")
  assert(nrow(s7) == 357L * 2L * 61L, "Phase 7 station catchment grid is incomplete")
  assert(nrow(z7) == 357L * 2L, "Phase 7 hinterland grid is incomplete")
  assert(all(abs(z7[, .(population = sum(nearest_station_population)), by = point_method]$population - 7113886) < 1e-5), "Phase 7 hinterlands do not conserve population")
  assert(all(file.exists(file.path(cfg$paths$phase7_frontend, "public", "tiles", c("components.pmtiles", "surface.pmtiles", "stations.pmtiles")))), "Phase 7 PMTiles are missing")
}

phase7_status <- file.path(cfg$paths$validation, "current_phase7_decision.json")
if (file.exists(phase7_status)) {
  s7_status <- jsonlite::fromJSON(phase7_status)
  g7 <- data.table::fread(file.path(cfg$paths$validation, "phase7", "phase7_gate_checks.csv"))
  assert((s7_status$status == "GO_PHASE7_LOCAL_STATIC_BUILD") == all(g7$passed), "Phase 7 decision contradicts its gate checks")
}

cat("All tests passed.\n")
