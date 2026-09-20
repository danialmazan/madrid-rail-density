#!/usr/bin/env Rscript
source("R/utils.R")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))

cfg <- read_config()
qa_dir <- file.path(cfg$paths$validation, "phase3")
ensure_dir(qa_dir)

stations <- readRDS(file.path(cfg$paths$phase3_derived, "canonical_stations.rds"))
entrances <- readRDS(file.path(cfg$paths$phase3_derived, "station_entrances.rds"))
origins <- readRDS(file.path(cfg$paths$phase3_derived, "station_distance_origins.rds"))
station_dt <- as.data.table(st_drop_geometry(stations))
entrance_dt <- as.data.table(st_drop_geometry(entrances))
origin_dt <- as.data.table(st_drop_geometry(origins))
services <- fread(file.path(cfg$paths$phase3_derived, "station_services.csv"))
source_records <- fread(file.path(cfg$paths$phase3_interim, "station_source_records.csv"))
excluded <- fread(file.path(cfg$paths$phase3_interim, "excluded_source_records.csv"))
excluded_entrances <- fread(file.path(cfg$paths$phase3_interim, "excluded_entrance_records.csv"))
sections <- readRDS(file.path(cfg$paths$phase2_interim, "ine_sections_2025.rds"))

mode_counts <- services[, .(stations = uniqueN(station_id)), by = mode]
mode_expected <- data.table(
  mode = c("metro", "metro_ligero", "cercanias", "tranvia_parla"),
  expected = c(241L, 37L, 92L, 19L)
)
mode_counts <- merge(mode_expected, mode_counts, by = "mode", all.x = TRUE)
mode_counts[is.na(stations), stations := 0L]
fwrite(mode_counts, file.path(qa_dir, "mode_station_counts.csv"))

line_counts <- services[, .(stations = uniqueN(station_id)), by = .(mode, line_id, line_name)][order(mode, line_id)]
fwrite(line_counts, file.path(qa_dir, "line_station_counts.csv"))

mode_source <- data.table(
  mode = c("metro", "metro_ligero", "cercanias", "tranvia_parla"),
  source_code = c("M4", "M10", "M5", "M10")
)
station_modes <- unique(services[, .(station_id, mode)])
station_modes <- merge(station_modes, mode_source, by = "mode")
source_access <- unique(entrance_dt[, .(station_id, source_code)])
station_modes <- merge(
  station_modes,
  source_access[, .(station_id, source_code, has_own_source_entrance = TRUE)],
  by = c("station_id", "source_code"), all.x = TRUE
)
station_modes[is.na(has_own_source_entrance), has_own_source_entrance := FALSE]
station_modes <- merge(
  station_modes,
  station_dt[, .(station_id, has_any_station_entrance = has_official_entrance)],
  by = "station_id"
)
entrance_coverage <- station_modes[, .(
  stations = .N,
  stations_with_own_source_entrance = sum(has_own_source_entrance),
  own_source_coverage = mean(has_own_source_entrance),
  stations_with_any_station_entrance = sum(has_any_station_entrance),
  any_station_entrance_coverage = mean(has_any_station_entrance)
), by = mode][order(match(mode, c("metro", "metro_ligero", "cercanias", "tranvia_parla")))]
fwrite(entrance_coverage, file.path(qa_dir, "entrance_coverage_by_mode.csv"))

m10_underground <- unique(source_records[source_code == "M10" & source_situation == "I", station_id])
fallback_stations <- station_dt[has_official_entrance == FALSE, station_id]
fallback_surface_check <- source_records[station_id %in% fallback_stations, .(
  only_surface_m10 = all(source_code == "M10" & source_situation == "S")
), by = station_id]
fallback_details <- merge(
  station_dt[station_id %in% fallback_stations, .(
    station_id, station_name, modes, lines, municipality_name,
    reference_source_code, reference_source_station_id, preferred_origin_type
  )],
  fallback_surface_check,
  by = "station_id", all.x = TRUE
)[order(modes, station_name)]
fwrite(fallback_details, file.path(qa_dir, "surface_stop_fallbacks.csv"))
fwrite(
  entrance_dt[, .(
    entrance_id, station_id, source_code, entrance_name,
    distance_to_station_reference_m
  )][order(-distance_to_station_reference_m)],
  file.path(qa_dir, "entrance_reference_distance_diagnostics.csv")
)
reference_match <- merge(
  station_dt[, .(station_id, reference_source_code, reference_source_station_id, reference_x, reference_y)],
  source_records[, .(source_code, source_station_id, source_x, source_y)],
  by.x = c("reference_source_code", "reference_source_station_id"),
  by.y = c("source_code", "source_station_id"), all.x = TRUE
)
reference_match[, coordinate_difference_m := sqrt((reference_x - source_x)^2 + (reference_y - source_y)^2)]

dispersion <- station_dt[, .(
  station_id, station_name, source_names, modes, source_record_count,
  source_point_max_separation_m,
  review = source_point_max_separation_m > cfg$phase3$dispersion_review_threshold_m
)][order(-source_point_max_separation_m)]
fwrite(dispersion, file.path(qa_dir, "station_source_dispersion.csv"))

region <- st_union(st_geometry(sections))
inside_region <- lengths(st_intersects(stations, region)) > 0L
entrances_inside_region <- lengths(st_intersects(entrances, region)) > 0L

known <- list(
  Sol = c("metro", "cercanias"),
  `Nuevos Ministerios` = c("metro", "cercanias"),
  Atocha = c("metro", "cercanias"),
  `Madrid Chamartín–Clara Campoamor` = c("metro", "cercanias")
)
known_ok <- vapply(names(known), function(name) {
  hit <- station_dt[station_name == name]
  if (nrow(hit) != 1L) return(FALSE)
  identical(sort(strsplit(hit$modes, "|", fixed = TRUE)[[1]]), sort(known[[name]]))
}, logical(1))

expected_modes <- mode_expected$mode
expected_lines <- c(paste0("M", 1:12), "MR", paste0("ML", 1:3), "C1", "C2", "C3", "C4", "C5", "C7", "C8A", "C8B", "C9", "C10", "TP")
checks <- data.table(
  check = c(
    "canonical_station_count_357",
    "source_records_in_scope_440",
    "excluded_records_21",
    "excluded_cercanias_outside_region_19",
    "excluded_missing_name_2",
    "station_ids_unique",
    "station_names_unique",
    "all_stations_have_services",
    "source_records_map_once",
    "mode_station_counts_match_contract",
    "multimode_station_count_32",
    "all_reference_points_inside_comunidad",
    "all_municipalities_resolved",
    "source_dates_consistent",
    "service_taxonomy_valid",
    "known_interchanges_merged_once",
    "callao_and_santo_domingo_separate",
    "maximum_source_point_separation_within_gate",
    "official_entrance_ids_unique",
    "official_entrances_in_scope_1020",
    "excluded_entrances_are_out_of_region_cercanias",
    "all_entrances_link_to_one_canonical_station",
    "all_entrances_inside_comunidad",
    "metro_entrance_coverage_complete",
    "cercanias_entrance_coverage_complete",
    "all_underground_metro_ligero_complexes_have_entrances",
    "all_station_point_fallbacks_are_surface_stops",
    "every_station_has_preferred_distance_origin",
    "reference_points_are_actual_official_source_points",
    "maximum_entrance_to_reference_distance_within_gate"
  ),
  passed = c(
    nrow(stations) == 357L,
    nrow(source_records) == 440L,
    nrow(excluded) == 21L,
    excluded[exclusion_reason == "cercanias_outside_comunidad_de_madrid", .N] == 19L,
    excluded[exclusion_reason == "missing_station_name", .N] == 2L,
    uniqueN(station_dt$station_id) == nrow(station_dt),
    uniqueN(station_dt$station_name) == nrow(station_dt),
    all(station_dt$station_id %in% services$station_id),
    nrow(source_records) == uniqueN(source_records, by = c("source_code", "source_station_id")),
    all(mode_counts$stations == mode_counts$expected),
    station_dt[mode_count > 1L, .N] == 32L,
    all(inside_region),
    all(!is.na(station_dt$municipality_name) & nzchar(station_dt$municipality_name)),
    uniqueN(source_records$source_data_date) == 1L,
    all(services$mode %in% expected_modes) && all(services$line_id %in% expected_lines),
    all(known_ok),
    station_dt[station_name %in% c("Callao", "Santo Domingo"), uniqueN(station_id)] == 2L,
    max(station_dt$source_point_max_separation_m) <= cfg$phase3$maximum_source_point_separation_m,
    uniqueN(entrance_dt$entrance_id) == nrow(entrance_dt),
    nrow(entrance_dt) == 1020L,
    nrow(excluded_entrances) == 15L && all(excluded_entrances$source_code == "M5") && all(excluded_entrances$province_code != 28L),
    all(entrance_dt$station_id %in% station_dt$station_id),
    all(entrances_inside_region),
    entrance_coverage[mode == "metro", own_source_coverage] == 1,
    entrance_coverage[mode == "cercanias", own_source_coverage] == 1,
    all(m10_underground %in% entrance_dt[source_code == "M10", station_id]),
    nrow(fallback_surface_check) == length(fallback_stations) && all(fallback_surface_check$only_surface_m10),
    uniqueN(origin_dt$station_id) == nrow(station_dt) && all(station_dt$station_id %in% origin_dt$station_id),
    all(reference_match$coordinate_difference_m < 1e-8),
    max(entrance_dt$distance_to_station_reference_m) <= cfg$phase3$maximum_entrance_to_station_point_m
  )
)
fwrite(checks, file.path(qa_dir, "phase3_gate_checks.csv"))

metrics <- data.table(
  metric = c(
    "raw_source_records", "in_scope_source_records", "excluded_source_records",
    "canonical_physical_stations", "multimode_stations", "multi_line_or_mode_interchanges",
    "stations_over_dispersion_review_threshold", "maximum_source_point_separation_m",
    "source_data_date_count", "municipalities", "service_rows",
    "official_entrances", "stations_with_official_entrances", "station_point_fallbacks",
    "preferred_distance_origins", "maximum_entrance_to_reference_distance_m",
    "metro_ligero_own_source_entrance_coverage", "tranvia_parla_own_source_entrance_coverage"
  ),
  value = c(
    nrow(source_records) + nrow(excluded), nrow(source_records), nrow(excluded), nrow(stations),
    station_dt[mode_count > 1L, .N], station_dt[is_interchange == TRUE, .N],
    station_dt[source_point_max_separation_m > cfg$phase3$dispersion_review_threshold_m, .N],
    max(station_dt$source_point_max_separation_m), uniqueN(source_records$source_data_date),
    uniqueN(station_dt$municipality_code), nrow(services),
    nrow(entrances), sum(station_dt$has_official_entrance), sum(!station_dt$has_official_entrance),
    nrow(origins), max(entrance_dt$distance_to_station_reference_m),
    entrance_coverage[mode == "metro_ligero", own_source_coverage],
    entrance_coverage[mode == "tranvia_parla", own_source_coverage]
  )
)
fwrite(metrics, file.path(qa_dir, "phase3_metrics.csv"))
fwrite(station_dt, file.path(qa_dir, "canonical_stations.csv"))

map_data <- copy(station_dt)
map_data[, primary_mode := tstrsplit(modes, "|", fixed = TRUE, keep = 1L)]
map_sf <- stations
map_sf$primary_mode <- map_data$primary_mode
p <- ggplot() +
  geom_sf(data = st_as_sf(st_union(sections)), fill = "#f4f0e8", color = "#b9b2a5", linewidth = 0.3) +
  geom_sf(data = map_sf, aes(color = primary_mode), size = 1.2, alpha = 0.85) +
  scale_color_manual(values = c(metro = "#e34a33", metro_ligero = "#6a51a3", cercanias = "#2171b5", tranvia_parla = "#238b45")) +
  coord_sf(datum = NA) +
  labs(title = "Canonical urban-rail stations in Comunidad de Madrid", subtitle = "One point per connected physical station/interchange; CRTM snapshot 2026-09-15", color = NULL) +
  theme_void(base_size = 11) + theme(legend.position = "bottom", plot.background = element_rect(fill = "#fbf8f1", color = NA), panel.background = element_rect(fill = "#fbf8f1", color = NA))
ggsave(file.path(qa_dir, "canonical_station_map.png"), p, width = 8.5, height = 7.5, dpi = 180, bg = "#fbf8f1")

coverage_map <- stations
coverage_map$origin_coverage <- ifelse(coverage_map$has_official_entrance, "Official entrance(s)", "Official surface-stop point fallback")
p_coverage <- ggplot() +
  geom_sf(data = st_as_sf(region), fill = "#f4f0e8", color = "#b9b2a5", linewidth = 0.3) +
  geom_sf(data = entrances, color = "#276b83", size = 0.28, alpha = 0.45) +
  geom_sf(data = coverage_map, aes(color = origin_coverage), size = 1.15) +
  scale_color_manual(values = c("Official entrance(s)" = "#183229", "Official surface-stop point fallback" = "#ef5b49")) +
  coord_sf(datum = NA) +
  labs(title = "Entrance-first distance origins", subtitle = "CRTM entrances where available; official point fallback only at uncovered surface stops", color = NULL) +
  theme_void(base_size = 11) + theme(legend.position = "bottom", plot.background = element_rect(fill = "#fbf8f1", color = NA), panel.background = element_rect(fill = "#fbf8f1", color = NA))
ggsave(file.path(qa_dir, "entrance_coverage_map.png"), p_coverage, width = 8.5, height = 7.5, dpi = 180, bg = "#fbf8f1")

status <- if (all(checks$passed)) "GO_PHASE3_ENTRANCE_FIRST_WITH_SURFACE_FALLBACK" else "PHASE3_REVIEW_REQUIRED"
top_dispersion <- dispersion[1:min(5L, .N)]
report <- c(
  "# Phase 3 — entrance-first canonical rail-station dataset", "",
  paste0("**Decision: ", status, "**"), "",
  paste0("The official CRTM snapshot produced **", nrow(stations), " canonical physical stations**, **", nrow(entrances), " in-scope official entrance points**, and ", nrow(origins), " preferred distance-origin records. A further ", nrow(excluded), " station records were excluded: 19 Cercanías stations outside Comunidad de Madrid and two unnamed incomplete Metro records."), "",
  "## Scope and counts", "",
  paste0("- Metro de Madrid: ", mode_counts[mode == "metro", stations], " physical stations."),
  paste0("- Metro Ligero: ", mode_counts[mode == "metro_ligero", stations], " physical stations."),
  paste0("- Cercanías within Comunidad de Madrid: ", mode_counts[mode == "cercanias", stations], " physical stations."),
  paste0("- Tranvía de Parla: ", mode_counts[mode == "tranvia_parla", stations], " physical stops."),
  paste0("- Cross-mode stations: ", station_dt[mode_count > 1L, .N], ". Overlaps are counted once in the 357-station union."), "",
  "The declared scope is `urban_rail`, with modes retained separately so Tranvía de Parla or any other mode can be filtered without rebuilding the station universe.", "",
  "## Canonicalisation rule", "",
  "CRTM `CODIGOINTERCAMBIADOR` groups the records belonging to a connected physical station/interchange. The retained reference point is an actual official CRTM station point: the source point closest to the group mean. It is used only as the fallback and sensitivity baseline. Raw source names, IDs, coordinates, lines and the source-to-station mapping are retained.", "",
  "One source anomaly is corrected explicitly and tested: Callao and Santo Domingo both carry interchange code 62 but are separate stations. They receive separate stable IDs. Editorial display-name overrides are limited to ambiguous cross-mode names; all original CRTM names remain in `source_names`.", "",
  "## Entrance completeness decision", "",
  paste0("- Metro: ", entrance_coverage[mode == "metro", stations_with_own_source_entrance], "/", entrance_coverage[mode == "metro", stations], " stations have a Metro access record (100%)."),
  paste0("- Cercanías within Comunidad de Madrid: ", entrance_coverage[mode == "cercanias", stations_with_own_source_entrance], "/", entrance_coverage[mode == "cercanias", stations], " stations have a Cercanías access record (100%)."),
  paste0("- Metro Ligero: ", entrance_coverage[mode == "metro_ligero", stations_with_own_source_entrance], "/", entrance_coverage[mode == "metro_ligero", stations], " stations have an M10 access record (", sprintf("%.1f%%", 100 * entrance_coverage[mode == "metro_ligero", own_source_coverage]), "). All nine underground Metro Ligero station complexes have official access records; the uncovered cases are surface stops."),
  paste0("- Tranvía de Parla: ", entrance_coverage[mode == "tranvia_parla", stations_with_own_source_entrance], "/", entrance_coverage[mode == "tranvia_parla", stations], " stops have an M10 access record. All are surface stops; Parla Centro has a Cercanías entrance through the same interchange."), "",
  paste0("Literal entrance coverage is therefore not sufficient to use entrance points alone across all four modes. The approved method is **nearest official entrance where available, otherwise the actual official station/stop point**. This gives 317 stations entrance-level geometry and 40 explicitly flagged surface-stop fallbacks. A station-point-only result must be computed alongside it as the sensitivity scenario."), "",
  "## Spatial QA", "",
  paste0("All reference points fall inside the Comunidad de Madrid boundary. The largest within-station source-point spread is ", sprintf("%.1f", max(station_dt$source_point_max_separation_m)), " m. Stations above the ", cfg$phase3$dispersion_review_threshold_m, " m review threshold are retained because their source records represent connected station complexes:"),
  paste0("- ", top_dispersion$station_name, ": ", sprintf("%.1f", top_dispersion$source_point_max_separation_m), " m (", top_dispersion$source_names, ")."), "",
  paste0("All entrance and reference points fall inside Comunidad de Madrid. The most distant entrance is ", sprintf("%.1f", max(entrance_dt$distance_to_station_reference_m)), " m from its retained station reference point. Preferred Euclidean distance is defined as the minimum distance to any origin belonging to the station. It must still be described as straight-line distance, not walking distance."), "",
  "## Provenance and next boundary", "",
  paste0("The six official CRTM station/access layers report `FECHAACTUAL = ", unique(source_records$source_data_date), "`; the immutable project snapshot date is ", cfg$phase3$station_snapshot, ". Reuse must cite CRTM, retain update metadata and follow the official open-data licence."), "",
  "Phase 3 ends here. No building-to-station distance joins, catchment populations or rankings are calculated in this phase."
)
writeLines(report, file.path(qa_dir, "phase3_report.md"))
jsonlite::write_json(list(
  status = status, generated_at = iso_now(), snapshot_date = cfg$phase3$station_snapshot,
  source_data_date = unique(source_records$source_data_date), station_count = nrow(stations),
  scope = cfg$phase3$scope, preferred_distance_method = cfg$phase3$preferred_distance_method,
  reference_point_method = cfg$phase3$reference_point_method,
  official_entrance_count = nrow(entrances), stations_with_official_entrances = sum(station_dt$has_official_entrance),
  station_point_fallback_count = sum(!station_dt$has_official_entrance),
  checks = lapply(seq_len(nrow(checks)), function(i) list(check = checks$check[i], passed = checks$passed[i]))
), file.path(cfg$paths$validation, "current_phase3_decision.json"), auto_unbox = TRUE, pretty = TRUE)

cat("Phase 3 validation decision:", status, "\n")
print(checks)
