#!/usr/bin/env Rscript
source("R/utils.R")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))

cfg <- read_config()
out_dir <- file.path(cfg$paths$validation, "phase1b")
plot_dir <- file.path(out_dir, "maps")
ensure_dir(out_dir)
ensure_dir(plot_dir)

points <- readRDS(file.path(cfg$paths$interim, "phase1b_component_points.rds"))
station_paths <- list.files(cfg$paths$rail_raw, pattern = "_estaciones\\.geojson$", full.names = TRUE)
if (length(station_paths) != length(cfg$phase1b$station_services)) stop("Missing official CRTM prototype station layers.")

station_rows <- lapply(station_paths, function(path) {
  x <- st_read(path, quiet = TRUE, stringsAsFactors = FALSE)
  if (st_crs(x)$epsg != cfg$project$crs) x <- st_transform(x, cfg$project$crs)
  x$source_layer <- sub("_estaciones\\.geojson$", "", basename(path))
  x
})
stations_raw <- do.call(rbind, station_rows)
stations_raw <- stations_raw[as.character(stations_raw$CODIGOPROVINCIA) == "28", ]
prototype_municipality_codes <- vapply(cfg$catastro$municipalities, function(x) sub("^28", "", x$ine_code), character(1))
stations_raw <- stations_raw[as.character(stations_raw$CODIGOMUNICIPIO) %in% prototype_municipality_codes, ]
station_xy <- st_coordinates(stations_raw)
station_dt <- as.data.table(st_drop_geometry(stations_raw))
station_dt[, `:=`(x = station_xy[, 1], y = station_xy[, 2])]
station_dt[, normalized_name := toupper(iconv(DENOMINACION, to = "ASCII//TRANSLIT"))]
station_dt[, normalized_name := gsub("[^A-Z0-9]", "", normalized_name)]
station_dt[, interchange_code := trim_na(CODIGOINTERCAMBIADOR)]
station_dt[, canonical_key := fifelse(!is.na(interchange_code), paste0("I_", interchange_code), paste0("N_", normalized_name))]
stations <- station_dt[, .(
  station_name = paste(sort(unique(DENOMINACION)), collapse = " / "),
  modes = paste(sort(unique(as.character(MODO))), collapse = "+"),
  municipality_codes = paste(sort(unique(as.character(CODIGOMUNICIPIO))), collapse = "+"),
  source_records = .N,
  x = mean(x), y = mean(y)
), by = .(station_id = canonical_key)]

section_path <- file.path("data", "raw", "ine", "2021", "secciones_2021_prototype.geojson")
sections <- st_read(section_path, quiet = TRUE)
sections <- sections[sections$TIPO == "SECCIONADO", ]
if (st_crs(sections)$epsg != cfg$project$crs) sections <- st_transform(sections, cfg$project$crs)
study_area <- suppressWarnings(st_union(st_make_valid(sections)))
station_sf <- st_as_sf(stations, coords = c("x", "y"), crs = cfg$project$crs, remove = FALSE)
inside_study_area <- lengths(st_within(station_sf, study_area)) > 0
boundary <- st_boundary(study_area)
stations[, coverage_limit_m := 0]
stations[inside_study_area, coverage_limit_m := as.numeric(st_distance(station_sf[inside_study_area, ], boundary, by_element = FALSE)[, 1])]
fwrite(stations, file.path(out_dir, "prototype_stations.csv"))

radii <- as.numeric(unlist(cfg$phase1b$radii_m))
max_radius <- max(radii)
weight_cols <- c(
  volume = "dwellings_volume",
  volume_all = "dwellings_volume_all",
  footprint = "dwellings_footprint",
  equal = "dwellings_equal",
  largest = "dwellings_largest"
)
px <- points$x
py <- points$y
result_rows <- vector("list", nrow(stations))

for (i in seq_len(nrow(stations))) {
  bbox_candidates <- which(abs(px - stations$x[i]) <= max_radius & abs(py - stations$y[i]) <= max_radius)
  if (!length(bbox_candidates)) next
  candidate_distance <- sqrt((px[bbox_candidates] - stations$x[i])^2 + (py[bbox_candidates] - stations$y[i])^2)
  near <- bbox_candidates[candidate_distance <= max_radius]
  if (!length(near)) next

  relevant_refs <- unique(points$parcel_ref[near])
  bound_idx <- which(points$parcel_ref %in% relevant_refs)
  bound_distance <- sqrt((px[bound_idx] - stations$x[i])^2 + (py[bound_idx] - stations$y[i])^2)
  bound_dt <- data.table(
    parcel_ref = points$parcel_ref[bound_idx],
    parcel_dwellings = points$parcel_dwellings[bound_idx],
    distance_m = bound_distance
  )[, .(parcel_dwellings = parcel_dwellings[1], minimum_distance_m = min(distance_m), maximum_distance_m = max(distance_m)), by = parcel_ref]

  rows <- lapply(radii, function(radius) {
    within <- bbox_candidates[candidate_distance <= radius]
    values <- vapply(weight_cols, function(column) sum(points[[column]][within], na.rm = TRUE), numeric(1))
    data.table(
      station_id = stations$station_id[i], station_name = stations$station_name[i], modes = stations$modes[i],
      radius_m = radius,
      coverage_complete = radius <= stations$coverage_limit_m[i],
      dwellings_volume = values[["volume"]],
      dwellings_volume_all = values[["volume_all"]],
      dwellings_footprint = values[["footprint"]],
      dwellings_equal = values[["equal"]],
      dwellings_largest = values[["largest"]],
      dwellings_lower_bound = bound_dt[maximum_distance_m <= radius, sum(parcel_dwellings)],
      dwellings_upper_bound = bound_dt[minimum_distance_m <= radius, sum(parcel_dwellings)]
    )
  })
  result_rows[[i]] <- rbindlist(rows)
  if (i %% 50 == 0) message("Processed ", i, "/", nrow(stations), " canonical stations")
}
catchments <- rbindlist(result_rows, fill = TRUE)
fwrite(catchments, file.path(out_dir, "station_dwelling_sensitivity.csv"))

alternative_cols <- c("dwellings_volume_all", "dwellings_footprint", "dwellings_equal", "dwellings_largest")
long <- melt(
  catchments,
  id.vars = c("station_id", "station_name", "modes", "radius_m", "coverage_complete", "dwellings_volume", "dwellings_lower_bound", "dwellings_upper_bound"),
  measure.vars = alternative_cols,
  variable.name = "method", value.name = "alternative_dwellings"
)
long[, method := sub("^dwellings_", "", as.character(method))]
long[, `:=`(
  signed_relative_difference = (alternative_dwellings - dwellings_volume) / dwellings_volume,
  absolute_relative_difference = abs(alternative_dwellings - dwellings_volume) / dwellings_volume,
  bound_width_relative = (dwellings_upper_bound - dwellings_lower_bound) / dwellings_volume
)]
eligible <- long[coverage_complete == TRUE & dwellings_volume >= cfg$phase1b$minimum_baseline_dwellings & is.finite(absolute_relative_difference)]
metrics <- eligible[, .(
  station_radius_cells = .N,
  median_absolute_relative_difference = median(absolute_relative_difference),
  p95_absolute_relative_difference = as.numeric(quantile(absolute_relative_difference, 0.95)),
  maximum_absolute_relative_difference = max(absolute_relative_difference),
  share_within_tolerance = mean(absolute_relative_difference <= cfg$phase1b$sensitivity_relative_tolerance)
), by = method]
fwrite(metrics, file.path(out_dir, "sensitivity_metrics.csv"))

by_radius <- eligible[, .(
  station_cells = .N,
  median_absolute_relative_difference = median(absolute_relative_difference),
  p95_absolute_relative_difference = as.numeric(quantile(absolute_relative_difference, 0.95)),
  share_within_tolerance = mean(absolute_relative_difference <= cfg$phase1b$sensitivity_relative_tolerance)
), by = .(method, radius_m)]
fwrite(by_radius, file.path(out_dir, "sensitivity_by_radius.csv"))

outliers <- eligible[method %in% c("volume_all", "footprint") & absolute_relative_difference > cfg$phase1b$sensitivity_relative_tolerance]
setorder(outliers, -absolute_relative_difference)
fwrite(outliers, file.path(out_dir, "sensitivity_outlier_cells.csv"))

station_summary <- eligible[, .(
  maximum_footprint_difference = max(absolute_relative_difference[method == "footprint"], na.rm = TRUE),
  maximum_equal_difference = max(absolute_relative_difference[method == "equal"], na.rm = TRUE),
  maximum_largest_difference = max(absolute_relative_difference[method == "largest"], na.rm = TRUE),
  maximum_bound_width = max(bound_width_relative, na.rm = TRUE)
), by = .(station_id, station_name, modes)]
setorder(station_summary, -maximum_footprint_difference)
fwrite(station_summary, file.path(out_dir, "station_sensitivity_summary.csv"))

gate_methods <- c("volume_all", "footprint")
gate_rows <- metrics[method %in% gate_methods]
gate_pass <- nrow(gate_rows) == length(gate_methods) && all(gate_rows$share_within_tolerance >= cfg$phase1b$sensitivity_required_cell_share)
status <- list(
  status = if (gate_pass) "GO_MODIFIED_COMPONENT_ALLOCATION" else "NO_GO_REDESIGN",
  reason = if (gate_pass) {
    paste0(
      "Prototype station-distance results are stable: both footprint allocation and volume allocation without ancillary exclusion remain within ",
      100 * cfg$phase1b$sensitivity_relative_tolerance, "% of the selected BuildingPart-volume method for at least ",
      100 * cfg$phase1b$sensitivity_required_cell_share, "% of eligible station-radius cells."
    )
  } else {
    paste0(
      "Prototype station-distance results are not sufficiently stable: footprint allocation and/or volume allocation without ancillary exclusion fails the ",
      100 * cfg$phase1b$sensitivity_required_cell_share, "% within ", 100 * cfg$phase1b$sensitivity_relative_tolerance, "% gate."
    )
  },
  decision_methods = gate_methods,
  tolerance = cfg$phase1b$sensitivity_relative_tolerance,
  required_cell_share = cfg$phase1b$sensitivity_required_cell_share,
  canonical_stations = nrow(stations),
  complete_coverage_stations = uniqueN(eligible$station_id),
  station_radius_cells = nrow(unique(eligible[, .(station_id, radius_m)]))
)
jsonlite::write_json(status, file.path(out_dir, "phase1b_status.json"), auto_unbox = TRUE, pretty = TRUE, digits = 8)
current_decision <- c(status, list(
  approved_weight = if (gate_pass) "cat_reconstructed_dwellings" else NULL,
  approved_spatial_support = if (gate_pass) "connected_building_components" else NULL,
  component_allocation = if (gate_pass) "buildingpart_footprint_area_x_floors_above_ground" else NULL,
  ancillary_rule = if (gate_pass) paste0("exclude components <= ", cfg$phase1b$minor_component_max_footprint_m2, " m2 and <= ", 100 * cfg$phase1b$minor_component_max_volume_share, "% of parcel volume; redistribute within parcel") else NULL,
  phase2_started = FALSE
))
jsonlite::write_json(current_decision, file.path(cfg$paths$validation, "current_method_decision.json"), auto_unbox = TRUE, pretty = TRUE, digits = 8, null = "null")

p1 <- ggplot(eligible, aes(x = absolute_relative_difference * 100, fill = method)) +
  geom_histogram(bins = 60, position = "identity", alpha = 0.45) +
  geom_vline(xintercept = 100 * cfg$phase1b$sensitivity_relative_tolerance, linetype = 2, color = "#ef5b49") +
  coord_cartesian(xlim = c(0, as.numeric(quantile(eligible$absolute_relative_difference * 100, 0.99)))) +
  labs(title = "Sensitivity to physical-component allocation", subtitle = "Prototype dwelling catchments; absolute difference from BuildingPart-volume allocation", x = "Absolute difference (%)", y = "Station-radius cells", fill = NULL) +
  theme_minimal(base_size = 12)
ggsave(file.path(plot_dir, "allocation_difference_distribution.png"), p1, width = 8, height = 5, dpi = 160, bg = "#f4f0e8")

footprint_heat <- eligible[method == "footprint"]
p2 <- ggplot(footprint_heat, aes(x = radius_m, y = reorder(station_name, absolute_relative_difference, FUN = max), fill = signed_relative_difference * 100)) +
  geom_tile() +
  scale_fill_gradient2(low = "#326c83", mid = "#f4f0e8", high = "#ef5b49", midpoint = 0) +
  labs(title = "Footprint versus BuildingPart-volume allocation", subtitle = "Only canonical stations with at least 100 modeled dwellings in the catchment", x = "Straight-line radius (m)", y = NULL, fill = "Difference (%)") +
  theme_minimal(base_size = 9) +
  theme(axis.text.y = element_text(size = 5))
ggsave(file.path(plot_dir, "footprint_volume_station_heatmap.png"), p2, width = 10, height = 14, dpi = 160, bg = "#f4f0e8", limitsize = FALSE)

scan <- fread(file.path(out_dir, "buildingpart_scan_summary.csv"))
alloc <- fread(file.path(out_dir, "disconnected_component_allocations.csv"))
minor_equivalent <- alloc[minor_ancillary == TRUE, sum(parcel_dwellings * raw_volume_share)]
report <- c(
  "# Phase 1b component-allocation sensitivity",
  "",
  paste0("**Status: ", status$status, "**"),
  "",
  status$reason,
  "",
  "## Scope",
  "",
  paste0("- Canonical preliminary CRTM stations: ", nrow(stations), "."),
  paste0("- Stations contributing at least one complete, eligible catchment: ", status$complete_coverage_stations, "."),
  paste0("- Station-radius cells evaluated: ", status$station_radius_cells, "."),
  paste0("- BuildingParts scanned: ", format(sum(scan$building_parts_scanned), big.mark = ","), "."),
  paste0("- Relevant BuildingParts matched to components: ", format(sum(scan$matched_building_parts), big.mark = ","), "; unmatched: ", sum(scan$unmatched_building_parts), "."),
  paste0("- Disconnected residential parcels: ", uniqueN(alloc$parcel_ref), "; connected components: ", nrow(alloc), "."),
  paste0("- Minor ancillary components excluded from the preferred allocation: ", sum(alloc$minor_ancillary), ", representing ", sprintf("%.1f", minor_equivalent), " pre-redistribution dwelling-equivalents."),
  "",
  "## Sensitivity metrics",
  "",
  paste0("The decision gate requires at least ", 100 * cfg$phase1b$sensitivity_required_cell_share, "% of station-radius cells to remain within ", 100 * cfg$phase1b$sensitivity_relative_tolerance, "% of the preferred allocation for both footprint weighting and volume weighting without ancillary exclusion."),
  "",
  paste0("- ", metrics$method, ": median ", sprintf("%.3f%%", 100 * metrics$median_absolute_relative_difference), "; 95th percentile ", sprintf("%.3f%%", 100 * metrics$p95_absolute_relative_difference), "; within tolerance ", sprintf("%.2f%%", 100 * metrics$share_within_tolerance), "."),
  paste0("- Decision-method outliers above tolerance: ", nrow(outliers), " cells across ", uniqueN(outliers$station_id), " stations; maximum ", sprintf("%.2f%%", 100 * max(outliers$absolute_relative_difference, na.rm = TRUE)), ". These remain flagged for Phase 2 QA."),
  "",
  "`equal` and `largest` are stress tests and do not determine the gate. Lower/upper bounds assign every parcel to the least/most favorable component relative to each station; they are deliberately extreme and are reported separately.",
  "",
  "## Interpretation",
  "",
  if (gate_pass) {
    "The project may proceed using CAT-derived parcel dwellings distributed over connected physical components by BuildingPart footprint area × floors above ground, after excluding only tiny one-percent ancillary components. Every parcel total is conserved. Outputs must describe this as modeled component allocation rather than an observed building-level dwelling count."
  } else {
    "The current component proxy is too consequential for station-distance results. Do not proceed to the regional population layer without richer address/entrance linkage or a coarser parcel-based support."
  },
  "",
  "This Phase 1b test uses dwelling catchments, not population estimates, and does not start Phase 2."
)
writeLines(report, file.path(out_dir, "phase1b_report.md"))
cat(status$status, "-", status$reason, "\n")
