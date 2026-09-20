#!/usr/bin/env Rscript
source("R/utils.R")
source("R/catastro_io.R")
source("R/spatial_allocation.R")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(data.table))

cfg <- read_config()
table_dir <- file.path(cfg$paths$validation, "phase1b")
ensure_dir(table_dir)

comparison <- fread(file.path(cfg$paths$validation, "tables", "cat_inspire_comparison.csv"))
component_counts <- fread(file.path(cfg$paths$validation, "tables", "multipart_connected_components.csv"))
disconnected_refs <- component_counts[connected_components > 1, parcel_ref]
comparison <- comparison[!is.na(residential_area_m2) & !is.na(municipality_code)]

building_paths <- list.files(cfg$paths$interim, pattern = "^buildings_[0-9]{5}\\.rds$", full.names = TRUE)
buildings <- do.call(rbind, lapply(building_paths, readRDS))
idx <- match(comparison$parcel_ref, buildings$parcel_ref)
if (anyNA(idx)) stop("Joined residential parcel lacks INSPIRE Building geometry.")
residential_buildings <- buildings[idx, ]
residential_buildings$parcel_dwellings <- comparison$reconstructed_dwellings

affected <- residential_buildings[residential_buildings$parcel_ref %in% disconnected_refs, ]
message("Creating connected components for ", nrow(affected), " disconnected residential parcels")
components <- explode_building_components(affected)
components$point_geometry <- NULL
components$parcel_dwellings <- comparison$reconstructed_dwellings[match(components$parcel_ref, comparison$parcel_ref)]
components$component_key <- paste(components$parcel_ref, components$component_id, sep = "_")

part_summary_rows <- list()
scan_summary_rows <- list()
archives <- list.files(cfg$paths$inspire_raw, pattern = "\\.zip$", full.names = TRUE)
for (municipality in cfg$catastro$municipalities) {
  code <- as.character(municipality$code)
  municipal_refs <- unique(components$parcel_ref[components$municipality_code == code])
  if (!length(municipal_refs)) next
  archive <- archives[endsWith(archives, paste0("BU.", code, ".zip"))]
  if (length(archive) != 1L) stop("Expected one INSPIRE archive for ", code)
  message("Scanning complete BuildingPart layer for ", code)
  parts <- st_read(
    gml_path_in_zip(archive, "buildingpart"), quiet = TRUE,
    query = "SELECT localId, numberOfFloorsAboveGround, geometry FROM BuildingPart",
    stringsAsFactors = FALSE
  )
  parts$building_ref <- sub("_part.*$", "", as.character(parts$localId))
  total_parts <- nrow(parts)
  parts <- parts[parts$building_ref %in% municipal_refs, ]
  municipal_components <- components[components$municipality_code == code, ]

  part_points <- suppressWarnings(st_point_on_surface(parts))
  hits <- st_within(part_points, municipal_components)
  component_index <- vapply(seq_along(hits), function(i) {
    candidates <- hits[[i]]
    same_parcel <- candidates[municipal_components$parcel_ref[candidates] == parts$building_ref[i]]
    if (length(same_parcel)) same_parcel[1] else NA_integer_
  }, integer(1))
  unmatched <- which(is.na(component_index))
  if (length(unmatched)) {
    for (i in unmatched) {
      candidates <- which(municipal_components$parcel_ref == parts$building_ref[i])
      if (length(candidates)) component_index[i] <- candidates[st_nearest_feature(part_points[i, ], municipal_components[candidates, ])]
    }
  }
  matched <- !is.na(component_index)
  floors <- pmax(safe_number(parts$numberOfFloorsAboveGround), 1, na.rm = TRUE)
  part_dt <- data.table(
    component_key = municipal_components$component_key[component_index[matched]],
    part_area_m2 = as.numeric(st_area(parts[matched, ])),
    floors_above = floors[matched]
  )
  part_summary_rows[[length(part_summary_rows) + 1L]] <- part_dt[, .(
    building_part_count = .N,
    building_part_area_m2 = sum(part_area_m2),
    volume_proxy = sum(part_area_m2 * floors_above),
    maximum_floors = max(floors_above)
  ), by = component_key]
  scan_summary_rows[[length(scan_summary_rows) + 1L]] <- data.table(
    municipality_code = code,
    building_parts_scanned = total_parts,
    relevant_building_parts = nrow(parts),
    matched_building_parts = sum(matched),
    unmatched_building_parts = sum(!matched)
  )
  rm(parts, part_points, hits, part_dt)
  gc()
}

part_summary <- rbindlist(part_summary_rows, fill = TRUE)
scan_summary <- rbindlist(scan_summary_rows, fill = TRUE)
component_dt <- as.data.table(st_drop_geometry(components))
component_dt <- merge(component_dt, part_summary, by = "component_key", all.x = TRUE)
component_dt[, volume_source := fifelse(is.na(volume_proxy) | volume_proxy <= 0, "footprint_fallback", "buildingpart_area_x_floors")]
component_dt[volume_source == "footprint_fallback", volume_proxy := footprint_m2]
component_dt[is.na(building_part_count), building_part_count := 0L]

allocated <- allocate_component_methods(
  component_dt,
  minor_max_footprint_m2 = cfg$phase1b$minor_component_max_footprint_m2,
  minor_max_volume_share = cfg$phase1b$minor_component_max_volume_share
)

component_points <- suppressWarnings(st_point_on_surface(components))
xy <- st_coordinates(component_points)
allocated[, `:=`(x = xy[, 1], y = xy[, 2])]

single <- residential_buildings[!residential_buildings$parcel_ref %in% disconnected_refs, ]
single_points <- suppressWarnings(st_point_on_surface(single))
single_xy <- st_coordinates(single_points)
single_dt <- data.table(
  component_key = paste0(single$parcel_ref, "_1"),
  parcel_ref = single$parcel_ref,
  municipality_code = single$municipality_code,
  component_id = 1L,
  footprint_m2 = as.numeric(st_area(single)),
  parcel_dwellings = single$parcel_dwellings,
  component_count = 1L,
  minor_ancillary = FALSE,
  volume_source = "single_component_not_required",
  x = single_xy[, 1], y = single_xy[, 2]
)
for (nm in c("dwellings_volume", "dwellings_volume_all", "dwellings_footprint", "dwellings_equal", "dwellings_largest")) {
  single_dt[, (nm) := parcel_dwellings]
}

keep <- c(
  "component_key", "parcel_ref", "municipality_code", "component_id", "component_count",
  "footprint_m2", "parcel_dwellings", "building_part_count", "volume_proxy", "volume_source",
  "minor_ancillary", "x", "y", "dwellings_volume", "dwellings_volume_all",
  "dwellings_footprint", "dwellings_equal", "dwellings_largest"
)
for (nm in setdiff(keep, names(single_dt))) single_dt[, (nm) := NA]
for (nm in setdiff(keep, names(allocated))) allocated[, (nm) := NA]
all_points <- rbindlist(list(single_dt[, ..keep], allocated[, ..keep]), use.names = TRUE)

methods <- grep("^dwellings_", names(all_points), value = TRUE)
conservation <- all_points[, c(
  list(parcel_dwellings = unique(parcel_dwellings), components = .N),
  lapply(.SD, sum, na.rm = TRUE)
), by = parcel_ref, .SDcols = methods]
for (nm in methods) {
  if (any(abs(conservation[[nm]] - conservation$parcel_dwellings) > 1e-8)) stop("Allocation does not conserve dwellings for method ", nm)
}

saveRDS(all_points, file.path(cfg$paths$interim, "phase1b_component_points.rds"), compress = "xz")
saveRDS(components, file.path(cfg$paths$interim, "phase1b_disconnected_components.rds"), compress = "xz")
fwrite(scan_summary, file.path(table_dir, "buildingpart_scan_summary.csv"))
fwrite(allocated, file.path(table_dir, "disconnected_component_allocations.csv"))
fwrite(conservation, file.path(table_dir, "parcel_allocation_conservation.csv"))

cat("Prepared", nrow(all_points), "component points for", uniqueN(all_points$parcel_ref), "residential parcels.\n")
