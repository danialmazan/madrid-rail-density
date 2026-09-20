#!/usr/bin/env Rscript
source("R/utils.R")
source("R/catastro_io.R")
source("R/spatial_allocation.R")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(data.table))

cfg <- read_config()
building_dir <- file.path(cfg$paths$phase2_interim, "buildings_by_municipality")
cat_dir <- file.path(cfg$paths$phase2_interim, "residential_by_municipality")
out_dir <- file.path(cfg$paths$phase2_interim, "components_by_municipality")
ensure_dir(out_dir)

archives <- list.files(cfg$paths$inspire_raw, pattern = "A[.]ES[.]SDGC[.]BU[.][0-9]{5}[.]zip$", full.names = TRUE)
codes <- sort(sub(".*BU[.]([0-9]{5})[.]zip$", "\\1", archives))
scan_rows <- vector("list", length(codes))

for (i in seq_along(codes)) {
  code <- codes[i]
  polygon_path <- file.path(out_dir, paste0("components_", code, ".rds"))
  point_path <- file.path(out_dir, paste0("component_points_", code, ".rds"))
  cache_valid <- FALSE
  if (file.exists(polygon_path) && file.exists(point_path)) {
    cached_names <- names(readRDS(point_path))
    cached <- readRDS(point_path)
    cache_valid <- all(c("dwelling_rule", "confidence", "raw_residential_property_count", "weight_source") %in% cached_names) &&
      identical(attr(cached, "component_support_version"), "urban_rural_cat_plus_inspire_fallback_v5")
  }
  if (cache_valid) {
    message("[", i, "/", length(codes), "] Cached residential components ", code)
    comp <- readRDS(point_path)
    scan_rows[[i]] <- data.table(
      municipality_code = code, residential_parcels = uniqueN(comp$parcel_ref),
      components = nrow(comp), disconnected_parcels = sum(!duplicated(comp$parcel_ref) & comp$component_count > 1),
      building_parts_scanned = NA_integer_, relevant_building_parts = sum(comp$building_part_count, na.rm = TRUE),
      footprint_fallback_components = sum(comp$volume_source == "footprint_fallback", na.rm = TRUE)
    )
    next
  }

  message("[", i, "/", length(codes), "] Constructing residential components ", code)
  buildings <- readRDS(file.path(building_dir, paste0("residential_buildings_", code, ".rds")))
  residential <- readRDS(file.path(cat_dir, paste0("residential_", code, ".rds")))
  cat_index <- match(buildings$parcel_ref, residential$parcel_ref)
  has_cat <- !is.na(cat_index)
  cat_dwelling <- residential$reconstructed_dwellings[cat_index]
  cat_area <- residential$residential_area_m2[cat_index]
  municipality_median_m2 <- stats::median(
    residential$residential_area_m2[residential$reconstructed_dwellings > 0] /
      residential$reconstructed_dwellings[residential$reconstructed_dwellings > 0],
    na.rm = TRUE
  )
  if (!is.finite(municipality_median_m2) || municipality_median_m2 <= 0) municipality_median_m2 <- 100
  use_cat_area_equivalent <- has_cat & (is.na(cat_dwelling) | cat_dwelling <= 0)
  buildings$parcel_dwellings <- ifelse(has_cat, cat_dwelling, buildings$number_dwellings)
  buildings$parcel_dwellings[use_cat_area_equivalent] <- cat_area[use_cat_area_equivalent] / municipality_median_m2
  buildings$parcel_residential_area_m2 <- ifelse(has_cat, residential$residential_area_m2[cat_index], buildings$official_area_m2)
  buildings$raw_residential_property_count <- ifelse(has_cat, residential$raw_residential_property_count[cat_index], buildings$number_dwellings)
  buildings$dwelling_rule <- ifelse(has_cat, residential$dwelling_rule[cat_index], "inspire_dwelling_fallback")
  buildings$dwelling_rule[use_cat_area_equivalent] <- "cat_area_dwelling_equivalent_fallback"
  buildings$confidence <- ifelse(has_cat, residential$confidence[cat_index], "fallback")
  buildings$confidence[use_cat_area_equivalent] <- "fallback"
  buildings$residential_area_share <- ifelse(has_cat, residential$residential_area_share[cat_index], 1)
  cat_domain <- residential$cat_domain[cat_index]
  buildings$weight_source <- ifelse(has_cat & cat_domain == "U", "CAT_urban_reconstructed",
    ifelse(has_cat & cat_domain == "R", "CAT_rural_reconstructed", "INSPIRE_numberOfDwellings_fallback"))
  buildings$weight_source[use_cat_area_equivalent] <- ifelse(cat_domain[use_cat_area_equivalent] == "R",
    "CAT_rural_area_dwelling_equivalent", "CAT_urban_area_dwelling_equivalent")
  if (anyNA(buildings$parcel_dwellings) || any(buildings$parcel_dwellings <= 0)) stop("Invalid parcel dwelling weight for ", code)

  # Valid INSPIRE MULTIPOLYGON members are already disconnected polygon
  # components. Vectorized casting is equivalent to the Phase 1 union/cast
  # result for all 14,856 tested prototype multipart parcels and is far faster.
  single <- buildings[buildings$patch_count <= 1L, ]
  single$component_id <- 1L
  multi <- suppressWarnings(st_cast(buildings[buildings$patch_count > 1L, ], "POLYGON", warn = FALSE))
  if (nrow(multi)) multi$component_id <- ave(seq_len(nrow(multi)), multi$parcel_ref, FUN = seq_along)
  components <- rbind(single, multi)
  components$component_key <- paste(components$parcel_ref, components$component_id, sep = "_")
  components$footprint_m2 <- as.numeric(st_area(components))
  component_counts <- as.integer(table(components$parcel_ref)[components$parcel_ref])
  components$component_count <- component_counts
  disconnected_refs <- unique(components$parcel_ref[components$component_count > 1L])

  component_dt <- as.data.table(st_drop_geometry(components))
  component_dt[, `:=`(
    building_part_count = 0L,
    volume_proxy = footprint_m2,
    maximum_floors = NA_real_,
    volume_source = fifelse(component_count == 1L, "single_component_not_required", "footprint_fallback")
  )]
  total_parts <- 0L
  relevant_parts <- 0L

  if (length(disconnected_refs)) {
    archive <- archives[endsWith(archives, paste0("BU.", code, ".zip"))]
    message("    scanning BuildingPart heights for ", length(disconnected_refs), " disconnected parcels")
    parts <- st_read(
      gml_path_in_zip(archive, "buildingpart"), quiet = TRUE,
      query = "SELECT localId, numberOfFloorsAboveGround, geometry FROM BuildingPart",
      stringsAsFactors = FALSE
    )
    if (st_crs(parts)$epsg != cfg$project$crs) parts <- st_transform(parts, cfg$project$crs)
    total_parts <- nrow(parts)
    parts$building_ref <- sub("_part.*$", "", as.character(parts$localId))
    parts <- parts[parts$building_ref %in% disconnected_refs, ]
    relevant_parts <- nrow(parts)
    if (nrow(parts)) {
      relevant_components <- components[components$parcel_ref %in% disconnected_refs, ]
      part_points <- suppressWarnings(st_point_on_surface(parts))
      hits <- st_within(part_points, relevant_components)
      component_index <- vapply(seq_along(hits), function(k) {
        candidates <- hits[[k]]
        same <- candidates[relevant_components$parcel_ref[candidates] == parts$building_ref[k]]
        if (length(same)) same[1] else NA_integer_
      }, integer(1))
      unmatched <- which(is.na(component_index))
      if (length(unmatched)) {
        for (k in unmatched) {
          candidates <- which(relevant_components$parcel_ref == parts$building_ref[k])
          if (length(candidates)) component_index[k] <- candidates[st_nearest_feature(part_points[k, ], relevant_components[candidates, ])]
        }
      }
      matched <- !is.na(component_index)
      floors <- safe_number(parts$numberOfFloorsAboveGround)
      floors[is.na(floors) | floors < 1] <- 1
      part_dt <- data.table(
        component_key = relevant_components$component_key[component_index[matched]],
        part_area_m2 = as.numeric(st_area(parts[matched, ])), floors_above = floors[matched]
      )
      part_summary <- part_dt[, .(
        building_part_count = .N,
        volume_proxy = sum(part_area_m2 * floors_above),
        maximum_floors = max(floors_above)
      ), by = component_key]
      hit <- match(component_dt$component_key, part_summary$component_key)
      has_part <- !is.na(hit)
      component_dt$building_part_count[has_part] <- part_summary$building_part_count[hit[has_part]]
      component_dt$volume_proxy[has_part] <- part_summary$volume_proxy[hit[has_part]]
      component_dt$maximum_floors[has_part] <- part_summary$maximum_floors[hit[has_part]]
      component_dt$volume_source[has_part] <- "buildingpart_area_x_floors"
      rm(parts, part_points, hits, relevant_components, part_dt, part_summary)
      gc()
    }
  }

  allocated <- allocate_component_methods(
    component_dt, dwelling_col = "parcel_dwellings",
    minor_max_footprint_m2 = cfg$phase1b$minor_component_max_footprint_m2,
    minor_max_volume_share = cfg$phase1b$minor_component_max_volume_share
  )
  allocated[, `:=`(
    chosen_weight = dwellings_volume,
    chosen_weight_method = "cat_reconstructed_dwellings_component_volume",
    component_residential_area_m2 = parcel_residential_area_m2 * volume_share
  )]
  conservation <- allocated[, .(allocated = sum(chosen_weight), target = unique(parcel_dwellings)), by = parcel_ref]
  if (any(abs(conservation$allocated - conservation$target) > 1e-7)) {
    stop("Component dwelling weights do not conserve parcel totals for ", code)
  }

  ord <- match(components$component_key, allocated$component_key)
  components <- st_sf(allocated[ord], geometry = st_geometry(components), crs = st_crs(components))
  points <- suppressWarnings(st_point_on_surface(components))
  attr(components, "component_support_version") <- "urban_rural_cat_plus_inspire_fallback_v5"
  attr(points, "component_support_version") <- "urban_rural_cat_plus_inspire_fallback_v5"
  # Per-municipality caches favor fast restart/read performance; the combined
  # regional point layer below remains xz-compressed for compact storage.
  saveRDS(components, polygon_path, compress = "gzip")
  saveRDS(points, point_path, compress = "gzip")
  scan_rows[[i]] <- data.table(
    municipality_code = code, residential_parcels = uniqueN(allocated$parcel_ref), components = nrow(allocated),
    disconnected_parcels = uniqueN(allocated[component_count > 1, parcel_ref]),
    building_parts_scanned = total_parts, relevant_building_parts = relevant_parts,
    footprint_fallback_components = sum(allocated$volume_source == "footprint_fallback")
  )
  rm(buildings, residential, components, points, allocated, component_dt, single, multi)
  gc()
}

point_paths <- list.files(out_dir, pattern = "^component_points_[0-9]{5}[.]rds$", full.names = TRUE)
regional_points <- do.call(rbind, lapply(point_paths, readRDS))
if (anyDuplicated(regional_points$component_key)) stop("Regional component keys are not unique.")
saveRDS(regional_points, file.path(cfg$paths$phase2_interim, "regional_component_points.rds"), compress = "xz")
fwrite(rbindlist(scan_rows, fill = TRUE), file.path(cfg$paths$phase2_interim, "regional_component_summary.csv"))
cat("Prepared", nrow(regional_points), "residential component points for", uniqueN(regional_points$parcel_ref), "parcels.\n")
