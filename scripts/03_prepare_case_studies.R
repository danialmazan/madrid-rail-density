#!/usr/bin/env Rscript
source("R/utils.R")
source("R/spatial_allocation.R")
cfg <- read_config()
case_dir <- file.path(cfg$paths$validation, "case_studies")
ensure_dir(case_dir)

building_paths <- list.files(cfg$paths$interim, pattern = "^buildings_[0-9]{5}\\.rds$", full.names = TRUE)
if (!length(building_paths)) stop("No parsed Building files. Run scripts/02_parse_catastro.R first.")
buildings <- do.call(rbind, lapply(building_paths, readRDS))
case_lookup <- data.table::rbindlist(lapply(cfg$catastro$case_parcels, data.table::as.data.table))
case_indices <- which(buildings$parcel_ref %in% case_lookup$parcel_ref)
if (!length(case_indices)) stop("None of the configured case parcels was found.")
building_attributes <- sf::st_drop_geometry(buildings)
building_geometry <- sf::st_geometry(buildings)
residential_path <- file.path(cfg$paths$interim, "residential_parcels.rds")
residential <- if (file.exists(residential_path)) readRDS(residential_path) else NULL
parts_paths <- list.files(cfg$paths$interim, pattern = "^case_building_parts_[0-9]{5}\\.rds$", full.names = TRUE)
case_parts <- if (length(parts_paths)) do.call(rbind, lapply(parts_paths, readRDS)) else NULL
if (!is.null(case_parts)) case_parts$building_ref <- sub("_part[0-9]+$", "", case_parts$localId)

component_tables <- list()
for (i in case_indices) {
  case_building <- sf::st_sf(building_attributes[i, , drop = FALSE], geometry = building_geometry[i])
  components <- explode_building_components(case_building)
  allocation_dwellings <- case_building$number_dwellings
  if (!is.null(residential)) {
    candidate <- residential[parcel_ref == case_building$parcel_ref, reconstructed_dwellings]
    if (length(candidate) == 1L && !is.na(candidate)) allocation_dwellings <- candidate
  }
  components$allocated_dwellings_footprint <- allocation_dwellings * components$footprint_share
  components$volume_proxy <- NA_real_
  components$volume_share <- NA_real_
  components$allocated_dwellings_volume <- NA_real_
  part_count <- NA_integer_
  if (!is.null(case_parts)) {
    parcel_parts <- case_parts[case_parts$building_ref == case_building$parcel_ref, ]
    part_count <- nrow(parcel_parts)
    if (part_count) {
      part_points <- suppressWarnings(sf::st_point_on_surface(parcel_parts))
      hits <- sf::st_within(part_points, components)
      component_index <- vapply(hits, function(x) if (length(x)) x[1] else NA_integer_, integer(1))
      if (anyNA(component_index)) stop("A case BuildingPart could not be assigned to a connected component for ", case_building$parcel_ref)
      part_area <- as.numeric(sf::st_area(parcel_parts))
      floors <- pmax(safe_number(parcel_parts$numberOfFloorsAboveGround), 1, na.rm = TRUE)
      proxy <- data.table::data.table(component_id = component_index, volume_proxy = part_area * floors)[, .(volume_proxy = sum(volume_proxy)), by = component_id]
      components$volume_proxy <- proxy$volume_proxy[match(components$component_id, proxy$component_id)]
      components$volume_share <- components$volume_proxy / sum(components$volume_proxy, na.rm = TRUE)
      components$allocated_dwellings_volume <- allocation_dwellings * components$volume_share
      if (!isTRUE(all.equal(sum(components$allocated_dwellings_volume), allocation_dwellings, tolerance = 1e-8))) stop("BuildingPart volume allocation did not preserve parcel dwellings.")
    }
  }
  case_name <- case_lookup[parcel_ref == case_building$parcel_ref, case][1]
  component_tables[[length(component_tables) + 1L]] <- data.table::data.table(
    parcel_ref = components$parcel_ref,
    case = case_name,
    component_id = components$component_id,
    footprint_m2 = components$footprint_m2,
    footprint_share = components$footprint_share,
    inspire_dwellings = components$number_dwellings,
    allocation_dwellings = allocation_dwellings,
    source_polygon_patches = components$patch_count,
    connected_component_count = nrow(components),
    building_part_count = part_count,
    allocated_dwellings_footprint = components$allocated_dwellings_footprint,
    volume_proxy = components$volume_proxy,
    volume_share = components$volume_share,
    allocated_dwellings_volume = components$allocated_dwellings_volume
  )

  png_path <- file.path(case_dir, paste0(case_building$parcel_ref, "_components.png"))
  grDevices::png(png_path, width = 1400, height = 1100, res = 160, bg = "#f4f0e8")
  old <- graphics::par(mar = c(1.5, 1.5, 4, 1.5), bg = "#f4f0e8")
  plot(sf::st_geometry(components), col = "#9dc3b3", border = "#183229", lwd = 1.5, axes = FALSE)
  points(sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(components))), pch = 21, bg = "#ef5b49", col = "#183229", cex = 1.2)
  labels <- sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(components)))
  component_labels <- if (all(is.na(components$allocated_dwellings_volume))) components$component_id else paste0(components$component_id, " · ", round(components$allocated_dwellings_volume))
  graphics::text(labels[, 1], labels[, 2], labels = component_labels, pos = 3, cex = 0.7, col = "#183229")
  subtitle <- if (all(is.na(components$allocated_dwellings_volume))) "Footprint allocation only; BuildingParts unavailable" else paste0(allocation_dwellings, " selected parcel dwellings allocated by BuildingPart floors × area; labels show component estimate")
  graphics::title(main = paste(case_name, case_building$parcel_ref, sep = " — "), sub = subtitle, col.main = "#183229", col.sub = "#183229")
  graphics::par(old)
  grDevices::dev.off()
}
data.table::fwrite(data.table::rbindlist(component_tables), file.path(case_dir, "case_component_allocations.csv"))
cat("Created", length(case_indices), "case-study component maps.\n")
