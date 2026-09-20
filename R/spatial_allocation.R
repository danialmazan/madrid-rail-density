connected_polygon_parts <- function(g, crs = NA) {
  geometry <- if (inherits(g, "sfc")) g else sf::st_sfc(g, crs = crs)
  valid <- suppressWarnings(sf::st_make_valid(geometry))
  patches <- suppressWarnings(sf::st_cast(valid, "POLYGON", warn = FALSE))
  if (!length(patches)) return(patches)
  dissolved <- suppressWarnings(sf::st_union(patches))
  suppressWarnings(sf::st_cast(dissolved, "POLYGON", warn = FALSE))
}

allocate_component_methods <- function(components, dwelling_col = "parcel_dwellings", minor_max_footprint_m2 = 25, minor_max_volume_share = 0.01) {
  x <- data.table::as.data.table(components)
  required <- c("parcel_ref", "component_id", "footprint_m2", "volume_proxy", dwelling_col)
  if (length(missing <- setdiff(required, names(x)))) stop("Missing component allocation columns: ", paste(missing, collapse = ", "))
  x[, volume_proxy := data.table::fifelse(is.na(volume_proxy) | volume_proxy <= 0, footprint_m2, volume_proxy)]
  x[, `:=`(
    raw_volume_share = volume_proxy / sum(volume_proxy),
    raw_footprint_share = footprint_m2 / sum(footprint_m2),
    component_count = .N
  ), by = parcel_ref]
  x[, minor_ancillary := component_count > 1 & footprint_m2 <= minor_max_footprint_m2 & raw_volume_share <= minor_max_volume_share]
  x[, eligible := !minor_ancillary]
  x[, `:=`(
    volume_share = data.table::fifelse(eligible, volume_proxy / sum(volume_proxy[eligible]), 0),
    footprint_share = data.table::fifelse(eligible, footprint_m2 / sum(footprint_m2[eligible]), 0),
    equal_share = data.table::fifelse(eligible, 1 / sum(eligible), 0)
  ), by = parcel_ref]
  x[, largest_share := as.numeric(eligible & volume_proxy == max(volume_proxy[eligible])), by = parcel_ref]
  x[, largest_share := largest_share / sum(largest_share), by = parcel_ref]
  d <- x[[dwelling_col]]
  x[, `:=`(
    dwellings_volume = d * volume_share,
    dwellings_volume_all = d * raw_volume_share,
    dwellings_footprint = d * footprint_share,
    dwellings_equal = d * equal_share,
    dwellings_largest = d * largest_share
  )]
  x[]
}

explode_building_components <- function(buildings) {
  original_total <- sum(buildings$number_dwellings, na.rm = TRUE)
  attrs <- sf::st_drop_geometry(buildings)
  source_geometry <- sf::st_geometry(buildings)
  geometry_parts <- lapply(seq_len(nrow(buildings)), function(i) {
    parts <- connected_polygon_parts(source_geometry[i], sf::st_crs(buildings))
    if (!length(parts)) return(NULL)
    row <- attrs[rep(i, length(parts)), , drop = FALSE]
    row$component_id <- seq_along(parts)
    sf::st_sf(row, geometry = parts, crs = sf::st_crs(buildings))
  })
  out <- do.call(rbind, geometry_parts)
  out$footprint_m2 <- as.numeric(sf::st_area(out))
  totals <- ave(out$footprint_m2, out$parcel_ref, FUN = sum)
  out$footprint_share <- ifelse(totals > 0, out$footprint_m2 / totals, NA_real_)
  out$allocated_dwellings_footprint <- out$number_dwellings * out$footprint_share
  allocated_total <- sum(out$allocated_dwellings_footprint, na.rm = TRUE)
  if (!isTRUE(all.equal(original_total, allocated_total, tolerance = 1e-8))) {
    stop("Component allocation failed to preserve the parcel dwelling total.")
  }
  out$point_geometry <- sf::st_geometry(suppressWarnings(sf::st_point_on_surface(out)))
  out
}
