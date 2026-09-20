#!/usr/bin/env Rscript
source("R/utils.R")
source("R/catastro_io.R")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(data.table))

cfg <- read_config()
cat_dir <- file.path(cfg$paths$phase2_interim, "residential_by_municipality")
out_dir <- file.path(cfg$paths$phase2_interim, "buildings_by_municipality")
ensure_dir(out_dir)

archives <- list.files(cfg$paths$inspire_raw, pattern = "A[.]ES[.]SDGC[.]BU[.][0-9]{5}[.]zip$", full.names = TRUE)
codes <- sort(sub(".*BU[.]([0-9]{5})[.]zip$", "\\1", archives))
if (length(codes) != 179L) stop("Expected 179 municipal INSPIRE archives.")

summaries <- vector("list", length(codes))
for (i in seq_along(codes)) {
  code <- codes[i]
  destination <- file.path(out_dir, paste0("residential_buildings_", code, ".rds"))
  archive <- archives[endsWith(archives, paste0("BU.", code, ".zip"))]
  residential <- readRDS(file.path(cat_dir, paste0("residential_", code, ".rds")))
  cache_valid <- FALSE
  if (file.exists(destination)) {
    cached <- readRDS(destination)
    cache_valid <- identical(attr(cached, "retention_rule"), "urban_rural_cat_residential_or_inspire_dwelling_v4")
  }
  if (cache_valid) {
    message("[", i, "/", length(codes), "] Cached INSPIRE residential buildings ", code)
    matched <- cached
    total_features <- attr(matched, "municipal_building_feature_count")
    if (is.null(total_features)) total_features <- NA_integer_
  } else {
    message("[", i, "/", length(codes), "] Reading INSPIRE Building ", code)
    raw <- read_inspire_layer(archive, "building")
    total_features <- nrow(raw)
    buildings <- standardize_buildings(raw, code)
    if (anyDuplicated(buildings$parcel_ref)) stop("Repeated INSPIRE parcel reference in municipality ", code)
    # Retain any residual INSPIRE-only parcel with a positive dwelling count so
    # CAT-missing housing is not silently removed from spatial support.
    keep <- buildings$parcel_ref %in% residential$parcel_ref | (!is.na(buildings$number_dwellings) & buildings$number_dwellings > 0)
    matched <- buildings[keep, ]
    matched$cat_residential_match <- matched$parcel_ref %in% residential$parcel_ref
    attr(matched, "municipal_building_feature_count") <- total_features
    attr(matched, "retention_rule") <- "urban_rural_cat_residential_or_inspire_dwelling_v4"
    saveRDS(matched, destination, compress = "xz")
    rm(raw, buildings)
    gc()
  }
  joined <- residential$parcel_ref %in% matched$parcel_ref
  summaries[[i]] <- data.table(
    municipality_code = code,
    inspire_building_features = total_features,
    residential_cat_parcels = nrow(residential),
    joined_residential_parcels = sum(joined),
    inspire_only_dwelling_parcels = sum(!matched$cat_residential_match),
    inspire_only_dwellings = sum(matched$number_dwellings[!matched$cat_residential_match], na.rm = TRUE),
    parcel_join_share = if (nrow(residential)) mean(joined) else 1,
    joined_dwelling_share = if (sum(residential$reconstructed_dwellings, na.rm = TRUE) > 0) {
      sum(residential$reconstructed_dwellings[joined], na.rm = TRUE) / sum(residential$reconstructed_dwellings, na.rm = TRUE)
    } else 1,
    joined_residential_area_share = if (sum(residential$residential_area_m2, na.rm = TRUE) > 0) {
      sum(residential$residential_area_m2[joined], na.rm = TRUE) / sum(residential$residential_area_m2, na.rm = TRUE)
    } else 1
  )
}

summary_dt <- rbindlist(summaries)
paths <- list.files(out_dir, pattern = "^residential_buildings_[0-9]{5}[.]rds$", full.names = TRUE)
regional <- do.call(rbind, lapply(paths, readRDS))
if (anyDuplicated(regional$parcel_ref)) stop("Regional INSPIRE residential parcel references are not unique.")
saveRDS(regional, file.path(cfg$paths$phase2_interim, "regional_residential_buildings.rds"), compress = "xz")
fwrite(summary_dt, file.path(cfg$paths$phase2_interim, "regional_inspire_join_summary.csv"))

cat("Matched", nrow(regional), "residential parcels to INSPIRE Building geometries.\n")
cat("Dwelling-weighted join coverage:", sprintf("%.4f%%", 100 * weighted.mean(summary_dt$joined_dwelling_share, pmax(summary_dt$residential_cat_parcels, 1))), "(municipal diagnostic).\n")
