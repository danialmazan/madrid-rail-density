#!/usr/bin/env Rscript
source("R/utils.R")
source("R/catastro_io.R")
source("R/dwelling_rules.R")
suppressPackageStartupMessages(library(data.table))

cfg <- read_config()
out_dir <- file.path(cfg$paths$phase2_interim, "residential_by_municipality")
ensure_dir(out_dir)

inspire_archives <- list.files(cfg$paths$inspire_raw, pattern = "A[.]ES[.]SDGC[.]BU[.][0-9]{5}[.]zip$", full.names = TRUE)
codes <- sort(sub(".*BU[.]([0-9]{5})[.]zip$", "\\1", inspire_archives))
if (length(codes) != 179L) stop("Expected 179 Madrid municipality codes from the ATOM downloads.")

delivery_zips <- list.files(cfg$paths$cat_raw, pattern = "[.]zip$", recursive = FALSE, full.names = TRUE, ignore.case = TRUE)
invisible(extract_nested_cat_archives(delivery_zips, cfg$paths$cat_raw))
delivery_zips <- list.files(cfg$paths$cat_raw, pattern = "[.]zip$", recursive = FALSE, full.names = TRUE, ignore.case = TRUE)
invisible(extract_configured_cat_members(delivery_zips, codes, cfg$paths$cat_raw))
cat_paths <- list.files(cfg$paths$cat_raw, pattern = "[.]CAT[.]gz$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
cat_codes <- sub("^([0-9]{5})[UR]_.*$", "\\1", basename(cat_paths))
cat_paths <- cat_paths[cat_codes %in% codes]
cat_codes <- sub("^([0-9]{5})[UR]_.*$", "\\1", basename(cat_paths))
if (length(unique(cat_codes)) != length(codes)) stop("Authenticated CAT delivery does not cover all 179 Madrid municipalities.")

for (i in seq_along(codes)) {
  code <- codes[i]
  destination <- file.path(out_dir, paste0("residential_", code, ".rds"))
  cache_valid <- FALSE
  if (file.exists(destination)) {
    cached <- readRDS(destination)
    cache_valid <- identical(attr(cached, "cat_combined_version"), "urban_rural_v1")
  }
  if (cache_valid) {
    message("[", i, "/", length(codes), "] Cached CAT residential summary ", code)
    next
  }
  path <- cat_paths[cat_codes == code]
  if (!length(path)) stop("Expected CAT files for ", code)
  message("[", i, "/", length(codes), "] Parsing CAT ", code)
  parsed_parts <- lapply(path, function(p) {
    z <- parse_cat_file(p, keep = c("11", "14", "15"))
    domain <- sub("^[0-9]{5}([UR])_.*$", "\\1", basename(p))
    lapply(z, function(tab) { tab[, cat_domain := domain]; tab })
  })
  parsed <- lapply(c("11", "14", "15"), function(type) {
    tab <- rbindlist(lapply(parsed_parts, `[[`, type), use.names = TRUE, fill = TRUE)
    # A small number of rustic-reference parcels are delivered identically in
    # both domains. Retain the urban copy for the whole parcel to prevent any
    # construction or property record from being counted twice.
    urban_refs <- unique(tab[cat_domain == "U", parcel_ref])
    tab[cat_domain == "U" | !parcel_ref %in% urban_refs]
  }) |> stats::setNames(c("11", "14", "15"))
  residential <- derive_residential_parcels(parsed[["14"]], parsed[["15"]], cfg)
  if (nrow(residential)) {
    qualified <- qualify_residential_construction(parsed[["14"]], cfg)
    area_summary <- qualified[!is.na(area_m2) & area_m2 > 0, .(
      cat_total_construction_m2 = sum(area_m2),
      cat_qualifying_residential_m2 = sum(area_m2[is_residential == TRUE])
    ), by = parcel_ref]
    residential <- merge(residential, area_summary, by = "parcel_ref", all.x = TRUE)
    domain_map <- unique(parsed[["14"]][, .(parcel_ref, cat_domain)])
    if (anyDuplicated(domain_map$parcel_ref)) stop("Parcel occurs in both urban and rural CAT domains for ", code)
    residential <- merge(residential, domain_map, by = "parcel_ref", all.x = TRUE)
    residential[, `:=`(
      municipality_code = code,
      residential_area_share = cat_qualifying_residential_m2 / cat_total_construction_m2
    )]
  }
  parcel_summary <- parsed[["11"]][, .(
    parcel_ref, cat_domain, municipality_name, parcel_area_m2, total_built_area_m2,
    above_ground_area_m2, below_ground_area_m2
  )]
  attr(residential, "cat_combined_version") <- "urban_rural_v1"
  attr(parcel_summary, "cat_combined_version") <- "urban_rural_v1"
  saveRDS(residential, destination, compress = "xz")
  saveRDS(parcel_summary, file.path(out_dir, paste0("parcels_", code, ".rds")), compress = "xz")
  rm(parsed, parsed_parts, residential, parcel_summary)
  gc()
}

residential_paths <- list.files(out_dir, pattern = "^residential_[0-9]{5}[.]rds$", full.names = TRUE)
residential <- rbindlist(lapply(residential_paths, readRDS), use.names = TRUE, fill = TRUE)
if (anyDuplicated(residential$parcel_ref)) stop("Regional residential parcel references are not unique.")
saveRDS(residential, file.path(cfg$paths$phase2_interim, "regional_residential_parcels.rds"), compress = "xz")

manifest_rows <- data.table(
  source = "Catastro CAT municipal member",
  municipality_code = cat_codes,
  municipality_name = NA_character_,
  snapshot_date = cfg$phase2$cat_snapshot,
  retrieval_time = iso_now(),
  url = NA_character_,
  local_path = cat_paths,
  bytes = file.info(cat_paths)$size,
  sha256 = vapply(cat_paths, sha256_file, character(1)),
  note = "Authenticated province-wide CAT urban/rural member used for Phase 2"
)
delivery_manifest <- data.table(
  source = "Catastro CAT delivery archive",
  municipality_code = NA_character_, municipality_name = NA_character_,
  snapshot_date = cfg$phase2$cat_snapshot, retrieval_time = iso_now(), url = NA_character_,
  local_path = delivery_zips, bytes = file.info(delivery_zips)$size,
  sha256 = vapply(delivery_zips, sha256_file, character(1)),
  note = "Authenticated original or extracted province-wide urban/rural CAT archive"
)
append_source_manifest(rbindlist(list(manifest_rows, delivery_manifest), fill = TRUE))
cat("Prepared", nrow(residential), "regional residential parcel records across", uniqueN(residential$municipality_code), "municipalities.\n")
