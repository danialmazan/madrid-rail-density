#!/usr/bin/env Rscript
source("R/utils.R")
source("R/catastro_io.R")
cfg <- read_config()
ensure_dir(cfg$paths$interim)

archives <- list.files(cfg$paths$inspire_raw, pattern = "\\.zip$", full.names = TRUE)
if (!length(archives)) stop("No INSPIRE archives found. Run scripts/01_download_inspire.R first.")

for (archive in archives) {
  code <- sub(".*BU\\.([0-9]{5})\\.zip$", "\\1", archive)
  message("Reading INSPIRE Building layer for ", code)
  buildings <- standardize_buildings(read_inspire_layer(archive, "building"), code)
  saveRDS(buildings, file.path(cfg$paths$interim, paste0("buildings_", code, ".rds")), compress = FALSE)
}

municipality_codes <- vapply(cfg$catastro$municipalities, function(x) as.character(x$code), character(1))
delivery_zips <- list.files(cfg$paths$cat_raw, pattern = "\\.zip$", recursive = FALSE, full.names = TRUE, ignore.case = TRUE)
extract_configured_cat_members(delivery_zips, municipality_codes, cfg$paths$cat_raw)

cat_paths <- list.files(
  cfg$paths$cat_raw,
  pattern = "(\\.CAT\\.gz|\\.cat|\\.txt)$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)
cat_paths <- cat_paths[vapply(basename(cat_paths), function(x) {
  any(startsWith(toupper(x), paste0(municipality_codes, "U_")))
}, logical(1))]
if (!length(cat_paths)) {
  message("No authenticated CAT inputs found; INSPIRE parsing is complete.")
  quit(status = 0)
}

all_records <- stats::setNames(vector("list", 4), c("11", "13", "14", "15"))
for (path in cat_paths) {
  message("Parsing CAT: ", path)
  parsed <- parse_cat_file(path)
  for (type in names(parsed)) all_records[[type]][[length(all_records[[type]]) + 1L]] <- parsed[[type]]
}

manifest_path <- file.path("data", "raw", "source_manifest.csv")
manifest_rows <- data.table::data.table(
  source = c(rep("Catastro CAT municipal member", length(cat_paths)), rep("Catastro CAT delivery archive", length(delivery_zips))),
  municipality_code = c(sub("^([0-9]{5})U_.*$", "\\1", basename(cat_paths)), rep("28", length(delivery_zips))),
  snapshot_date = "2026-08-14",
  retrieval_time = iso_now(),
  local_path = c(cat_paths, delivery_zips),
  bytes = c(file.info(cat_paths)$size, file.info(delivery_zips)$size),
  sha256 = vapply(c(cat_paths, delivery_zips), sha256_file, character(1)),
  note = c(rep("Authenticated CAT urban file extracted unchanged from provincial delivery", length(cat_paths)), rep("Authenticated portal/provincial delivery retained unchanged", length(delivery_zips)))
)
append_source_manifest(manifest_rows, manifest_path)
for (type in names(all_records)) {
  combined <- data.table::rbindlist(all_records[[type]], use.names = TRUE, fill = TRUE)
  saveRDS(combined, file.path(cfg$paths$interim, paste0("cat_type", type, ".rds")), compress = "xz")
  if (requireNamespace("arrow", quietly = TRUE)) arrow::write_parquet(combined, file.path(cfg$paths$interim, paste0("cat_type", type, ".parquet")), compression = "zstd")
}
