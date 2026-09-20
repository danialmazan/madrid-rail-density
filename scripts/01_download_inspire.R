#!/usr/bin/env Rscript
source("R/utils.R")
cfg <- read_config()
raw_dir <- cfg$paths$inspire_raw
ensure_dir(raw_dir)

base <- "https://www.catastro.hacienda.gob.es/INSPIRE/Buildings/28"
manifest_rows <- list()
for (municipality in cfg$catastro$municipalities) {
  code <- municipality$code
  url_name <- utils::URLencode(municipality$url_name, reserved = TRUE)
  url <- sprintf("%s/%s-%s/A.ES.SDGC.BU.%s.zip", base, code, url_name, code)
  destination <- file.path(raw_dir, sprintf("A.ES.SDGC.BU.%s.zip", code))
  if (!file.exists(destination) || file.info(destination)$size == 0) {
    args <- c("--location", "--fail", "--silent", "--show-error")
    if (isTRUE(cfg$catastro$insecure_ssl)) args <- c(args, "--insecure")
    status <- system2("curl", c(args, shQuote(url), "--output", shQuote(destination)))
    if (status != 0) stop("Download failed for ", code)
  }
  listing <- utils::unzip(destination, list = TRUE)
  required <- c("building.gml", "buildingpart.gml", "otherconstruction.gml")
  if (!all(vapply(required, function(suffix) any(endsWith(tolower(listing$Name), suffix)), logical(1)))) stop("Unexpected INSPIRE archive contents: ", destination)
  manifest_rows[[length(manifest_rows) + 1L]] <- data.table::data.table(
    source = "DGC Catastro INSPIRE Buildings ATOM",
    municipality_code = code,
    municipality_name = municipality$name,
    snapshot_date = cfg$catastro$atom_snapshot,
    retrieval_time = iso_now(),
    url = url,
    local_path = destination,
    bytes = file.info(destination)$size,
    sha256 = sha256_file(destination)
  )
}
manifest <- data.table::rbindlist(manifest_rows)
ensure_dir("data/raw")
append_source_manifest(manifest)
cat("Downloaded/verified", nrow(manifest), "INSPIRE archives.\n")
