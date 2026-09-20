#!/usr/bin/env Rscript
source("R/utils.R")
cfg <- read_config()
ensure_dir(cfg$paths$rail_raw)

rows <- list()
for (service in cfg$phase1b$station_services) {
  destination <- file.path(cfg$paths$rail_raw, paste0(service$code, "_estaciones.geojson"))
  if (!file.exists(destination) || file.info(destination)$size == 0) {
    status <- system2(
      "curl",
      c("--location", "--fail", "--silent", "--show-error", shQuote(service$url), "--output", shQuote(destination))
    )
    if (status != 0) stop("CRTM station download failed for ", service$code)
  }
  layer <- sf::st_read(destination, quiet = TRUE)
  if (!nrow(layer) || !all(c("IDESTACION", "DENOMINACION", "MODO") %in% names(layer))) {
    stop("Unexpected CRTM station schema: ", destination)
  }
  rows[[length(rows) + 1L]] <- data.table::data.table(
    source = "CRTM official station Feature Service",
    municipality_code = NA_character_,
    municipality_name = NA_character_,
    snapshot_date = cfg$phase1b$station_snapshot,
    retrieval_time = iso_now(),
    url = service$url,
    local_path = destination,
    bytes = file.info(destination)$size,
    sha256 = sha256_file(destination),
    note = paste("Preliminary Phase 1b station points; mode", service$mode)
  )
}
append_source_manifest(data.table::rbindlist(rows))
cat("Downloaded/verified", length(rows), "official CRTM station layers.\n")
