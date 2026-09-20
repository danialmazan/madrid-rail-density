#!/usr/bin/env Rscript
source("R/utils.R")
source("R/station_helpers.R")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(data.table))

cfg <- read_config()
raw_dir <- cfg$paths$rail_phase3_raw
ensure_dir(raw_dir)

curl_download <- function(url, destination) {
  if (file.exists(destination) && file.info(destination)$size > 0) return(invisible(destination))
  partial <- paste0(destination, ".part")
  status <- system2("curl", c("--location", "--fail", "--retry", "3", "--silent", "--show-error", shQuote(url), "--output", shQuote(partial)))
  if (status != 0 || !file.exists(partial) || file.info(partial)$size == 0) stop("Download failed: ", url)
  if (!file.rename(partial, destination)) stop("Could not finalize download: ", destination)
  invisible(destination)
}

station_required <- c("IDESTACION", "DENOMINACION", "CODIGOINTERCAMBIADOR", "CODIGOPROVINCIA", "CODIGOMUNICIPIO", "FECHAFIN", "FECHAACTUAL", "LINEAS")
access_required <- c("IDACCESO", "IDFESTACION", "CODIGOESTACION", "DENOMINACION", "CODIGOPROVINCIA", "CODIGOMUNICIPIO", "FECHAFIN", "FECHAACTUAL")
platform_required <- c("IDANDEN", "IDFESTACION", "CODIGOESTACION", "DENOMINACIONESTACION", "FECHAFIN", "FECHAACTUAL")
manifest <- rbindlist(lapply(cfg$phase3$station_services, function(service) {
  specs <- list(
    list(kind = "stations", suffix = "estaciones", url = service$url, required = station_required),
    list(kind = "entrances", suffix = "accesos", url = service$access_url, required = access_required),
    list(kind = "platforms", suffix = "andenes", url = service$platform_url, required = platform_required)
  )
  rbindlist(lapply(specs, function(spec) {
    destination <- file.path(raw_dir, paste0(service$code, "_", spec$suffix, ".geojson"))
    curl_download(spec$url, destination)
    layer <- st_read(destination, quiet = TRUE, stringsAsFactors = FALSE)
    missing <- setdiff(spec$required, names(layer))
    if (length(missing)) stop(service$code, " ", spec$kind, " source is missing fields: ", paste(missing, collapse = ", "))
    if (!nrow(layer) || any(st_is_empty(layer))) stop(service$code, " ", spec$kind, " source is empty or contains empty geometries.")
    if (is.na(st_crs(layer))) stop(service$code, " ", spec$kind, " source has no CRS.")
    if (st_crs(layer)$epsg != cfg$project$crs) layer <- st_transform(layer, cfg$project$crs)
    dates <- unique(station_trim(layer$FECHAACTUAL))
    data.table(
      source = paste0("CRTM official ArcGIS open data ", service$code, " ", spec$kind),
      municipality_code = NA_character_, municipality_name = NA_character_,
      snapshot_date = cfg$phase3$station_snapshot, source_date = paste(dates, collapse = "|"), retrieval_time = iso_now(),
      url = spec$url, local_path = destination, bytes = file.info(destination)$size,
      sha256 = sha256_file(destination), row_count = nrow(layer), licence_url = cfg$phase3$crtm_license_url,
      note = paste0(spec$kind, " records; source FECHAACTUAL=", paste(dates, collapse = "|"))
    )
  }))
}))
append_source_manifest(manifest)
fwrite(manifest, file.path(raw_dir, "source_manifest.csv"))
cat("Verified", nrow(manifest), "official CRTM station, entrance and platform layers in", raw_dir, "\n")
