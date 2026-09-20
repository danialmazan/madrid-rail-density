#!/usr/bin/env Rscript
source("R/utils.R")
cfg <- read_config()
raw_dir <- file.path("data", "raw", "ine", "2021")
ensure_dir(raw_dir)

get_file <- function(url, destination) {
  if (file.exists(destination) && file.info(destination)$size > 0) return(invisible(destination))
  response <- httr::GET(url, httr::write_disk(destination, overwrite = TRUE), httr::timeout(180))
  httr::stop_for_status(response)
  invisible(destination)
}

metadata_urls <- c(
  SDC21_tablas_disponibles.xlsx = "https://www.ine.es/censos2021/SDC21_tablas_disponibles.xlsx",
  SDC21_variables_disponibles.xlsx = "https://www.ine.es/censos2021/SDC21_variables_disponibles.xlsx",
  SDC21_unidades_medida_disponibles.xlsx = "https://www.ine.es/censos2021/SDC21_unidades_medida_disponibles.xlsx"
)
for (name in names(metadata_urls)) get_file(metadata_urls[[name]], file.path(raw_dir, name))

section_housing_path <- file.path(raw_dir, "viviendas_secciones.json")
if (!file.exists(section_housing_path) || file.info(section_housing_path)$size == 0) {
  response <- httr::POST(
    "https://www.ine.es/Censo2021/api",
    body = list(idioma = "ES", metrica = list("SVIVIENDAS"), tabla = "viv.fam", variables = list("ID_RESIDENCIA_N5")),
    encode = "json",
    httr::accept_json(),
    httr::timeout(300)
  )
  httr::stop_for_status(response)
  writeLines(httr::content(response, as = "text", encoding = "UTF-8"), section_housing_path, useBytes = TRUE)
}

ine_codes <- vapply(cfg$catastro$municipalities, function(x) as.character(x$ine_code), character(1))
filter <- paste0("CUMUN IN (", paste(sprintf("'%s'", ine_codes), collapse = ","), ")")
sections_url <- paste0(
  "https://www.ine.es/geoserver/ogc/features/v1/collections/WMS_INE_SECCIONES_G01%3ASecciones_2021/items?",
  "f=application%2Fgeo%2Bjson&limit=10000&filter=", utils::URLencode(filter, reserved = TRUE)
)
sections_path <- file.path(raw_dir, "secciones_2021_prototype.geojson")
get_file(sections_url, sections_path)

paths <- c(file.path(raw_dir, names(metadata_urls)), section_housing_path, sections_path)
manifest_path <- file.path("data", "raw", "source_manifest.csv")
rows <- data.table::data.table(
  source = "INE 2021 Census validation",
  municipality_code = "prototype",
  snapshot_date = "2021",
  retrieval_time = iso_now(),
  local_path = paths,
  bytes = file.info(paths)$size,
  sha256 = vapply(paths, sha256_file, character(1)),
  note = "Official INE metadata, dwelling control, or census-section geography"
)
append_source_manifest(rows, manifest_path)
