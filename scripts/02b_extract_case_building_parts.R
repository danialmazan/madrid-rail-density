#!/usr/bin/env Rscript
source("R/utils.R")
source("R/catastro_io.R")
suppressPackageStartupMessages(library(sf))
cfg <- read_config()

case_refs <- vapply(cfg$catastro$case_parcels, `[[`, character(1), "parcel_ref")
archives <- list.files(cfg$paths$inspire_raw, pattern = "\\.zip$", full.names = TRUE)

for (municipality in cfg$catastro$municipalities) {
  code <- as.character(municipality$code)
  building_path <- file.path(cfg$paths$interim, paste0("buildings_", code, ".rds"))
  if (!file.exists(building_path)) next
  buildings <- readRDS(building_path)
  cases <- buildings[buildings$parcel_ref %in% case_refs, ]
  if (!nrow(cases)) next
  archive <- archives[endsWith(archives, paste0("BU.", code, ".zip"))]
  if (length(archive) != 1L) stop("Expected one INSPIRE archive for ", code)
  filter <- sf::st_as_text(sf::st_union(sf::st_buffer(sf::st_geometry(cases), 1)))
  message("Extracting BuildingParts for ", nrow(cases), " configured cases in ", code)
  parts <- sf::st_read(
    gml_path_in_zip(archive, "buildingpart"),
    wkt_filter = filter,
    quiet = TRUE,
    stringsAsFactors = FALSE
  )
  if (sf::st_crs(parts)$epsg != 25830) parts <- sf::st_transform(parts, 25830)
  saveRDS(parts, file.path(cfg$paths$interim, paste0("case_building_parts_", code, ".rds")), compress = FALSE)
}
