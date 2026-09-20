#!/usr/bin/env Rscript
source("R/utils.R")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(data.table))

cfg <- read_config()
raw_dir <- cfg$paths$ine_2025_raw
extract_dir <- file.path(cfg$paths$phase2_interim, "ine_2025_extracted")
ensure_dir(extract_dir)

zip_path <- file.path(raw_dir, "seccionado_2025.zip")
expected_shp <- list.files(extract_dir, pattern = "^SECC_CE_20250101[.]shp$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
if (!length(expected_shp)) {
  # Extracting the complete archive avoids matching its non-UTF-8 top-level directory name.
  status <- system2("unzip", c("-joq", shQuote(zip_path), "-d", shQuote(extract_dir)))
  if (status != 0) stop("System unzip failed for the INE section archive.")
  expected_shp <- list.files(extract_dir, pattern = "^SECC_CE_20250101[.]shp$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
}
if (length(expected_shp) != 1L) stop("Could not locate the 2025 section shapefile after extraction.")

sections <- st_read(expected_shp, quiet = TRUE, stringsAsFactors = FALSE)
required <- c("CUSEC", "CUMUN", "CPRO", "NMUN")
if (length(missing <- setdiff(required, names(sections)))) stop("Missing INE geography fields: ", paste(missing, collapse = ", "))
sections <- sections[trimws(as.character(sections$CPRO)) == "28", ]
if (st_crs(sections)$epsg != cfg$project$crs) sections <- st_transform(sections, cfg$project$crs)
sections$CUSEC <- trimws(as.character(sections$CUSEC))
sections$CUMUN <- trimws(as.character(sections$CUMUN))
sections$NMUN <- trimws(as.character(sections$NMUN))
if (anyDuplicated(sections$CUSEC)) stop("2025 Madrid CUSEC values are not unique.")

normalize_name <- function(x) {
  x <- iconv(toupper(trimws(x)), from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Z0-9]", "", x)
  x
}

series <- jsonlite::fromJSON(file.path(raw_dir, "madrid_population_table_69210.json"), simplifyVector = FALSE)
is_total <- vapply(series, function(z) {
  name <- trimws(z$Nombre)
  if (!grepl(" sección [0-9]{5}[.]", name)) return(FALSE)
  suffix <- sub("^.* sección [0-9]{5}[.] *", "", name)
  fields <- trimws(strsplit(suffix, ".", fixed = TRUE)[[1]])
  fields <- fields[nzchar(fields)]
  length(fields) == 4L && identical(sort(fields[1:3]), sort(c("Total", "Total", "Todas las edades"))) && fields[4] == "Dato base"
}, logical(1))
series <- series[is_total]
population <- rbindlist(lapply(series, function(z) {
  data <- z$Data
  value_2025 <- Filter(function(v) identical(as.integer(v$Anyo), as.integer(cfg$phase2$population_year)), data)
  if (length(value_2025) != 1L) return(NULL)
  name <- trimws(z$Nombre)
  data.table(
    municipality_name_series = sub(" sección [0-9]{5}[.].*$", "", name),
    section_suffix = sub("^.* sección ([0-9]{5})[.].*$", "\\1", name),
    population = as.numeric(value_2025[[1]]$Valor),
    secret = isTRUE(value_2025[[1]]$Secreto),
    series_code = z$COD
  )
}), fill = TRUE)
if (!nrow(population)) stop("No 2025 total-population section series found.")
population[, municipality_key := normalize_name(municipality_name_series)]

lookup <- data.table(
  CUSEC = sections$CUSEC,
  CUMUN = sections$CUMUN,
  municipality_name_geo = sections$NMUN,
  municipality_key = normalize_name(sections$NMUN),
  section_suffix = substr(sections$CUSEC, 6, 10)
)

# INE uses article-first names in one source and article-last names in the other.
canonical_key <- function(x) {
  x <- sub("^(LA|EL|LOS|LAS)", "", x)
  sub("(LA|EL|LOS|LAS)$", "", x)
}
population[, canonical_municipality_key := canonical_key(municipality_key)]
lookup[, canonical_municipality_key := canonical_key(municipality_key)]
joined <- merge(
  lookup, population,
  by = c("canonical_municipality_key", "section_suffix"),
  all.x = TRUE, allow.cartesian = FALSE
)
if (nrow(joined) != nrow(lookup)) stop("Population join changed the number of section rows.")
if (anyNA(joined$population)) {
  fwrite(joined[is.na(population)], file.path(cfg$paths$phase2_interim, "unmatched_ine_sections.csv"))
  stop(sum(is.na(joined$population)), " Madrid sections did not join to the 2025 population table.")
}
if (any(joined$secret)) stop("Unexpected secret/suppressed total-population section values.")

joined <- joined[match(sections$CUSEC, CUSEC)]
sections$population <- joined$population
sections$population_series_code <- joined$series_code
sections$population_reference_date <- cfg$phase2$population_reference_date
sections <- sections[, c("CUSEC", "CUMUN", "NMUN", "population", "population_series_code", "population_reference_date")]
saveRDS(sections, file.path(cfg$paths$phase2_interim, "ine_sections_2025.rds"), compress = "xz")
fwrite(st_drop_geometry(sections), file.path(cfg$paths$phase2_interim, "ine_sections_2025.csv"))
cat("Prepared", nrow(sections), "Madrid census sections with", format(sum(sections$population), big.mark = ","), "residents at", cfg$phase2$population_reference_date, ".\n")
