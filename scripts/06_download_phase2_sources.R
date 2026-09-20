#!/usr/bin/env Rscript
source("R/utils.R")
suppressPackageStartupMessages(library(xml2))
suppressPackageStartupMessages(library(data.table))

cfg <- read_config()
ensure_dir(cfg$paths$inspire_raw)
ensure_dir(cfg$paths$ine_2025_raw)

curl_download <- function(url, destination, insecure = FALSE) {
  if (file.exists(destination) && file.info(destination)$size > 0) return(invisible(destination))
  partial <- paste0(destination, ".part")
  args <- c("--location", "--fail", "--retry", "3", "--silent", "--show-error")
  if (insecure) args <- c(args, "--insecure")
  status <- system2("curl", c(args, shQuote(url), "--output", shQuote(partial)))
  if (status != 0 || !file.exists(partial) || file.info(partial)$size == 0) stop("Download failed: ", url)
  if (!file.rename(partial, destination)) stop("Could not finalize download: ", destination)
  invisible(destination)
}

feed_path <- file.path(cfg$paths$inspire_raw, "ES.SDGC.bu.atom_28.xml")
curl_download(cfg$catastro$atom_feed, feed_path, insecure = isTRUE(cfg$catastro$insecure_ssl))
feed <- read_xml(feed_path, encoding = "ISO-8859-1")
ns <- xml_ns(feed)
entries <- xml_find_all(feed, ".//d1:entry", ns)
urls <- xml_attr(xml_find_first(entries, ".//d1:link[@rel='enclosure']", ns), "href")
urls <- vapply(urls, utils::URLencode, character(1), reserved = FALSE)
titles <- trimws(xml_text(xml_find_first(entries, "./d1:title", ns)))
updated <- substr(xml_text(xml_find_first(entries, "./d1:updated", ns)), 1, 10)
codes <- sub("^.*A[.]ES[.]SDGC[.]BU[.]([0-9]{5})[.]zip$", "\\1", urls)
if (length(urls) != 179L || any(!grepl("^[0-9]{5}$", codes))) stop("Unexpected Madrid-province Catastro ATOM feed.")
destinations <- file.path(cfg$paths$inspire_raw, basename(urls))

download_one <- function(i) {
  curl_download(urls[i], destinations[i], insecure = isTRUE(cfg$catastro$insecure_ssl))
  listing <- utils::unzip(destinations[i], list = TRUE)$Name
  if (!any(grepl("[.]building[.]gml$", listing, ignore.case = TRUE)) || !any(grepl("[.]buildingpart[.]gml$", listing, ignore.case = TRUE))) {
    stop("Invalid INSPIRE archive: ", destinations[i])
  }
  TRUE
}
workers <- min(as.integer(cfg$phase2$download_workers), parallel::detectCores(logical = FALSE), length(urls))
if (.Platform$OS.type == "unix" && workers > 1L) {
  result <- parallel::mclapply(seq_along(urls), download_one, mc.cores = workers)
} else {
  result <- lapply(seq_along(urls), download_one)
}
if (!all(vapply(result, isTRUE, logical(1)))) stop("One or more INSPIRE downloads failed.")

inspire_manifest <- data.table(
  source = "DGC Catastro INSPIRE Buildings ATOM",
  municipality_code = codes,
  municipality_name = sub("^[0-9]{5}-(.*) buildings$", "\\1", titles),
  snapshot_date = updated,
  retrieval_time = iso_now(),
  url = urls,
  local_path = destinations,
  bytes = file.info(destinations)$size,
  sha256 = vapply(destinations, sha256_file, character(1)),
  note = "Complete Comunidad de Madrid municipal Building and BuildingPart archive"
)

sections_path <- file.path(cfg$paths$ine_2025_raw, "seccionado_2025.zip")
population_path <- file.path(cfg$paths$ine_2025_raw, "madrid_population_table_69210.json")
curl_download(cfg$phase2$ine_sections_url, sections_path)
curl_download(cfg$phase2$ine_population_url, population_path)
ine_manifest <- data.table(
  source = c("INE 2025 census-section geography", "INE Annual Population Census 2025 section table"),
  municipality_code = NA_character_, municipality_name = NA_character_,
  snapshot_date = cfg$phase2$population_reference_date,
  retrieval_time = iso_now(),
  url = c(cfg$phase2$ine_sections_url, cfg$phase2$ine_population_url),
  local_path = c(sections_path, population_path),
  bytes = file.info(c(sections_path, population_path))$size,
  sha256 = vapply(c(sections_path, population_path), sha256_file, character(1)),
  note = c("Matching-year official section boundaries", "Official resident population at 1 January 2025")
)
append_source_manifest(rbindlist(list(inspire_manifest, ine_manifest), fill = TRUE))
cat("Verified", length(urls), "municipal INSPIRE archives and both INE 2025 inputs.\n")
