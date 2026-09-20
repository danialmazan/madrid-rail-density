#!/usr/bin/env Rscript
source("R/utils.R")
suppressPackageStartupMessages(library(data.table))

cfg <- read_config()
site_dir <- cfg$paths$site
qa_dir <- file.path(cfg$paths$validation, "phase6")
ensure_dir(qa_dir)
json_path <- file.path(cfg$paths$phase6_derived, "phase6_data.json")
payload <- jsonlite::fromJSON(json_path)
html <- paste(readLines(file.path(site_dir, "index.html"), warn = FALSE), collapse = "\n")
js <- paste(readLines(file.path(site_dir, "app.js"), warn = FALSE), collapse = "\n")
css <- paste(readLines(file.path(site_dir, "styles.css"), warn = FALSE), collapse = "\n")
phase5 <- jsonlite::fromJSON(file.path(cfg$paths$validation, "current_phase5_decision.json"))
browser_qa_path <- file.path(qa_dir, "browser_qa.md")
browser_qa_text <- if (file.exists(browser_qa_path)) paste(readLines(browser_qa_path, warn = FALSE), collapse = "\n") else ""
browser_assets <- file.path(site_dir, c("index.html", "styles.css", "app.js", "data.js"))
browser_hashes <- vapply(browser_assets, digest::digest, character(1), algo = "sha256", serialize = FALSE, file = TRUE)
browser_qa_current <- nzchar(browser_qa_text) && all(vapply(browser_hashes, grepl, logical(1), x = browser_qa_text, fixed = TRUE))

required_files <- file.path(site_dir, c(
  "index.html", "styles.css", "app.js", "data.js",
  "downloads/station-catchments-2025.csv", "downloads/station-metadata.csv",
  "downloads/regional-network-coverage-2025.csv", "downloads/network-overlap-2025.csv"
))
station_download <- fread(file.path(site_dir, "downloads", "station-catchments-2025.csv"))
metadata_download <- fread(file.path(site_dir, "downloads", "station-metadata.csv"))
regional_download <- fread(file.path(site_dir, "downloads", "regional-network-coverage-2025.csv"))
overlap_download <- fread(file.path(site_dir, "downloads", "network-overlap-2025.csv"))

i18n_matches <- c(
  regmatches(html, gregexpr('data-i18n="[^"]+"', html))[[1]],
  regmatches(html, gregexpr('data-i18n-html="[^"]+"', html))[[1]],
  regmatches(html, gregexpr('data-i18n-placeholder="[^"]+"', html))[[1]]
)
i18n_keys <- unique(sub('"$', '', sub('^[^=]+="', '', i18n_matches)))
missing_copy <- i18n_keys[!vapply(i18n_keys, function(k) grepl(paste0("[,{]", k, ":"), js), logical(1))]
regional_800 <- payload$regional[payload$regional$scenario == "preferred" & payload$regional$network == "any_urban_rail" & payload$regional$radius_m == 800,]
source_regional_800 <- regional_download[scenario == "preferred" & network == "any_urban_rail" & radius_m == 800]

checks <- data.table(
  check = c(
    "phase5_publication_gate_passed", "required_site_files_exist", "payload_station_count",
    "payload_station_radius_grid_complete", "payload_regional_grid_complete", "payload_fallback_count",
    "download_station_grid_complete", "download_metadata_unique", "download_fallback_flags_retained",
    "regional_800_matches_audited_source", "overlap_partition_reconciles",
    "all_declared_i18n_keys_present", "english_and_spanish_copy_present", "canonical_url_present",
    "straight_line_language_present", "overlap_warning_present", "responsive_styles_present",
    "reduced_motion_supported", "payload_size_under_one_megabyte", "no_external_javascript_dependency",
    "browser_qa_matches_current_assets"
  ),
  passed = c(
    identical(phase5$status, "GO_PHASE5_PUBLICATION_READY_WITH_CAVEATS"), all(file.exists(required_files)),
    nrow(payload$stations) == 357L && uniqueN(payload$stations$station_id) == 357L,
    nrow(payload$station_curves) == 357L * 15L && uniqueN(payload$station_curves, by = c("station_id", "radius_m")) == nrow(payload$station_curves),
    nrow(payload$regional) == 15L * 5L * 2L, sum(payload$stations$fallback) == 40L,
    nrow(station_download) == 357L * 15L && uniqueN(station_download, by = c("station_id", "radius_m")) == nrow(station_download),
    nrow(metadata_download) == 357L && uniqueN(metadata_download$station_id) == 357L,
    sum(metadata_download$surface_stop_fallback) == 40L,
    nrow(regional_800) == 1L && abs(regional_800$population - round(source_regional_800$population)) < 1,
    max(abs(overlap_download$population_a_only + overlap_download$population_b_only + overlap_download$population_both + overlap_download$population_neither - overlap_download$regional_population)) < 1e-5,
    length(missing_copy) == 0L, grepl("en:\\{", js) && grepl("es:\\{", js),
    grepl(cfg$phase6$canonical_url, html, fixed = TRUE),
    grepl("Straight-line", html, fixed = TRUE) && grepl("línea recta", js, fixed = TRUE),
    grepl("must not be summed", html, fixed = TRUE) && grepl("no deben sumarse", js, fixed = TRUE),
    grepl("@media(max-width:680px)", css, fixed = TRUE), grepl("prefers-reduced-motion", css, fixed = TRUE),
    file.info(json_path)$size < 1e6, !grepl("<script[^>]+src=[\"']https?://", html), browser_qa_current
  )
)
fwrite(checks, file.path(qa_dir, "phase6_gate_checks.csv"))
status <- if (all(checks$passed)) "GO_PHASE6_SITE_READY" else "PHASE6_REVIEW_REQUIRED"
metrics <- data.table(
  metric = c("stations", "station_radius_rows", "site_payload_bytes", "download_rows", "fallback_stations", "regional_800_population", "regional_800_share"),
  value = c(nrow(payload$stations), nrow(payload$station_curves), file.info(json_path)$size, nrow(station_download), sum(payload$stations$fallback), regional_800$population, regional_800$share)
)
fwrite(metrics, file.path(qa_dir, "phase6_metrics.csv"))
report <- c(
  "# Phase 6 — website outputs and visualisation", "", paste0("**Decision: ", status, "**"), "",
  "The bilingual static article is generated from the audited Phase 4 and Phase 5 outputs. It uses entrance-first station catchments, retains the official-station-point sensitivity comparison, and provides all 100 m increments from 100 to 1,500 m.", "",
  "## Publication features", "",
  "- Linked regional coverage curve, radius control, station map, station profile and ranking.",
  "- One canonical physical station per observation, with mode filtering and station search.",
  "- Metro/Cercanías marginal and shared coverage based on a non-duplicated population partition.",
  "- Bilingual English/Spanish copy, responsive layouts, keyboard-operable controls and reduced-motion support.",
  "- Downloadable catchments, station metadata with fallback flags, regional coverage and network overlap.", "",
  "## Required caveats retained", "",
  "- Distances are Euclidean straight-line distances, not walking distances.",
  "- Building populations are modelled estimates, not observed households.",
  "- Forty uncovered surface stops use official stop points as explicit fallbacks.",
  "- Individual station catchments overlap and are not summed for regional coverage.",
  "- INE 2025 population and 2026 Catastro/CRTM snapshots are disclosed separately.", "",
  "Browser rendering and interaction are validated separately at desktop and mobile sizes."
)
writeLines(report, file.path(qa_dir, "phase6_report.md"))
jsonlite::write_json(list(
  status = status, generated_at = iso_now(), canonical_url = cfg$phase6$canonical_url,
  checks = lapply(seq_len(nrow(checks)), function(i) list(check = checks$check[i], passed = checks$passed[i])),
  metrics = stats::setNames(as.list(metrics$value), metrics$metric), browser_qa = if (browser_qa_current) "passed" else "pending"
), file.path(cfg$paths$validation, "current_phase6_decision.json"), auto_unbox = TRUE, pretty = TRUE)
cat("Phase 6 validation decision:", status, "\n")
print(checks)
if (!all(checks$passed)) stop("Phase 6 validation gates failed: ", paste(checks[passed == FALSE, check], collapse = ", "))
