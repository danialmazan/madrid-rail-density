#!/usr/bin/env Rscript
source("R/utils.R")
cfg <- read_config()
decision_path <- file.path(cfg$paths$validation, "current_phase7_decision.json")
if (!file.exists(decision_path)) stop("Phase 7 validation decision is missing.")
decision <- jsonlite::fromJSON(decision_path)
if (!identical(decision$status, "GO_PHASE7_LOCAL_STATIC_BUILD")) stop("Refusing promotion: Phase 7 gates are not green.")
source_site <- cfg$paths$phase7_site
target_site <- cfg$paths$site
archive_site <- "site_phase6_archive"
if (!dir.exists(source_site) || !file.exists(file.path(source_site, "index.html"))) stop("Phase 7 build is incomplete.")
if (dir.exists(target_site) && !dir.exists(archive_site)) {
  dir.create(archive_site, recursive = TRUE)
  files <- list.files(target_site, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  if (length(files) && !all(file.copy(files, archive_site, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE))) stop("Could not preserve Phase 6 site archive.")
}
staging <- "site_phase7_promoting"
if (dir.exists(staging)) unlink(staging, recursive = TRUE)
dir.create(staging, recursive = TRUE)
files <- list.files(source_site, all.files = TRUE, no.. = TRUE, full.names = TRUE)
if (!all(file.copy(files, staging, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE))) stop("Could not stage the Phase 7 site.")
if (dir.exists(target_site)) unlink(target_site, recursive = TRUE)
if (!file.rename(staging, target_site)) stop("Could not atomically promote Phase 7 to site/.")
cat("Promoted validated Phase 7 build to site/; prior Phase 6 retained at", archive_site, "\n")
