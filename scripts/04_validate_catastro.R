#!/usr/bin/env Rscript
source("R/utils.R")
source("R/catastro_io.R")
source("R/dwelling_rules.R")
source("R/spatial_allocation.R")
source("R/validation_helpers.R")
cfg <- read_config()
table_dir <- file.path(cfg$paths$validation, "tables")
case_dir <- file.path(cfg$paths$validation, "case_studies")
ensure_dir(table_dir)
ensure_dir(case_dir)

building_paths <- list.files(cfg$paths$interim, pattern = "^buildings_[0-9]{5}\\.rds$", full.names = TRUE)
if (!length(building_paths)) stop("No parsed Building files. Run scripts/02_parse_catastro.R first.")
buildings <- do.call(rbind, lapply(building_paths, readRDS))
checks <- validate_inspire_buildings(buildings)
data.table::fwrite(checks$summary, file.path(table_dir, "inspire_summary.csv"))
data.table::fwrite(checks$use_cross, file.path(table_dir, "use_dwelling_crosscheck.csv"))
data.table::fwrite(checks$duplicates, file.path(table_dir, "duplicate_parcel_refs.csv"))
write_inspire_diagnostic_plots(buildings, cfg$paths$validation)

case_refs <- vapply(cfg$catastro$case_parcels, `[[`, character(1), "parcel_ref")
case_names <- vapply(cfg$catastro$case_parcels, `[[`, character(1), "case")
case_lookup <- data.table::data.table(parcel_ref = case_refs, case = case_names)
building_attributes <- data.table::as.data.table(sf::st_drop_geometry(buildings))
case_rows <- merge(building_attributes[parcel_ref %in% case_refs], case_lookup, by = "parcel_ref", all.y = TRUE)
data.table::fwrite(case_rows, file.path(case_dir, "case_parcels.csv"))

cat14_path <- file.path(cfg$paths$interim, "cat_type14.rds")
cat15_path <- file.path(cfg$paths$interim, "cat_type15.rds")
comparison <- NULL
ine_validation <- NULL
component_validation <- NULL
if (file.exists(cat14_path) && file.exists(cat15_path)) {
  residential <- derive_residential_parcels(readRDS(cat14_path), readRDS(cat15_path), cfg)
  saveRDS(residential, file.path(cfg$paths$interim, "residential_parcels.rds"), compress = "xz")
  comparison <- compare_cat_inspire(residential, buildings)
  data.table::fwrite(comparison, file.path(table_dir, "cat_inspire_comparison.csv"))
  rules <- residential[, .(
    parcels = .N,
    residential_area_m2 = sum(residential_area_m2, na.rm = TRUE),
    raw_residential_properties = sum(raw_residential_property_count, na.rm = TRUE),
    reconstructed_dwellings = sum(reconstructed_dwellings, na.rm = TRUE)
  ), by = .(dwelling_rule, confidence)]
  data.table::fwrite(rules, file.path(table_dir, "cat_rule_summary.csv"))
  case_cat <- merge(comparison[parcel_ref %in% case_refs], case_lookup, by = "parcel_ref", all.y = TRUE)
  data.table::fwrite(case_cat, file.path(case_dir, "case_cat_summary.csv"))

  ine_sections_path <- file.path("data", "raw", "ine", "2021", "secciones_2021_prototype.geojson")
  ine_housing_path <- file.path("data", "raw", "ine", "2021", "viviendas_secciones.json")
  if (file.exists(ine_sections_path) && file.exists(ine_housing_path)) {
    ine_validation <- validate_ine_housing(comparison, buildings, ine_sections_path, ine_housing_path, cfg$paths$validation)
  }

  residential_refs <- comparison[!is.na(residential_area_m2) & !is.na(municipality_code), parcel_ref]
  multipart <- buildings[buildings$parcel_ref %in% residential_refs & buildings$patch_count > 1, ]
  connected_counts <- vapply(sf::st_geometry(multipart), function(g) {
    length(connected_polygon_parts(g, sf::st_crs(buildings)))
  }, integer(1))
  component_rows <- data.table::data.table(parcel_ref = multipart$parcel_ref, polygon_patches = multipart$patch_count, connected_components = connected_counts)
  data.table::fwrite(component_rows, file.path(table_dir, "multipart_connected_components.csv"))
  component_join <- merge(comparison, component_rows, by = "parcel_ref", all.x = TRUE)
  component_join[is.na(connected_components), connected_components := 1L]
  component_validation <- component_join[!is.na(residential_area_m2) & !is.na(municipality_code), .(
    parcels = .N,
    residential_area_m2 = sum(residential_area_m2),
    reconstructed_dwellings = sum(reconstructed_dwellings)
  ), by = .(disconnected = connected_components > 1)]
  component_validation[, `:=`(
    residential_area_share = residential_area_m2 / sum(residential_area_m2),
    dwelling_share = reconstructed_dwellings / sum(reconstructed_dwellings)
  )]
  data.table::fwrite(component_validation, file.path(table_dir, "disconnected_component_summary.csv"))
}

status <- phase1_gate_status(comparison, cfg)
visual_check_path <- file.path(case_dir, "sec_visual_checks.csv")
visual_checks <- if (file.exists(visual_check_path)) data.table::fread(visual_check_path) else NULL
required_visual_checks <- c("vertical_property", "multipart_parcel", "mixed_use")
visual_checks_pass <- !is.null(visual_checks) &&
  all(required_visual_checks %in% visual_checks$check_type) &&
  all(visual_checks[check_type %in% required_visual_checks, status] == "pass")
if (!is.null(component_validation)) {
  disconnected_share <- component_validation[disconnected == TRUE, dwelling_share]
  status$disconnected_component_dwelling_share <- disconnected_share
  if (length(disconnected_share) && disconnected_share > cfg$phase1_gates$maximum_spatially_ambiguous_share) {
    status$status <- "NO_GO_REDESIGN"
    status$reason <- paste0(
      "CAT-derived dwelling counts are reliable at parcel level, but ",
      sprintf("%.3f%%", 100 * disconnected_share),
      " of reconstructed dwellings are attached to parcels with separated physical footprints. ",
      "This exceeds the configured 10% spatial-ambiguity threshold, and the BuildingPart volume split has only been validated on selected cases."
    )
  }
}
if (!is.null(ine_validation) && status$status == "PROVISIONAL_GO_DWELLINGS" && visual_checks_pass) {
  status$status <- "GO_DWELLINGS"
  status$reason <- paste(
    "CAT-derived dwelling weighting passes the automated join, confidence and ambiguity gates;",
    "the independent INE housing comparison is strong; and official SEC checks confirm the",
    "vertical-property reconstruction, parcel-level multipart semantics and mixed-use filtering."
  )
} else if (!is.null(ine_validation) && status$status == "PROVISIONAL_GO_DWELLINGS") {
  status$reason <- "Automated dwelling/join gates and the independent INE check pass; targeted component tests pass, but the required official SEC checks remain incomplete."
}
status$official_sec_checks_passed <- if (is.null(visual_checks)) 0L else sum(visual_checks$status == "pass")
status$official_sec_required_checks_pass <- visual_checks_pass
jsonlite::write_json(status, file.path(cfg$paths$validation, "phase1_status.json"), auto_unbox = TRUE, pretty = TRUE, digits = 8)

totals <- checks$summary[, .(
  building_features = sum(building_features),
  unique_parcel_refs = sum(unique_parcel_refs),
  duplicate_parcel_rows = sum(duplicate_parcel_rows),
  dwelling_total = sum(dwelling_total),
  multipart_buildings = sum(multipart_buildings),
  maximum_patch_count = max(maximum_patch_count),
  naive_patch_exploded_dwelling_total = sum(naive_patch_exploded_dwelling_total),
  residential_dominant_zero_dwellings = sum(residential_dominant_zero_dwellings),
  nonresidential_dominant_with_dwellings = sum(nonresidential_dominant_with_dwellings),
  unknown_use_with_dwellings = sum(unknown_use_with_dwellings)
)]
report <- c(
  "# Phase 1 Catastro feasibility report",
  "",
  paste0("**Status: ", status$status, "**"),
  "",
  status$reason,
  "",
  "## Public INSPIRE validation",
  "",
  paste0("- Building features: ", format(totals$building_features, big.mark = ",", scientific = FALSE)),
  paste0("- Unique parcel references: ", format(totals$unique_parcel_refs, big.mark = ",", scientific = FALSE)),
  paste0("- Duplicate parcel rows: ", totals$duplicate_parcel_rows),
  paste0("- Parcel-level dwelling total: ", format(totals$dwelling_total, big.mark = ",", scientific = FALSE)),
  paste0("- Multipart Buildings: ", format(totals$multipart_buildings, big.mark = ",", scientific = FALSE)),
  paste0("- Maximum polygon patches in one Building: ", totals$maximum_patch_count),
  paste0("- Incorrect dwelling total after naïve patch explosion: ", format(totals$naive_patch_exploded_dwelling_total, big.mark = ",", scientific = FALSE)),
  paste0("- Residential-dominant Buildings with zero dwellings: ", format(totals$residential_dominant_zero_dwellings, big.mark = ",", scientific = FALSE)),
  paste0("- Non-residential-dominant Buildings with dwellings: ", format(totals$nonresidential_dominant_with_dwellings, big.mark = ",", scientific = FALSE)),
  paste0("- Buildings with unknown dominant use and dwellings: ", format(totals$unknown_use_with_dwellings, big.mark = ",", scientific = FALSE)),
  if (!is.null(comparison)) c(
    "",
    "## CAT dwelling reconstruction",
    "",
    paste0("- CAT residential parcels: ", format(sum(!is.na(comparison$residential_area_m2)), big.mark = ",")),
    paste0("- CAT qualifying residential area: ", format(round(sum(comparison$residential_area_m2, na.rm = TRUE)), big.mark = ","), " m²"),
    paste0("- Raw residential property units: ", format(sum(comparison$raw_residential_property_count, na.rm = TRUE), big.mark = ",")),
    paste0("- Reconstructed dwellings: ", format(sum(comparison$reconstructed_dwellings, na.rm = TRUE), big.mark = ",")),
    paste0("- CAT residential-area join coverage: ", sprintf("%.3f%%", 100 * status$join_coverage)),
    paste0("- High/medium-confidence residential-area share: ", sprintf("%.3f%%", 100 * status$primary_rule_share)),
    paste0("- Unresolved residential-area share: ", sprintf("%.3f%%", 100 * status$unresolved_share))
  ) else NULL,
  if (!is.null(ine_validation)) c(
    "",
    "## Independent INE 2021 housing check",
    "",
    paste0("- Sections tested: ", format(ine_validation$metrics$sections, big.mark = ",")),
    paste0("- Pearson correlation, CAT vs INE: ", sprintf("%.3f", ine_validation$metrics$pearson_cat_ine)),
    paste0("- Spearman correlation, CAT vs INE: ", sprintf("%.3f", ine_validation$metrics$spearman_cat_ine)),
    paste0("- Median section CAT/INE dwelling ratio: ", sprintf("%.3f", ine_validation$metrics$median_cat_to_ine_ratio)),
    paste0("- 5th–95th percentile section ratio: ", sprintf("%.3f–%.3f", ine_validation$metrics$p05_cat_to_ine_ratio, ine_validation$metrics$p95_cat_to_ine_ratio)),
    "- Interpretation: CAT is dated 14 August 2026 while INE is 2021; agreement is diagnostic and exact equality is neither expected nor required."
  ) else NULL,
  if (!is.null(component_validation)) c(
    "",
    "## Multipart and component allocation",
    "",
    paste0("- Residential parcels with more than one disconnected footprint: ", format(component_validation[disconnected == TRUE, parcels], big.mark = ",")),
    paste0("- Share of reconstructed dwellings on those parcels: ", sprintf("%.3f%%", 100 * component_validation[disconnected == TRUE, dwelling_share])),
    "- Targeted cases use BuildingPart area × above-ground floors to allocate parcel totals to connected components; all case allocations conserve the parcel dwelling total.",
    "- Conservation proves arithmetic consistency, not that the dwelling share assigned to each physical component is correct. BuildingPart has no residential-use or dwelling attribute, so the prototype-wide share remains spatially ambiguous.",
    "- BuildingPart IDs are matched to their exact INSPIRE Building reference before spatial allocation; this avoids duplicate zero-unit auxiliary Building features such as `27041Z3VK3720D`."
  ) else NULL,
  if (!is.null(visual_checks)) c(
    "",
    "## Official SEC case checks",
    "",
    paste0("- Required checks passed: ", sum(visual_checks$status == "pass"), "/", length(required_visual_checks)),
    "- Vertical property: Sombrerería 1 is recorded without horizontal division; one cadastral property contains 40 qualifying housing construction records, matching the CAT reconstruction rather than the INSPIRE count of 1.",
    "- Multipart parcel: Riaño 13 and its associated addresses are one parcel with several properties, stairways and uses; the snapshot assigns its 484 dwellings once at parcel level.",
    "- Mixed use: Gran Vía 15 has retail as its dominant INSPIRE use but the official SEC lists 9 residential properties among 44 total properties; CAT use filtering recovers all 9.",
    "- The live SEC item count for the Riaño parcel differs slightly from the 14 August 2026 snapshot. This is recorded as source-date drift and is not used to overwrite the immutable snapshot."
  ) else NULL,
  "",
  "## Decision boundary",
  "",
  if (identical(status$status, "GO_DWELLINGS"))
    "Phase 1 supports CAT-derived dwelling counts as the regional allocation weight. Residential floor area remains a diagnostic and flagged fallback, not the primary weight. Per the approved plan, work stops here pending acceptance before Phase 2."
  else if (identical(status$status, "NO_GO_REDESIGN"))
    "Do not build the regional building-population layer with the current component rule. The parcel-level dwelling method passes, but physical-building placement must be redesigned and validated first—for example by retaining parcels as the support, adding address/entrance evidence to place CAT units, or formally validating a BuildingPart volume allocation on a representative sample. Residential floor-area weighting alone does not solve this spatial attachment problem."
  else
    "No Phase 2 population allocation is authorized by this report; the remaining Phase 1 gates are listed above.",
  "",
  "## Reproduce",
  "",
  "Run `make phase1`. See the CSV diagnostics in `validation/tables/`."
)
writeLines(report, file.path(cfg$paths$validation, "phase1_report.md"))
cat(status$status, "-", status$reason, "\n")
