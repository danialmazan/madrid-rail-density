#!/usr/bin/env Rscript
source("R/utils.R")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(data.table))

cfg <- read_config()
qa_dir <- file.path(cfg$paths$validation, "phase2")
ensure_dir(qa_dir)

residential <- readRDS(file.path(cfg$paths$phase2_interim, "regional_residential_parcels.rds"))
buildings <- readRDS(file.path(cfg$paths$phase2_interim, "regional_residential_buildings.rds"))
population <- readRDS(file.path(cfg$paths$phase2_derived, "component_population_2025.rds"))
sections <- readRDS(file.path(cfg$paths$phase2_interim, "ine_sections_2025.rds"))
pop_dt <- as.data.table(st_drop_geometry(population))

weight_source_summary <- pop_dt[support_type == "residential_component", .(
  components = .N, parcels = uniqueN(parcel_ref), dwelling_weight = sum(chosen_weight),
  allocated_population = sum(estimated_population)
), by = weight_source][order(-dwelling_weight)]
fwrite(weight_source_summary, file.path(qa_dir, "weight_source_summary.csv"))
cat_domain_summary <- residential[, .(
  parcels = .N, reconstructed_dwellings = sum(reconstructed_dwellings),
  residential_area_m2 = sum(residential_area_m2)
), by = cat_domain]
fwrite(cat_domain_summary, file.path(qa_dir, "cat_domain_summary.csv"))

joined <- residential$parcel_ref %in% buildings$parcel_ref
join_metrics <- data.table(
  metric = c("parcel_join_share", "dwelling_weight_join_share", "residential_area_join_share"),
  value = c(
    mean(joined),
    sum(residential$reconstructed_dwellings[joined], na.rm = TRUE) / sum(residential$reconstructed_dwellings, na.rm = TRUE),
    sum(residential$residential_area_m2[joined], na.rm = TRUE) / sum(residential$residential_area_m2, na.rm = TRUE)
  )
)
fwrite(join_metrics, file.path(qa_dir, "cat_inspire_join_metrics.csv"))

component_totals <- pop_dt[support_type == "residential_component", .(
  allocated_dwellings = sum(chosen_weight), allocated_residential_area_m2 = sum(component_residential_area_m2),
  target_dwellings = unique(parcel_dwellings), target_residential_area_m2 = unique(parcel_residential_area_m2),
  weight_source = unique(weight_source)
), by = parcel_ref]
component_totals <- merge(component_totals, residential[, .(
  parcel_ref, reconstructed_dwellings, residential_area_m2, confidence, dwelling_rule
)], by = "parcel_ref", all.x = TRUE)
component_totals[, `:=`(
  dwelling_residual = allocated_dwellings - target_dwellings,
  area_residual = allocated_residential_area_m2 - target_residential_area_m2
)]
fwrite(component_totals, file.path(qa_dir, "parcel_component_conservation.csv.gz"))

reconciliation <- fread(file.path(cfg$paths$phase2_derived, "section_reconciliation.csv"))
section_diagnostics <- reconciliation[, .(
  CUSEC, CUMUN, municipality_name, control_population, dwelling_weight, area_weight, components, fallback,
  residents_per_dwelling = fifelse(dwelling_weight > 0, control_population / dwelling_weight, NA_real_),
  residents_per_100m2 = fifelse(area_weight > 0, 100 * control_population / area_weight, NA_real_),
  residual, area_residual
)]
fwrite(section_diagnostics, file.path(qa_dir, "section_diagnostics.csv"))
occupancy_outliers <- section_diagnostics[control_population > 0 & residents_per_dwelling > 5][order(-residents_per_dwelling)]
fwrite(occupancy_outliers, file.path(qa_dir, "high_residents_per_dwelling_sections.csv"))

method_sensitivity <- pop_dt[, .(
  component_key, parcel_ref, CUSEC, support_type, chosen_weight, component_residential_area_m2,
  population_dwelling = estimated_population, population_area = estimated_population_area,
  absolute_difference = abs(estimated_population - estimated_population_area)
)]
method_sensitivity[, relative_difference := absolute_difference / pmax(population_dwelling, population_area, 1e-9)]
fwrite(method_sensitivity, file.path(qa_dir, "dwelling_vs_area_sensitivity.csv.gz"))

# Compare point assignment with a true area split only for components crossing section boundaries.
component_dir <- file.path(cfg$paths$phase2_interim, "components_by_municipality")
polygon_paths <- list.files(component_dir, pattern = "^components_[0-9]{5}[.]rds$", full.names = TRUE)
fragment_rows <- list()
cross_rows <- list()
for (i in seq_along(polygon_paths)) {
  components <- readRDS(polygon_paths[i])
  if (!nrow(components)) next
  bbox_hits <- st_intersects(st_as_sfc(st_bbox(components)), sections)[[1]]
  local_sections <- sections[bbox_hits, ]
  hits <- st_intersects(components, local_sections)
  candidates <- which(lengths(hits) > 1L)
  if (!length(candidates)) next
  for (j in candidates) {
    section_candidates <- hits[[j]]
    pieces <- suppressWarnings(st_intersection(
      st_make_valid(components[j, c("component_key", "chosen_weight")]),
      st_make_valid(local_sections[section_candidates, "CUSEC"])
    ))
    if (!nrow(pieces)) next
    areas <- as.numeric(st_area(pieces))
    keep <- areas > 0.01
    if (sum(keep) <= 1L) next
    pieces <- pieces[keep, ]
    areas <- areas[keep]
    shares <- areas / sum(areas)
    fragment_rows[[length(fragment_rows) + 1L]] <- data.table(
      component_key = components$component_key[j], CUSEC = pieces$CUSEC,
      fragment_area_m2 = areas, area_share = shares,
      fragment_weight = components$chosen_weight[j] * shares
    )
    cross_rows[[length(cross_rows) + 1L]] <- data.table(
      component_key = components$component_key[j], parcel_ref = components$parcel_ref[j],
      municipality_code = components$municipality_code[j], sections_crossed = nrow(pieces),
      component_area_m2 = sum(areas), chosen_weight = components$chosen_weight[j]
    )
  }
  if (i %% 20L == 0L) message("Boundary scan: ", i, "/", length(polygon_paths), " municipalities")
  rm(components, local_sections, hits)
  gc()
}
fragments <- rbindlist(fragment_rows, fill = TRUE)
crossing <- rbindlist(cross_rows, fill = TRUE)
if (!nrow(fragments)) {
  fragments <- data.table(component_key = character(), CUSEC = character(), fragment_area_m2 = numeric(), area_share = numeric(), fragment_weight = numeric())
  crossing <- data.table(component_key = character(), parcel_ref = character(), municipality_code = character(), sections_crossed = integer(), component_area_m2 = numeric(), chosen_weight = numeric())
}
fwrite(fragments, file.path(qa_dir, "boundary_split_fragments.csv.gz"))
fwrite(crossing, file.path(qa_dir, "boundary_crossing_components.csv"))

base <- pop_dt[, .(component_key, point_CUSEC = CUSEC, weight = chosen_weight, point_population = estimated_population)]
cross_keys <- unique(fragments$component_key)
point_removed <- base[component_key %in% cross_keys, .(removed_weight = sum(weight)), by = point_CUSEC]
fragment_added <- fragments[, .(added_weight = sum(fragment_weight)), by = CUSEC]
control <- data.table(CUSEC = sections$CUSEC, control_population = sections$population)
original_weight <- base[, .(original_weight = sum(weight)), by = point_CUSEC]
alt_section <- merge(control, original_weight, by.x = "CUSEC", by.y = "point_CUSEC", all.x = TRUE)
alt_section <- merge(alt_section, point_removed, by.x = "CUSEC", by.y = "point_CUSEC", all.x = TRUE)
alt_section <- merge(alt_section, fragment_added, by = "CUSEC", all.x = TRUE)
alt_section[is.na(original_weight), original_weight := 0]
alt_section[is.na(removed_weight), removed_weight := 0]
alt_section[is.na(added_weight), added_weight := 0]
alt_section[, alternative_weight := original_weight - removed_weight + added_weight]

noncross_alt <- base[!component_key %in% cross_keys]
noncross_alt <- merge(noncross_alt, alt_section[, .(CUSEC, control_population, alternative_weight)], by.x = "point_CUSEC", by.y = "CUSEC", all.x = TRUE)
noncross_alt[, split_population := fifelse(alternative_weight > 0, control_population * weight / alternative_weight, 0)]
cross_alt <- merge(fragments, alt_section[, .(CUSEC, control_population, alternative_weight)], by = "CUSEC", all.x = TRUE)
cross_alt[, fragment_population := fifelse(alternative_weight > 0, control_population * fragment_weight / alternative_weight, 0)]
cross_alt <- cross_alt[, .(split_population = sum(fragment_population)), by = component_key]
alternative <- rbindlist(list(
  noncross_alt[, .(component_key, split_population)], cross_alt
))
comparison <- merge(base[, .(component_key, point_population)], alternative, by = "component_key", all.x = TRUE)
comparison[is.na(split_population), split_population := point_population]
comparison[, `:=`(
  absolute_difference = abs(split_population - point_population),
  signed_difference = split_population - point_population
)]
fwrite(comparison, file.path(qa_dir, "point_vs_boundary_split_sensitivity.csv.gz"))

total_population <- sum(sections$population)
fallback_population <- sum(pop_dt[support_type == "section_fallback", estimated_population])
nearest_weight_share <- sum(pop_dt[nearest_section_assignment == TRUE, chosen_weight]) / sum(pop_dt$chosen_weight)
inspire_fallback_dwelling_share <- sum(pop_dt[weight_source == "INSPIRE_numberOfDwellings_fallback", chosen_weight], na.rm = TRUE) / sum(pop_dt$chosen_weight)
all_weight_fallback_share <- sum(pop_dt[grepl("fallback|equivalent", weight_source), chosen_weight], na.rm = TRUE) / sum(pop_dt$chosen_weight)
metrics <- data.table(
  metric = c(
    "sections", "regional_population", "maximum_section_residual", "fallback_sections",
    "fallback_population_share", "nearest_assignment_count", "nearest_dwelling_weight_share",
    "maximum_nearest_distance_m", "maximum_parcel_dwelling_residual", "maximum_parcel_area_residual_m2",
    "inspire_fallback_dwelling_share", "all_weight_fallback_share", "sections_above_5_residents_per_dwelling",
    "population_share_in_sections_above_5_residents_per_dwelling", "dwelling_area_total_variation_share", "boundary_crossing_components",
    "boundary_split_total_variation_share", "boundary_split_net_residual"
  ),
  value = c(
    nrow(sections), total_population, max(abs(reconciliation$residual)), sum(reconciliation$fallback),
    fallback_population / total_population, sum(pop_dt$nearest_section_assignment), nearest_weight_share,
    if (any(pop_dt$nearest_section_assignment)) max(pop_dt$nearest_section_distance_m) else 0,
    max(abs(component_totals$dwelling_residual)), max(abs(component_totals$area_residual)),
    inspire_fallback_dwelling_share, all_weight_fallback_share, nrow(occupancy_outliers),
    sum(occupancy_outliers$control_population) / total_population,
    sum(method_sensitivity$absolute_difference) / (2 * total_population), nrow(crossing),
    sum(comparison$absolute_difference) / (2 * total_population), sum(comparison$signed_difference)
  )
)
fwrite(metrics, file.path(qa_dir, "phase2_metrics.csv"))

metric <- function(name) metrics[metric == name, value][1]
join_dwelling <- join_metrics[metric == "dwelling_weight_join_share", value]
join_area <- join_metrics[metric == "residential_area_join_share", value]
checks <- data.table(
  check = c("CAT-INSPIRE dwelling join >=99%", "CAT-INSPIRE residential-area join >=99%",
    "Parcel dwelling conservation", "Parcel area conservation", "Exact section population conservation",
    "Fallback population share <=0.1%", "All non-CAT dwelling fallback <=5%", "Boundary-split sensitivity <=0.1%"),
  passed = c(join_dwelling >= 0.99, join_area >= 0.99,
    metric("maximum_parcel_dwelling_residual") <= 1e-6,
    metric("maximum_parcel_area_residual_m2") <= 1e-5,
    metric("maximum_section_residual") <= 1e-6,
    metric("fallback_population_share") <= cfg$phase2$maximum_section_fallback_population_share,
    metric("all_weight_fallback_share") <= cfg$phase1_gates$maximum_fallback_share,
    metric("boundary_split_total_variation_share") <= cfg$phase2$boundary_split_materiality_population_share)
)
status <- if (all(checks$passed)) "GO_PHASE2_COMPLETE" else "REVIEW_REQUIRED"
fwrite(checks, file.path(qa_dir, "phase2_gate_checks.csv"))

png(file.path(qa_dir, "residents_per_dwelling_histogram.png"), width = 1400, height = 900, res = 150)
hist(section_diagnostics$residents_per_dwelling[is.finite(section_diagnostics$residents_per_dwelling) & section_diagnostics$control_population > 0],
  breaks = 80, xlim = c(0, 8), col = "#5B6F55", border = "white",
  main = "Residents per CAT-derived dwelling by census section", xlab = "Residents per reconstructed dwelling")
dev.off()

report <- c(
  "# Phase 2 — regional building-population model", "",
  paste0("**Decision: ", status, "**"), "",
  paste0("Reference population: **", format(total_population, big.mark = ","), " residents at ", cfg$phase2$population_reference_date, "**."),
  paste0("Support: **", nrow(pop_dt[support_type == "residential_component"]), " residential components across ", uniqueN(pop_dt$parcel_ref, na.rm = TRUE), " cadastral parcels**."), "",
  "## Gates", "",
  paste0("- ", checks$check, ": **", ifelse(checks$passed, "PASS", "FAIL"), "**"), "",
  "## Principal diagnostics", "",
  paste0("- Dwelling-weighted CAT–INSPIRE join coverage: ", sprintf("%.4f%%", 100 * join_dwelling), "."),
  paste0("- Residential-area-weighted CAT–INSPIRE join coverage: ", sprintf("%.4f%%", 100 * join_area), "."),
  paste0("- Census sections requiring a non-building fallback: ", metric("fallback_sections"), " (", sprintf("%.5f%%", 100 * metric("fallback_population_share")), " of population)."),
  paste0("- Component points assigned to the nearest section because they fell outside every section polygon: ", metric("nearest_assignment_count"), " (", sprintf("%.5f%%", 100 * metric("nearest_dwelling_weight_share")), " of dwelling weight)."),
  paste0("- Residual INSPIRE-only fallback support absent from the combined urban/rural CAT files: ", sprintf("%.3f%%", 100 * metric("inspire_fallback_dwelling_share")), " of dwelling weight."),
  paste0("- Rural CAT contribution: ", format(weight_source_summary[weight_source == "CAT_rural_reconstructed", parcels], big.mark = ","), " parcels and ", format(round(weight_source_summary[weight_source == "CAT_rural_reconstructed", dwelling_weight]), big.mark = ","), " dwelling units."),
  paste0("- All fallback-derived dwelling equivalents: ", sprintf("%.3f%%", 100 * metric("all_weight_fallback_share")), " of dwelling weight."),
  paste0("- Sections above five residents per derived dwelling: ", metric("sections_above_5_residents_per_dwelling"), " (", sprintf("%.3f%%", 100 * metric("population_share_in_sections_above_5_residents_per_dwelling")), " of regional population); retained as explicit QA outliers."),
  paste0("- Dwelling-versus-residential-area total-variation share: ", sprintf("%.3f%%", 100 * metric("dwelling_area_total_variation_share")), "."),
  paste0("- Components crossing section boundaries: ", metric("boundary_crossing_components"), "; point-versus-area-split total-variation share: ", sprintf("%.4f%%", 100 * metric("boundary_split_total_variation_share")), "."), "",
  "The production layer uses combined urban/rural CAT-reconstructed dwelling weights distributed among disconnected physical components by the Phase 1b BuildingPart volume rule. Positive INSPIRE dwelling counts provide an explicitly flagged residual fallback where neither CAT domain supplies a qualifying residential record. Every section control total is conserved. Residential-area allocation is retained as a sensitivity layer, not silently mixed into the primary method.", "",
  "Phase 3 station work has not been started."
)
writeLines(report, file.path(qa_dir, "phase2_report.md"))
jsonlite::write_json(list(
  status = status, generated_at = iso_now(), population_reference_date = cfg$phase2$population_reference_date,
  primary_method = "Combined urban/rural CAT reconstructed dwellings with flagged residual INSPIRE fallback; BuildingPart volume allocation for disconnected components",
  checks = lapply(seq_len(nrow(checks)), function(i) list(check = checks$check[i], passed = checks$passed[i])),
  metrics = stats::setNames(as.list(metrics$value), metrics$metric)
), file.path(cfg$paths$validation, "current_phase2_decision.json"), auto_unbox = TRUE, pretty = TRUE)

cat("Phase 2 validation decision:", status, "\n")
print(checks)
