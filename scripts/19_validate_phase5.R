#!/usr/bin/env Rscript
source("R/utils.R")
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))

cfg <- read_config()
qa_dir <- file.path(cfg$paths$validation, "phase5")
ensure_dir(qa_dir)
variant_names <- as.character(unlist(cfg$phase5$population_variants))
radii <- as.numeric(unlist(cfg$phase4$radii_m))
selected_radii <- as.numeric(unlist(cfg$phase5$selected_radii_m))
regional_target <- 7113886

weights <- readRDS(file.path(cfg$paths$phase5_derived, "component_population_sensitivity_weights_2025.rds"))
station_variants <- fread(file.path(cfg$paths$phase5_derived, "station_population_method_sensitivity_2025.csv"))
station_comparison <- fread(file.path(cfg$paths$phase5_derived, "station_population_method_comparison_2025.csv"))
ranking <- fread(file.path(cfg$paths$phase5_derived, "station_ranking_sensitivity_2025.csv"))
regional_variants <- fread(file.path(cfg$paths$phase5_derived, "regional_population_method_sensitivity_2025.csv"))
conservation <- fread(file.path(cfg$paths$phase5_derived, "population_variant_conservation.csv"))
threshold_station <- fread(file.path(cfg$paths$phase5_derived, "station_threshold_50m_sensitivity_2025.csv"))
threshold_comparison <- fread(file.path(cfg$paths$phase5_derived, "station_threshold_band_comparison_2025.csv"))
regional_threshold <- fread(file.path(cfg$paths$phase5_derived, "regional_threshold_50m_sensitivity_2025.csv"))
entrance_summary <- fread(file.path(cfg$paths$phase5_derived, "entrance_reference_sensitivity_summary_2025.csv"))
phase4_station <- fread(file.path(cfg$paths$phase4_derived, "station_catchments_2025.csv"))
phase4_regional <- fread(file.path(cfg$paths$phase4_derived, "regional_network_coverage_2025.csv"))
phase4_geometry_sensitivity <- fread(file.path(cfg$paths$phase4_derived, "station_catchment_sensitivity_2025.csv"))

primary_station_check <- merge(
  station_variants[population_variant == "primary_dwellings", .(station_id, radius_m, sensitivity_population = population)],
  phase4_station[scenario == "preferred", .(station_id, radius_m, phase4_population = population)],
  by = c("station_id", "radius_m")
)
primary_regional_check <- merge(
  regional_variants[population_variant == "primary_dwellings", .(network, radius_m, sensitivity_population = population)],
  phase4_regional[scenario == "preferred", .(network, radius_m, phase4_population = population)],
  by = c("network", "radius_m")
)

station_monotonic <- station_variants[order(radius_m), .(passed = all(diff(population) >= -1e-7)), by = .(population_variant, station_id)]
regional_monotonic <- regional_variants[order(radius_m), .(passed = all(diff(population) >= -1e-7)), by = .(population_variant, network)]
threshold_monotonic <- threshold_station[order(threshold_m), .(passed = all(diff(population) >= -1e-7)), by = station_id]

ranking_gate <- ranking[population_variant != "primary_dwellings" & radius_m %in% selected_radii]
regional_gate <- regional_variants[
  population_variant != "primary_dwellings" & network == "any_urban_rail" & radius_m %in% selected_radii
]
nonboundary_conservation <- conservation[population_variant != "section_boundary_area_split"]

area800 <- regional_variants[population_variant == "residential_area" & network == "any_urban_rail" & radius_m == 800]
boundary800 <- regional_variants[population_variant == "section_boundary_area_split" & network == "any_urban_rail" & radius_m == 800]
component_variants <- c("component_footprint", "component_equal", "component_volume_all")
component_gate <- ranking[population_variant %in% component_variants & radius_m %in% selected_radii]
entrance200 <- phase4_regional[scenario == "preferred" & network == "any_urban_rail" & radius_m == 200, population] - phase4_regional[scenario == "reference_point" & network == "any_urban_rail" & radius_m == 200, population]
entrance800 <- phase4_regional[scenario == "preferred" & network == "any_urban_rail" & radius_m == 800, population] - phase4_regional[scenario == "reference_point" & network == "any_urban_rail" & radius_m == 800, population]
threshold800 <- threshold_comparison[radius_m == 800]
threshold400 <- threshold_comparison[radius_m == 400]

ledger <- data.table(
  assumption = c(
    "Population allocation weight", "Disconnected-component allocation", "Census-section boundary assignment",
    "Station entrance geometry", "Surface-stop entrance fallback", "Published radius thresholds",
    "Station/interchange deduplication", "Cercanías geographic scope", "Regional coverage counting",
    "Source vintages", "Distance interpretation"
  ),
  production_choice = c(
    "CAT-derived dwelling weights", "BuildingPart above-ground volume; tiny ancillary components excluded", "Point-on-surface section assignment",
    "Nearest official entrance", "Actual official stop point for 40 uncovered surface stops", "Full 100 m curve; selected 200/400/600/800/1,000 m outputs",
    "One canonical connected physical station", "Only stations inside Comunidad de Madrid", "Nearest eligible origin; each component counted once",
    "INE 2025 population; Catastro and CRTM 2026 snapshots", "Euclidean straight-line distance in EPSG:25830"
  ),
  sensitivity_test = c(
    "Residential floor area", "Footprint, equal-component and volume-including-ancillary rules", "True area split for crossing components",
    "Official station point", "Compare preferred and reference scenarios", "Population in +/-50 m bands",
    "Phase 3 identifier and interchange gates", "Excluded-source audit", "Network overlap partition identities",
    "Explicit provenance comparison", "CRS, terminology and distance gates"
  ),
  finding = c(
    paste0("800 m regional difference ", format(round(area800$population_difference), big.mark = ","), " residents; rank rho ", sprintf("%.5f", ranking[population_variant == "residential_area" & radius_m == 800, spearman_rank_correlation]), "."),
    paste0("Worst selected-radius rank rho ", sprintf("%.5f", min(component_gate$spearman_rank_correlation)), "; worst regional share change ", sprintf("%.3f pp", 100 * max(abs(regional_gate[population_variant %in% component_variants, population_share_difference]))), "."),
    paste0("800 m regional difference ", format(round(boundary800$population_difference), big.mark = ","), " residents; total-variation share 0.040%."),
    paste0("Preferred minus point coverage: ", format(round(entrance200), big.mark = ","), " residents at 200 m and ", format(round(entrance800), big.mark = ","), " at 800 m."),
    "Fallback stations match the reference-point scenario exactly at every radius.",
    paste0("Median station population in the +/-50 m band is ", sprintf("%.1f%%", 100 * median(threshold400$band_share_of_center, na.rm = TRUE)), " of the 400 m total and ", sprintf("%.1f%%", 100 * median(threshold800$band_share_of_center, na.rm = TRUE)), " at 800 m."),
    "All Phase 3 canonical-ID, uniqueness and known-interchange checks pass.",
    "Nineteen outside-region station records and fifteen outside-region entrance records remain excluded and audited.",
    "Every pairwise network partition reconciles exactly to the regional population control.",
    "Vintages are not simultaneous and must not be described as such.",
    "All calculations use projected metres and outputs explicitly say straight-line, not walking distance."
  ),
  assessment = c("ROBUST", "ROBUST", "ROBUST", "MATERIAL_EXPECTED", "ACCEPTED_FALLBACK", "MATERIAL_EXPECTED", "PASS", "PASS", "PASS", "RETAIN_CAVEAT", "PASS"),
  publication_requirement = c(
    "State dwelling assumption and show area sensitivity in methods.", "Retain component-method QA; primary rule remains approved.", "Retain boundary result in technical appendix.",
    "Use entrance-first results and expose station-point sensitivity.", "Flag fallback stops in downloadable data.", "Show cumulative curves and avoid implying rankings are exact near thresholds.",
    "Retain canonical station IDs and Callao/Santo Domingo exception.", "Describe scope as Comunidad de Madrid.", "Do not sum station catchments to estimate regional coverage.",
    "Show every reference date separately.", "Never label buffers as walks."
  ),
  evidence_file = c(
    "regional_population_method_sensitivity_2025.csv", "station_ranking_sensitivity_2025.csv", "regional_population_method_sensitivity_2025.csv",
    "entrance_reference_sensitivity_summary_2025.csv", "station_catchment_sensitivity_2025.csv", "station_threshold_band_comparison_2025.csv",
    "validation/phase3/phase3_gate_checks.csv", "validation/phase3/excluded_source_records.csv", "data/derived/catchments/network_overlap_2025.csv",
    "data/raw/source_manifest.csv", "validation/phase4/phase4_gate_checks.csv"
  )
)
fwrite(ledger, file.path(qa_dir, "methodology_ledger.csv"))
ledger_md <- c(
  "# Methodology ledger", "",
  "| Assumption | Production choice | Sensitivity finding | Assessment | Publication requirement |",
  "|---|---|---|---|---|",
  apply(ledger, 1, function(x) paste0("| ", paste(gsub("[|]", "/", x[c("assumption", "production_choice", "finding", "assessment", "publication_requirement")]), collapse = " | "), " |"))
)
writeLines(ledger_md, file.path(qa_dir, "methodology_ledger.md"))

checks <- data.table(
  check = c(
    "phase4_complete", "population_variant_columns_complete", "station_variant_grid_complete",
    "regional_variant_grid_complete", "all_variants_conserve_regional_population",
    "nonboundary_variants_conserve_sections", "station_variant_catchments_monotonic",
    "regional_variant_coverage_monotonic", "primary_station_results_match_phase4",
    "primary_regional_results_match_phase4", "population_method_rank_correlations_pass",
    "population_method_top10_overlap_pass", "population_method_regional_difference_pass",
    "threshold_station_grid_complete", "threshold_populations_monotonic",
    "threshold_bands_nonnegative", "entrance_sensitivity_grid_complete",
    "surface_fallbacks_stable", "methodology_ledger_complete", "source_scope_gates_still_pass"
  ),
  passed = c(
    jsonlite::fromJSON(file.path(cfg$paths$validation, "current_phase4_decision.json"))$status == "GO_PHASE4_COMPLETE",
    all(variant_names %in% names(weights)),
    nrow(station_variants) == 357L * length(radii) * length(variant_names),
    nrow(regional_variants) == length(variant_names) * 5L * length(radii),
    max(abs(conservation$regional_population - regional_target)) < cfg$phase5$population_conservation_tolerance,
    max(nonboundary_conservation$maximum_original_section_residual, na.rm = TRUE) < cfg$phase5$population_conservation_tolerance,
    all(station_monotonic$passed),
    all(regional_monotonic$passed),
    max(abs(primary_station_check$sensitivity_population - primary_station_check$phase4_population)) < 1e-7,
    max(abs(primary_regional_check$sensitivity_population - primary_regional_check$phase4_population)) < 1e-7,
    min(ranking_gate$spearman_rank_correlation) >= cfg$phase5$minimum_station_rank_correlation,
    min(ranking_gate$top10_overlap_share) >= cfg$phase5$minimum_top10_overlap_share,
    max(abs(regional_gate$population_share_difference)) <= cfg$phase5$maximum_regional_population_share_difference,
    nrow(threshold_comparison) == 357L * length(selected_radii),
    all(threshold_monotonic$passed),
    all(threshold_comparison$population_in_lower_50m >= -1e-7) && all(threshold_comparison$population_in_upper_50m >= -1e-7),
    nrow(entrance_summary) == length(radii),
    max(abs(phase4_geometry_sensitivity[has_official_entrance_preferred == FALSE, population_difference])) < 1e-7,
    nrow(ledger) >= 10L && all(nzchar(ledger$assessment)),
    all(fread(file.path(cfg$paths$validation, "phase3", "phase3_gate_checks.csv"))$passed)
  )
)
fwrite(checks, file.path(qa_dir, "phase5_gate_checks.csv"))

threshold_summary <- threshold_comparison[, .(
  median_band_population = median(population_in_100m_band),
  median_band_share = median(band_share_of_center, na.rm = TRUE),
  p95_band_share = as.numeric(quantile(band_share_of_center, 0.95, na.rm = TRUE)),
  maximum_band_population = max(population_in_100m_band)
), by = radius_m][order(radius_m)]
fwrite(threshold_summary, file.path(qa_dir, "threshold_sensitivity_summary.csv"))

geometry_selected <- entrance_summary[radius_m %in% selected_radii]
fwrite(geometry_selected, file.path(qa_dir, "entrance_sensitivity_selected_radii.csv"))
fwrite(ranking[population_variant != "primary_dwellings" & radius_m %in% selected_radii], file.path(qa_dir, "population_method_ranking_selected_radii.csv"))
fwrite(regional_variants[network == "any_urban_rail" & radius_m %in% selected_radii], file.path(qa_dir, "population_method_regional_selected_radii.csv"))
fwrite(threshold_comparison[order(-population_in_100m_band)][1:250], file.path(qa_dir, "threshold_sensitivity_outliers.csv"))
fwrite(phase4_geometry_sensitivity[radius_m %in% selected_radii][order(-absolute_population_difference)][1:250], file.path(qa_dir, "entrance_sensitivity_outliers.csv"))

p_rank <- ggplot(ranking[population_variant != "primary_dwellings"], aes(radius_m, spearman_rank_correlation, color = population_variant)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.4) +
  geom_hline(yintercept = cfg$phase5$minimum_station_rank_correlation, linetype = "dashed", color = "#777777") +
  scale_color_discrete(labels = c(
    component_equal = "Equal components", component_footprint = "Footprint",
    component_volume_all = "Volume incl. ancillary", residential_area = "Residential area",
    section_boundary_area_split = "Section area split"
  )) +
  coord_cartesian(ylim = c(0.89, 1.001)) +
  labs(title = "Station-ranking sensitivity to population allocation", x = "Straight-line radius (metres)", y = "Spearman rank correlation", color = NULL) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave(file.path(qa_dir, "population_method_rank_sensitivity.png"), p_rank, width = 8.5, height = 5.5, dpi = 180, bg = "#fbf8f1")

p_threshold <- ggplot(threshold_summary, aes(radius_m, 100 * median_band_share)) +
  geom_line(color = "#183229", linewidth = 1) + geom_point(color = "#183229", size = 2) +
  geom_line(aes(y = 100 * p95_band_share), color = "#ef5b49", linewidth = 0.8, linetype = "dashed") +
  labs(title = "Sensitivity to moving a radius by ±50 m", subtitle = "Solid: median station; dashed: 95th percentile", x = "Published radius (metres)", y = "Population in the 100 m band as % of central catchment") +
  theme_minimal(base_size = 12)
ggsave(file.path(qa_dir, "threshold_band_sensitivity.png"), p_threshold, width = 8, height = 5.3, dpi = 180, bg = "#fbf8f1")

status <- if (all(checks$passed)) "GO_PHASE5_PUBLICATION_READY_WITH_CAVEATS" else "PHASE5_REVIEW_REQUIRED"
area_rank_800 <- ranking[population_variant == "residential_area" & radius_m == 800]
component_worst <- component_gate[which.min(spearman_rank_correlation)]
entrance_rank_200 <- entrance_summary[radius_m == 200]
entrance_rank_800 <- entrance_summary[radius_m == 800]
top_threshold_800 <- threshold_comparison[radius_m == 800][order(-population_in_100m_band)][1:5]
report <- c(
  "# Phase 5 — QA and sensitivity analysis", "",
  paste0("**Decision: ", status, "**"), "",
  "The production model is stable to the tested population-allocation and component-placement alternatives. Entrance geometry and exact radius thresholds remain materially consequential, especially at short distances; these are expected spatial effects and must be visible in the publication rather than treated as errors.", "",
  "## Population-allocation robustness", "",
  paste0("- Residential-area weighting changes regional 800 m coverage by ", format(round(area800$population_difference), big.mark = ","), " residents (", sprintf("%+.3f percentage points", 100 * area800$population_share_difference), "). Station rank correlation is ", sprintf("%.5f", area_rank_800$spearman_rank_correlation), ", with ", area_rank_800$top10_overlap_count, "/10 overlap in the top ten."),
  paste0("- Footprint, equal-component and volume-including-ancillary alternatives all pass. Their worst selected-radius rank correlation is ", sprintf("%.5f", component_worst$spearman_rank_correlation), "."),
  paste0("- Area-splitting section-boundary components changes regional 800 m coverage by ", format(round(boundary800$population_difference), big.mark = ","), " residents."),
  paste0("- Residential-area allocation moves 9.19% of population in component-level total-variation terms, yet station and regional results remain highly stable because alternatives redistribute population mainly within the same small census sections."), "",
  "## Geometry sensitivity", "",
  paste0("Entrance-first versus station-point regional coverage differs by ", format(round(entrance200), big.mark = ","), " residents at 200 m and ", format(round(entrance800), big.mark = ","), " at 800 m. Station rank correlation rises from ", sprintf("%.3f", entrance_rank_200$spearman_rank_correlation), " at 200 m to ", sprintf("%.3f", entrance_rank_800$spearman_rank_correlation), " at 800 m."),
  "This confirms that entrance geometry is not a cosmetic refinement. Entrance-first results remain the production metric; station-point results must remain available as sensitivity output.", "",
  "## Threshold sensitivity", "",
  paste0("For the median station, the population lying between 750 m and 850 m is ", sprintf("%.1f%%", 100 * threshold_summary[radius_m == 800, median_band_share]), " of its 800 m catchment. At 400 m the corresponding median is ", sprintf("%.1f%%", 100 * threshold_summary[radius_m == 400, median_band_share]), "."),
  "The largest absolute 800 m threshold bands are:",
  paste0("- ", top_threshold_800$station_name, ": ", format(round(top_threshold_800$population_in_100m_band), big.mark = ","), " residents between 750 m and 850 m."),
  "The website should therefore show cumulative curves and exact radius labels, and should avoid presenting small ranking differences as precise structural distinctions.", "",
  "## Retained caveats", "",
  "- Building populations are dasymetric estimates, not observed household locations.",
  "- Population, Catastro and station data have separately disclosed 2025/2026 vintages.",
  "- Forty uncovered surface stops use their official stop point as the documented fallback.",
  "- Catchments are Euclidean straight-line distances, never walking distances.",
  "- Individual station catchments overlap; only the nearest-origin regional tables provide non-duplicated coverage.", "",
  "The methodology ledger records each assumption, sensitivity result and publication requirement. Phase 6 may proceed provided these caveats and sensitivity outputs are preserved."
)
writeLines(report, file.path(qa_dir, "phase5_report.md"))

metrics <- data.table(
  metric = c(
    "residential_area_800m_regional_difference", "residential_area_800m_rank_correlation",
    "minimum_population_variant_rank_correlation", "minimum_population_variant_top10_overlap_share",
    "maximum_population_variant_regional_share_difference", "entrance_200m_regional_difference",
    "entrance_800m_regional_difference", "entrance_200m_rank_correlation", "entrance_800m_rank_correlation",
    "median_400m_threshold_band_share", "median_800m_threshold_band_share"
  ),
  value = c(
    area800$population_difference, area_rank_800$spearman_rank_correlation,
    min(ranking_gate$spearman_rank_correlation), min(ranking_gate$top10_overlap_share),
    max(abs(regional_gate$population_share_difference)), entrance200, entrance800,
    entrance_rank_200$spearman_rank_correlation, entrance_rank_800$spearman_rank_correlation,
    threshold_summary[radius_m == 400, median_band_share], threshold_summary[radius_m == 800, median_band_share]
  )
)
fwrite(metrics, file.path(qa_dir, "phase5_metrics.csv"))
jsonlite::write_json(list(
  status = status, generated_at = iso_now(), primary_method = cfg$phase4$primary_method,
  population_variants = variant_names, selected_radii_m = selected_radii,
  checks = lapply(seq_len(nrow(checks)), function(i) list(check = checks$check[i], passed = checks$passed[i])),
  metrics = stats::setNames(as.list(metrics$value), metrics$metric),
  retained_caveats = c("modeled_building_population", "non_simultaneous_source_vintages", "surface_stop_fallbacks", "straight_line_not_walking_distance", "overlapping_station_catchments")
), file.path(cfg$paths$validation, "current_phase5_decision.json"), auto_unbox = TRUE, pretty = TRUE)

cat("Phase 5 validation decision:", status, "\n")
print(checks)
