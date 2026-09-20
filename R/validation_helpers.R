validate_inspire_buildings <- function(buildings) {
  plain <- data.table::as.data.table(sf::st_drop_geometry(buildings))
  duplicate_rows <- plain[duplicated(parcel_ref) | duplicated(parcel_ref, fromLast = TRUE), ]
  use_cross <- plain[, .(
    building_features = .N,
    dwelling_total = sum(number_dwellings, na.rm = TRUE),
    unit_total = sum(number_building_units, na.rm = TRUE)
  ), by = .(municipality_code, current_use, has_dwellings = number_dwellings > 0)]
  summary <- plain[, .(
    building_features = .N,
    unique_parcel_refs = data.table::uniqueN(parcel_ref),
    duplicate_parcel_rows = sum(duplicated(parcel_ref)),
    dwelling_total = sum(number_dwellings, na.rm = TRUE),
    buildings_with_dwellings = sum(number_dwellings > 0, na.rm = TRUE),
    multipart_buildings = sum(patch_count > 1, na.rm = TRUE),
    maximum_patch_count = max(patch_count, na.rm = TRUE),
    naive_patch_exploded_dwelling_total = sum(number_dwellings * patch_count, na.rm = TRUE),
    residential_dominant_zero_dwellings = sum(current_use == "1_residential" & (is.na(number_dwellings) | number_dwellings == 0), na.rm = TRUE),
    nonresidential_dominant_with_dwellings = sum(!is.na(current_use) & trimws(current_use) != "" & current_use != "1_residential" & number_dwellings > 0, na.rm = TRUE),
    unknown_use_with_dwellings = sum((is.na(current_use) | trimws(current_use) == "") & number_dwellings > 0, na.rm = TRUE)
  ), by = municipality_code]
  list(summary = summary, use_cross = use_cross, duplicates = duplicate_rows)
}

write_inspire_diagnostic_plots <- function(buildings, validation_dir) {
  plot_dir <- file.path(validation_dir, "maps")
  table_dir <- file.path(validation_dir, "tables")
  ensure_dir(plot_dir)
  ensure_dir(table_dir)
  x <- data.table::as.data.table(sf::st_drop_geometry(buildings))
  x[, use_group := data.table::fcase(
    is.na(current_use) | trimws(current_use) == "", "unknown",
    current_use == "1_residential", "residential dominant",
    default = "other dominant"
  )]
  outliers <- utils::head(x[order(-number_dwellings), .(municipality_code, parcel_ref, current_use, number_building_units, number_dwellings, official_area_m2, patch_count)], 100)
  data.table::fwrite(outliers, file.path(table_dir, "top_dwelling_outliers.csv"))

  p1 <- ggplot2::ggplot(x[patch_count > 0], ggplot2::aes(x = patch_count, fill = municipality_code)) +
    ggplot2::geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
    ggplot2::scale_x_continuous(trans = "log10") +
    ggplot2::labs(title = "Polygon patches per parcel-derived Building", x = "Patch count (log scale)", y = "Building features", fill = "Municipality") +
    ggplot2::theme_minimal(base_size = 12)
  ggplot2::ggsave(file.path(plot_dir, "patch_count_distribution.png"), p1, width = 8, height = 5, dpi = 160, bg = "#f4f0e8")

  p2 <- ggplot2::ggplot(x[number_dwellings > 0 & official_area_m2 > 0], ggplot2::aes(x = official_area_m2, y = number_dwellings, color = use_group)) +
    ggplot2::geom_point(alpha = 0.2, size = 0.65) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::scale_color_manual(values = c("residential dominant" = "#183229", "other dominant" = "#ef5b49", "unknown" = "#777777")) +
    ggplot2::labs(title = "INSPIRE dwellings and gross floor area", subtitle = "Parcel-derived attributes; currentUse is diagnostic only", x = "Official gross floor area, m² (log scale)", y = "numberOfDwellings (log scale)", color = NULL) +
    ggplot2::theme_minimal(base_size = 12)
  ggplot2::ggsave(file.path(plot_dir, "dwelling_area_diagnostic.png"), p2, width = 8, height = 5, dpi = 160, bg = "#f4f0e8")
}

compare_cat_inspire <- function(residential_parcels, buildings) {
  b <- data.table::as.data.table(sf::st_drop_geometry(buildings))[, .(parcel_ref, municipality_code, inspire_dwellings = number_dwellings, inspire_units = number_building_units, current_use, official_area_m2, patch_count)]
  merge(residential_parcels, b, by = "parcel_ref", all = TRUE)
}

phase1_gate_status <- function(comparison, cfg) {
  if (is.null(comparison) || !nrow(comparison)) {
    return(list(status = "BLOCKED_CAT_REQUIRED", reason = "Public INSPIRE checks are complete, but CAT Types 14 and 15 are required for the dwelling and residential-area decision."))
  }
  cat_rows <- comparison[!is.na(residential_area_m2)]
  joined <- cat_rows[!is.na(municipality_code)]
  join_coverage <- if (nrow(cat_rows)) sum(joined$residential_area_m2, na.rm = TRUE) / sum(cat_rows$residential_area_m2, na.rm = TRUE) else 0
  unresolved_share <- sum(cat_rows[confidence == "unresolved", residential_area_m2], na.rm = TRUE) / sum(cat_rows$residential_area_m2, na.rm = TRUE)
  primary_share <- sum(cat_rows[confidence %in% c("high", "medium"), residential_area_m2], na.rm = TRUE) / sum(cat_rows$residential_area_m2, na.rm = TRUE)
  if (join_coverage < cfg$phase1_gates$no_go_join_coverage) {
    status <- "NO_GO"
    reason <- "CAT-to-INSPIRE residential-area join coverage is below the no-go threshold."
  } else if (join_coverage >= cfg$phase1_gates$go_join_coverage && primary_share >= cfg$phase1_gates$primary_rule_coverage && unresolved_share <= cfg$phase1_gates$maximum_unresolved_share) {
    status <- "PROVISIONAL_GO_DWELLINGS"
    reason <- "Automated dwelling and join gates pass; case-study and independent INE checks remain required before Phase 2."
  } else if (join_coverage >= cfg$phase1_gates$go_join_coverage) {
    status <- "PROVISIONAL_GO_MODIFIED_AREA"
    reason <- "Residential-area coverage passes, but dwelling-rule coverage does not."
  } else {
    status <- "REVIEW_REQUIRED"
    reason <- "Results lie between the configured go and no-go thresholds."
  }
  list(status = status, reason = reason, join_coverage = join_coverage, primary_rule_share = primary_share, unresolved_share = unresolved_share)
}

validate_ine_housing <- function(comparison, buildings, sections_path, housing_json_path, validation_dir) {
  sections <- sf::st_read(sections_path, quiet = TRUE, stringsAsFactors = FALSE)
  sections <- sections[sections$TIPO == "SECCIONADO", ]
  if (sf::st_crs(sections)$epsg != 25830) sections <- sf::st_transform(sections, 25830)

  cat_rows <- comparison[!is.na(residential_area_m2) & !is.na(municipality_code)]
  idx <- match(cat_rows$parcel_ref, buildings$parcel_ref)
  if (anyNA(idx)) stop("Joined CAT parcels unexpectedly lack Building geometry.")
  support <- buildings[idx, c("parcel_ref", "number_dwellings")]
  support$cat_dwellings <- cat_rows$reconstructed_dwellings
  support$residential_area_m2 <- cat_rows$residential_area_m2
  points <- suppressWarnings(sf::st_point_on_surface(support))
  hits <- sf::st_within(points, sections)
  points$CUSEC <- vapply(hits, function(i) if (length(i)) as.character(sections$CUSEC[i[1]]) else NA_character_, character(1))

  assigned <- data.table::as.data.table(sf::st_drop_geometry(points))[!is.na(CUSEC)]
  section_cat <- assigned[, .(
    cat_parcels = .N,
    cat_reconstructed_dwellings = sum(cat_dwellings, na.rm = TRUE),
    inspire_dwellings = sum(number_dwellings, na.rm = TRUE),
    cat_residential_area_m2 = sum(residential_area_m2, na.rm = TRUE)
  ), by = CUSEC]

  ine <- jsonlite::fromJSON(housing_json_path)$data
  ine <- data.table::as.data.table(ine)
  ine[, CUSEC := sub("^.*([0-9]{10}).*$", "\\1", ID_RESIDENCIA_N5)]
  ine <- ine[, .(CUSEC, ine_2021_dwellings = as.numeric(SVIVIENDAS))]
  result <- merge(
    data.table::as.data.table(sf::st_drop_geometry(sections))[, .(CUSEC, CUMUN, NMUN)],
    section_cat,
    by = "CUSEC",
    all.x = TRUE
  )
  result <- merge(result, ine, by = "CUSEC", all.x = TRUE)
  for (nm in c("cat_parcels", "cat_reconstructed_dwellings", "inspire_dwellings", "cat_residential_area_m2")) data.table::setnafill(result, type = "const", fill = 0, cols = nm)
  result[, `:=`(
    cat_to_ine_ratio = cat_reconstructed_dwellings / ine_2021_dwellings,
    inspire_to_ine_ratio = inspire_dwellings / ine_2021_dwellings,
    cat_minus_ine = cat_reconstructed_dwellings - ine_2021_dwellings
  )]

  municipalities <- result[, .(
    sections = .N,
    sections_with_cat_weight = sum(cat_reconstructed_dwellings > 0),
    cat_reconstructed_dwellings = sum(cat_reconstructed_dwellings),
    inspire_dwellings = sum(inspire_dwellings),
    ine_2021_dwellings = sum(ine_2021_dwellings, na.rm = TRUE)
  ), by = .(CUMUN, NMUN)]
  municipalities[, `:=`(
    cat_to_ine_ratio = cat_reconstructed_dwellings / ine_2021_dwellings,
    inspire_to_ine_ratio = inspire_dwellings / ine_2021_dwellings
  )]

  complete <- result[ine_2021_dwellings > 0]
  metrics <- data.table::data.table(
    sections = nrow(result),
    sections_with_ine = nrow(complete),
    sections_without_cat_weight = sum(result$cat_reconstructed_dwellings == 0),
    unassigned_cat_parcels = sum(is.na(points$CUSEC)),
    pearson_cat_ine = stats::cor(complete$cat_reconstructed_dwellings, complete$ine_2021_dwellings, use = "complete.obs"),
    spearman_cat_ine = stats::cor(complete$cat_reconstructed_dwellings, complete$ine_2021_dwellings, method = "spearman", use = "complete.obs"),
    median_cat_to_ine_ratio = stats::median(complete$cat_to_ine_ratio, na.rm = TRUE),
    p05_cat_to_ine_ratio = as.numeric(stats::quantile(complete$cat_to_ine_ratio, 0.05, na.rm = TRUE)),
    p95_cat_to_ine_ratio = as.numeric(stats::quantile(complete$cat_to_ine_ratio, 0.95, na.rm = TRUE))
  )

  table_dir <- file.path(validation_dir, "tables")
  data.table::fwrite(result, file.path(table_dir, "ine_section_dwelling_comparison.csv"))
  data.table::fwrite(municipalities, file.path(table_dir, "ine_municipal_dwelling_comparison.csv"))
  data.table::fwrite(metrics, file.path(table_dir, "ine_section_validation_metrics.csv"))

  p <- ggplot2::ggplot(complete, ggplot2::aes(x = ine_2021_dwellings, y = cat_reconstructed_dwellings, color = NMUN)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "#777777", linewidth = 0.5) +
    ggplot2::geom_point(alpha = 0.35, size = 0.75) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "CAT 2026 reconstruction versus INE 2021 dwellings", subtitle = "Census sections; date mismatch means this is a diagnostic, not exact reconciliation", x = "INE 2021 dwellings", y = "CAT reconstructed dwellings", color = NULL) +
    ggplot2::theme_minimal(base_size = 12)
  ggplot2::ggsave(file.path(validation_dir, "maps", "cat_ine_section_dwelling_comparison.png"), p, width = 7.5, height = 6, dpi = 160, bg = "#f4f0e8")
  list(sections = result, municipalities = municipalities, metrics = metrics)
}
