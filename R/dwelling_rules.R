normalize_location <- function(block, stair, floor, door) {
  fields <- lapply(list(block, stair, floor, door), function(x) {
    x <- toupper(trimws(ifelse(is.na(x), "", x)))
    gsub("[^A-Z0-9+-]", "", x)
  })
  do.call(paste, c(fields, sep = "|"))
}

qualify_residential_construction <- function(cat14, cfg) {
  x <- data.table::copy(cat14)
  x[, typology4 := substr(ifelse(is.na(typology), "", typology), 1, 4)]
  x[, is_residential := destination == "V" & typology4 %in% unlist(cfg$cat$qualifying_typologies) & !is.na(area_m2) & area_m2 > cfg$cat$residential_area_min_m2]
  x[, housing_form := data.table::fcase(
    typology4 %in% unlist(cfg$cat$collective_typologies), "collective",
    typology4 %in% unlist(cfg$cat$single_family_typologies), "single_family",
    default = "other"
  )]
  x[, location_key := normalize_location(block, stair, floor, door)]
  x
}

derive_residential_parcels <- function(cat14, cat15, cfg) {
  x14 <- qualify_residential_construction(cat14, cfg)
  residential <- x14[is_residential == TRUE]
  if (!nrow(residential)) return(data.table::data.table())

  x15 <- data.table::copy(cat15)
  property_signals <- x15[, .(
    all_property_count = data.table::uniqueN(cargo, na.rm = TRUE),
    nonzero_division_order_count = data.table::uniqueN(cargo[!is.na(division_order) & division_order != "0"], na.rm = TRUE),
    positive_coefficient_count = data.table::uniqueN(cargo[!is.na(ownership_coefficient) & ownership_coefficient > 0], na.rm = TRUE)
  ), by = parcel_ref]

  cargo_summary <- residential[, .(
    residential_area_m2 = sum(area_m2, na.rm = TRUE),
    housing_forms = paste(sort(unique(housing_form)), collapse = "+"),
    location_count = data.table::uniqueN(location_key[location_key != "|||"]),
    blank_door_share = mean(is.na(door) | trimws(door) == ""),
    construction_count = .N
  ), by = .(parcel_ref, cargo)]

  parcel_base <- cargo_summary[, .(
    raw_residential_property_count = data.table::uniqueN(cargo, na.rm = TRUE),
    residential_area_m2 = sum(residential_area_m2, na.rm = TRUE),
    residential_construction_count = sum(construction_count),
    has_collective = any(grepl("collective", housing_forms)),
    has_single_family = any(grepl("single_family", housing_forms)),
    vertical_location_count = sum(ifelse(grepl("collective", housing_forms), pmax(location_count, 1), 0)),
    blank_door_share = weighted.mean(blank_door_share, pmax(construction_count, 1), na.rm = TRUE)
  ), by = parcel_ref]
  parcel_base <- merge(parcel_base, property_signals, by = "parcel_ref", all.x = TRUE)
  parcel_base[is.na(nonzero_division_order_count), nonzero_division_order_count := 0L]
  parcel_base[is.na(positive_coefficient_count), positive_coefficient_count := 0L]

  parcel_base[, `:=`(
    reconstructed_dwellings = data.table::fcase(
      has_single_family & !has_collective, as.numeric(raw_residential_property_count),
      has_collective & !has_single_family & raw_residential_property_count == 1 & residential_construction_count > 1 & vertical_location_count > 1, as.numeric(vertical_location_count),
      has_collective & !has_single_family, as.numeric(raw_residential_property_count),
      has_collective & has_single_family & raw_residential_property_count > 1, as.numeric(raw_residential_property_count),
      default = as.numeric(raw_residential_property_count)
    ),
    dwelling_rule = data.table::fcase(
      has_single_family & !has_collective, "single_family_property",
      has_collective & !has_single_family & raw_residential_property_count == 1 & residential_construction_count > 1 & vertical_location_count > 1, "vertical_collective_location_reconstruction",
      has_collective & !has_single_family, "collective_property_units",
      has_collective & has_single_family & raw_residential_property_count > 1, "mixed_typology_property_units",
      default = "ambiguous_property_count"
    )
  )]
  parcel_base[, confidence := data.table::fcase(
    dwelling_rule %in% c("collective_property_units", "single_family_property"), "high",
    dwelling_rule == "mixed_typology_property_units", "medium",
    dwelling_rule == "vertical_collective_location_reconstruction" & blank_door_share == 0, "medium",
    dwelling_rule == "vertical_collective_location_reconstruction", "low",
    default = "unresolved"
  )]
  parcel_base[]
}
