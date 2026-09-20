#!/usr/bin/env Rscript
source("R/utils.R")
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(data.table))

cfg <- read_config()
ensure_dir(cfg$paths$phase2_derived)

sections <- readRDS(file.path(cfg$paths$phase2_interim, "ine_sections_2025.rds"))
points <- readRDS(file.path(cfg$paths$phase2_interim, "regional_component_points.rds"))
if (st_crs(points)$epsg != cfg$project$crs) points <- st_transform(points, cfg$project$crs)
if (st_crs(sections)$epsg != cfg$project$crs) sections <- st_transform(sections, cfg$project$crs)

message("Assigning ", nrow(points), " component points to 2025 census sections")
hits <- st_within(points, sections)
section_index <- vapply(hits, function(z) if (length(z)) z[1] else NA_integer_, integer(1))
multiple_hits <- lengths(hits) > 1L
nearest_assignment <- is.na(section_index)
nearest_distance_m <- rep(0, nrow(points))
if (any(nearest_assignment)) {
  nearest <- st_nearest_feature(points[nearest_assignment, ], sections)
  nearest_distance_m[nearest_assignment] <- as.numeric(st_distance(
    points[nearest_assignment, ], sections[nearest, ], by_element = TRUE
  ))
  section_index[nearest_assignment] <- nearest
}
if (anyNA(section_index)) stop("At least one residential component could not be assigned to a section.")

xy <- st_coordinates(points)
dt <- as.data.table(st_drop_geometry(points))
dt[, `:=`(
  x = xy[, 1], y = xy[, 2],
  CUSEC = sections$CUSEC[section_index], CUMUN = sections$CUMUN[section_index],
  municipality_name = sections$NMUN[section_index], section_population = sections$population[section_index],
  nearest_section_assignment = nearest_assignment,
  nearest_section_distance_m = nearest_distance_m,
  multiple_section_hits = multiple_hits,
  support_type = "residential_component"
)]

section_weights <- dt[, .(
  dwelling_weight = sum(chosen_weight, na.rm = TRUE),
  area_weight = sum(component_residential_area_m2, na.rm = TRUE),
  component_count = .N,
  parcel_count = uniqueN(parcel_ref)
), by = CUSEC]
section_control <- data.table(
  CUSEC = sections$CUSEC, CUMUN = sections$CUMUN, municipality_name = sections$NMUN,
  population = sections$population
)
section_control <- merge(section_control, section_weights, by = "CUSEC", all.x = TRUE)
for (nm in c("dwelling_weight", "area_weight", "component_count", "parcel_count")) {
  set(section_control, which(is.na(section_control[[nm]])), nm, 0)
}

fallback_sections <- section_control[population > 0 & dwelling_weight <= 0]
if (nrow(fallback_sections)) {
  fallback_geo <- suppressWarnings(st_point_on_surface(sections[match(fallback_sections$CUSEC, sections$CUSEC), ]))
  fallback_xy <- st_coordinates(fallback_geo)
  fallback <- data.table(
    component_key = paste0("SECTION_FALLBACK_", fallback_sections$CUSEC),
    parcel_ref = NA_character_, municipality_code = NA_character_, component_id = NA_integer_,
    component_count = 1L, footprint_m2 = NA_real_, parcel_dwellings = NA_real_,
    parcel_residential_area_m2 = NA_real_, building_part_count = NA_integer_, volume_proxy = NA_real_,
    maximum_floors = NA_real_, volume_source = "section_fallback", minor_ancillary = FALSE,
    chosen_weight = 1, chosen_weight_method = "section_fallback",
    component_residential_area_m2 = 1, x = fallback_xy[, 1], y = fallback_xy[, 2],
    CUSEC = fallback_sections$CUSEC, CUMUN = fallback_sections$CUMUN,
    municipality_name = fallback_sections$municipality_name,
    section_population = fallback_sections$population,
    nearest_section_assignment = FALSE, nearest_section_distance_m = 0,
    multiple_section_hits = FALSE, support_type = "section_fallback"
  )
  fallback[, `:=`(
    weight_source = "section_fallback", dwelling_rule = "section_fallback",
    confidence = "fallback", residential_area_share = NA_real_,
    raw_residential_property_count = NA_real_
  )]
  dt <- rbindlist(list(dt, fallback), use.names = TRUE, fill = TRUE)
}

dt[, section_dwelling_weight := sum(chosen_weight, na.rm = TRUE), by = CUSEC]
dt[, section_area_weight := sum(component_residential_area_m2, na.rm = TRUE), by = CUSEC]
dt[, section_residents_per_dwelling := fifelse(section_population > 0 & section_dwelling_weight > 0,
  section_population / section_dwelling_weight, NA_real_)]
dt[, section_support_quality := fcase(
  section_residents_per_dwelling > 10, "critical_low_dwelling_support",
  section_residents_per_dwelling > 5, "high_residents_per_dwelling",
  section_population == 0, "zero_population",
  default = "typical"
)]
dt[, `:=`(
  population_reference_date = cfg$phase2$population_reference_date,
  cat_snapshot = cfg$phase2$cat_snapshot,
  inspire_snapshot = cfg$phase2$inspire_snapshot,
  population_method_version = "phase2_v1_cat_dwellings_component_volume"
)]
dt[, estimated_population := fifelse(section_population > 0 & section_dwelling_weight > 0,
  section_population * chosen_weight / section_dwelling_weight, 0)]
dt[, estimated_population_area := fifelse(section_population > 0 & section_area_weight > 0,
  section_population * component_residential_area_m2 / section_area_weight, 0)]

# Correct tiny floating-point residuals on the largest support so every control total is exact.
for (value_col in c("estimated_population", "estimated_population_area")) {
  residuals <- dt[, .(residual = unique(section_population) - sum(get(value_col))), by = CUSEC]
  correction_index <- dt[, .I[which.max(chosen_weight)], by = CUSEC]$V1
  dt[correction_index, (value_col) := get(value_col) + residuals$residual[match(CUSEC, residuals$CUSEC)]]
}

population_points <- st_as_sf(dt, coords = c("x", "y"), crs = cfg$project$crs, remove = FALSE)
saveRDS(population_points, file.path(cfg$paths$phase2_derived, "component_population_2025.rds"), compress = "xz")
fwrite(st_drop_geometry(population_points), file.path(cfg$paths$phase2_derived, "component_population_2025.csv.gz"), na = "")

reconciliation <- dt[, .(
  control_population = unique(section_population),
  allocated_population = sum(estimated_population),
  allocated_population_area = sum(estimated_population_area),
  dwelling_weight = sum(chosen_weight), area_weight = sum(component_residential_area_m2),
  components = .N, fallback = any(support_type == "section_fallback")
), by = .(CUSEC, CUMUN, municipality_name)]
reconciliation[, `:=`(
  residual = allocated_population - control_population,
  area_residual = allocated_population_area - control_population
)]
fwrite(reconciliation, file.path(cfg$paths$phase2_derived, "section_reconciliation.csv"))
municipal <- reconciliation[, .(
  control_population = sum(control_population), allocated_population = sum(allocated_population),
  allocated_population_area = sum(allocated_population_area), sections = .N,
  fallback_sections = sum(fallback)
), by = .(CUMUN, municipality_name)]
fwrite(municipal, file.path(cfg$paths$phase2_derived, "municipality_reconciliation.csv"))
fwrite(dt[nearest_section_assignment == TRUE, .(
  component_key, parcel_ref, municipality_code, CUSEC, nearest_section_distance_m,
  chosen_weight, component_residential_area_m2
)], file.path(cfg$paths$phase2_derived, "nearest_section_assignments.csv"))

cat("Allocated", format(sum(dt$estimated_population), big.mark = ","), "residents across", uniqueN(dt$CUSEC), "sections.\n")
cat("Section fallbacks:", sum(dt$support_type == "section_fallback"), "; nearest-section assignments:", sum(dt$nearest_section_assignment), ".\n")
