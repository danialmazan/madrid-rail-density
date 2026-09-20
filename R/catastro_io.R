find_column <- function(x, candidates, required = TRUE) {
  nm <- names(x)
  normalized <- tolower(gsub("[^a-z0-9]", "", nm))
  wanted <- tolower(gsub("[^a-z0-9]", "", candidates))
  hit <- match(wanted, normalized, nomatch = 0L)
  hit <- hit[hit > 0L]
  if (length(hit)) return(nm[hit[1]])
  if (required) stop("None of the expected columns was found: ", paste(candidates, collapse = ", "))
  NULL
}

gml_path_in_zip <- function(zip_path, object = c("building", "buildingpart", "otherconstruction")) {
  object <- match.arg(object)
  listing <- utils::unzip(zip_path, list = TRUE)$Name
  hit <- listing[grepl(paste0("\\.", object, "\\.gml$"), listing, ignore.case = TRUE)]
  if (length(hit) != 1L) stop("Expected exactly one ", object, " GML in ", zip_path)
  paste0("/vsizip/", normalizePath(zip_path), "/", hit)
}

read_inspire_layer <- function(zip_path, object = c("building", "buildingpart", "otherconstruction"), quiet = TRUE) {
  object <- match.arg(object)
  x <- sf::st_read(gml_path_in_zip(zip_path, object), quiet = quiet, stringsAsFactors = FALSE)
  if (sf::st_crs(x)$epsg != 25830) x <- sf::st_transform(x, 25830)
  x
}

standardize_buildings <- function(x, municipality_code = NA_character_) {
  local_col <- find_column(x, c("localId", "inspireId_localId", "reference"))
  use_col <- find_column(x, c("currentUse"), required = FALSE)
  unit_col <- find_column(x, c("numberOfBuildingUnits"), required = FALSE)
  dwelling_col <- find_column(x, c("numberOfDwellings"), required = FALSE)
  area_col <- find_column(x, c("officialArea_value", "value"), required = FALSE)
  geom <- sf::st_geometry(x)
  patch_count <- vapply(geom, function(g) {
    if (inherits(g, "MULTIPOLYGON")) length(g) else if (inherits(g, "POLYGON")) 1L else length(sf::st_cast(g, "POLYGON", warn = FALSE))
  }, integer(1))
  out <- sf::st_sf(
    municipality_code = municipality_code,
    parcel_ref = trimws(as.character(x[[local_col]])),
    current_use = if (is.null(use_col)) NA_character_ else as.character(x[[use_col]]),
    number_building_units = if (is.null(unit_col)) NA_real_ else safe_number(x[[unit_col]]),
    number_dwellings = if (is.null(dwelling_col)) NA_real_ else safe_number(x[[dwelling_col]]),
    official_area_m2 = if (is.null(area_col)) NA_real_ else safe_number(x[[area_col]]),
    patch_count = patch_count,
    geometry = geom,
    crs = sf::st_crs(x)
  )
  out$parcel_ref <- trimws(out$parcel_ref)
  out
}

open_cat_connection <- function(path) {
  if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    return(gzfile(path, open = "rt", encoding = "latin1"))
  }
  if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    listing <- utils::unzip(path, list = TRUE)$Name
    candidates <- listing[grepl("\\.(cat|txt)$", listing, ignore.case = TRUE)]
    if (!length(candidates)) candidates <- listing[!grepl("/$", listing)]
    if (length(candidates) != 1L) stop("Expected one CAT text member in ", path)
    return(unz(path, candidates, open = "r", encoding = "latin1"))
  }
  file(path, open = "r", encoding = "latin1")
}

extract_configured_cat_members <- function(zip_paths, municipality_codes, exdir) {
  ensure_dir(exdir)
  extracted <- character()
  for (zip_path in zip_paths) {
    listing <- tryCatch(utils::unzip(zip_path, list = TRUE)$Name, error = function(e) character())
    for (code in municipality_codes) {
      pattern <- paste0("(^|/)", code, "[UR]_[0-9]{8}\\.CAT\\.gz$")
      members <- listing[grepl(pattern, listing, ignore.case = TRUE)]
      if (length(members) > 1L) stop("Multiple CAT members found for municipality ", code, " in ", zip_path)
      if (length(members) == 1L) {
        destination <- file.path(exdir, members)
        if (!file.exists(destination)) {
          message("Extracting configured CAT member ", members, " from ", basename(zip_path))
          utils::unzip(zip_path, files = members, exdir = exdir)
        }
        extracted <- c(extracted, destination)
      }
    }
  }
  unique(extracted[file.exists(extracted)])
}

extract_nested_cat_archives <- function(zip_paths, exdir) {
  ensure_dir(exdir)
  extracted <- character()
  for (zip_path in zip_paths) {
    listing <- tryCatch(utils::unzip(zip_path, list = TRUE)$Name, error = function(e) character())
    members <- listing[grepl("(^|/)[0-9]{2}_[UR]_[0-9]{8}_CAT[.]zip$", listing, ignore.case = TRUE)]
    for (member in members) {
      destination <- file.path(exdir, basename(member))
      if (!file.exists(destination)) {
        message("Extracting nested CAT archive ", basename(member), " from ", basename(zip_path))
        utils::unzip(zip_path, files = member, exdir = exdir, junkpaths = TRUE)
      }
      extracted <- c(extracted, destination)
    }
  }
  unique(extracted[file.exists(extracted)])
}

slice_fields <- function(lines, schema) {
  out <- lapply(schema, function(spec) substr(lines, spec[[1]], spec[[2]]))
  data.table::setDT(out)
  data.table::setnames(out, names(schema))
  out
}

cat_schemas <- function() list(
  `11` = list(parcel_ref = c(31, 44), municipality_dgc = c(78, 80), municipality_ine = c(81, 83), municipality_name = c(84, 123), parcel_area_m2 = c(296, 305), total_built_area_m2 = c(306, 312), above_ground_area_m2 = c(313, 319), below_ground_area_m2 = c(320, 326)),
  `13` = list(parcel_ref = c(31, 44), unit_code = c(45, 48), construction_year = c(296, 299), unit_footprint_m2 = c(301, 307)),
  `14` = list(parcel_ref = c(31, 44), construction_order = c(45, 48), cargo = c(51, 54), unit_code = c(55, 58), block = c(59, 62), stair = c(63, 64), floor = c(65, 67), door = c(68, 70), destination = c(71, 73), area_m2 = c(84, 90), porch_terrace_m2 = c(91, 97), other_floor_area_m2 = c(98, 104), typology = c(105, 109)),
  `15` = list(parcel_ref = c(31, 44), cargo = c(45, 48), fixed_property_id = c(51, 58), block = c(246, 249), stair = c(250, 251), floor = c(252, 254), door = c(255, 257), division_order = c(368, 371), property_use = c(428, 428), constructed_area_m2 = c(442, 451), ownership_coefficient = c(462, 470))
)

clean_cat_table <- function(x, record_type) {
  x <- data.table::copy(x)
  character_cols <- setdiff(names(x), grep("(_m2|coefficient|year)$", names(x), value = TRUE))
  for (nm in character_cols) data.table::set(x, j = nm, value = trim_na(x[[nm]]))
  numeric_cols <- grep("(_m2|coefficient|year)$", names(x), value = TRUE)
  for (nm in numeric_cols) data.table::set(x, j = nm, value = safe_number(x[[nm]]))
  if ("ownership_coefficient" %in% names(x)) x[, ownership_coefficient := ownership_coefficient / 1e6]
  data.table::set(x, j = "record_type", value = record_type)
  x
}

parse_cat_file <- function(path, keep = c("11", "13", "14", "15"), chunk_size = 100000L) {
  schemas <- cat_schemas()
  con <- open_cat_connection(path)
  on.exit(close(con), add = TRUE)
  buckets <- stats::setNames(vector("list", length(keep)), keep)
  repeat {
    lines <- readLines(con, n = chunk_size, warn = FALSE)
    if (!length(lines)) break
    record_type <- substr(lines, 1, 2)
    for (type in keep) {
      selected <- lines[record_type == type]
      if (length(selected)) buckets[[type]][[length(buckets[[type]]) + 1L]] <- slice_fields(selected, schemas[[type]])
    }
  }
  lapply(keep, function(type) {
    parts <- buckets[[type]]
    if (!length(parts)) return(data.table::data.table())
    clean_cat_table(data.table::rbindlist(parts, use.names = TRUE), type)
  }) |> stats::setNames(keep)
}
