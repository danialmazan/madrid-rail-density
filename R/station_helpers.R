station_trim <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

station_title_case <- function(x) {
  particles <- c("de", "del", "la", "las", "los", "el", "y")
  one <- function(value) {
    if (is.na(value) || !nzchar(value)) return(NA_character_)
    words <- strsplit(tolower(value), "[[:space:]]+")[[1]]
    words <- vapply(seq_along(words), function(i) {
      word <- words[i]
      if (i > 1L && word %in% particles) return(word)
      paste0(toupper(substr(word, 1, 1)), substr(word, 2, nchar(word)))
    }, character(1))
    out <- paste(words, collapse = " ")
    out <- gsub("\\bT-?4\\b", "T4", out, ignore.case = TRUE)
    out
  }
  vapply(x, one, character(1))
}

split_station_lines <- function(x) {
  x <- station_trim(x)
  if (is.na(x)) return(character())
  trimws(unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE))
}

canonical_service_lines <- function(source_code, line_text) {
  tokens <- split_station_lines(line_text)
  if (!length(tokens)) return(data.table::data.table(mode = character(), line_id = character(), line_name = character()))
  if (source_code == "M4") {
    ids <- ifelse(toupper(tokens) == "R", "MR", paste0("M", tokens))
    return(data.table::data.table(mode = "metro", line_id = ids, line_name = tokens))
  }
  if (source_code == "M10") {
    modes <- ifelse(tokens == "4", "tranvia_parla", "metro_ligero")
    ids <- ifelse(tokens == "4", "TP", paste0("ML", tokens))
    names <- ifelse(tokens == "4", "Tranvía de Parla", paste0("ML", tokens))
    return(data.table::data.table(mode = modes, line_id = ids, line_name = names))
  }
  if (source_code == "M5") {
    ids <- toupper(gsub("[^[:alnum:]]", "", tokens))
    return(data.table::data.table(mode = "cercanias", line_id = ids, line_name = tokens))
  }
  stop("Unsupported CRTM source code: ", source_code)
}

station_group_key <- function(interchange_code, source_id, cfg) {
  code <- suppressWarnings(as.integer(station_trim(interchange_code)))
  out <- ifelse(
    !is.na(code),
    sprintf("crtm_interchange_%03d", code),
    paste0("crtm_station_", gsub("[^[:alnum:]]+", "_", tolower(source_id)))
  )
  split_id <- cfg$phase3$exceptions$split_interchange_62_source_id
  out[code == 62L & source_id != split_id] <- "crtm_interchange_062_callao"
  out[source_id == split_id] <- "crtm_interchange_062_santo_domingo"
  out
}

station_name_overrides <- function() {
  c(
    crtm_interchange_010 = "Embajadores–Acacias",
    crtm_interchange_013 = "Méndez Álvaro",
    crtm_interchange_018 = "Príncipe Pío",
    crtm_interchange_025 = "Madrid Chamartín–Clara Campoamor",
    crtm_interchange_033 = "Aeropuerto T4",
    crtm_interchange_039 = "Puerta de Arganda–Vicálvaro",
    crtm_interchange_049 = "Sierra de Guadalupe–Vallecas",
    crtm_interchange_054 = "Atocha",
    crtm_interchange_067 = "Colón–Recoletos",
    crtm_interchange_085 = "Pirámides",
    crtm_interchange_087 = "Coslada Central",
    crtm_interchange_088 = "Leganés Central",
    crtm_interchange_089 = "Alcorcón Central",
    crtm_interchange_090 = "Móstoles Central",
    crtm_interchange_091 = "Fuenlabrada Central",
    crtm_interchange_092 = "Getafe Central",
    crtm_interchange_093 = "El Casar",
    crtm_interchange_094 = "Aravaca",
    crtm_interchange_100 = "Parla Centro",
    crtm_interchange_181 = "Delicias",
    crtm_interchange_480 = "Mirasierra–Paco de Lucía",
    crtm_interchange_440 = "Fuencarral (Metro)",
    crtm_interchange_216 = "Fuencarral (Cercanías)",
    crtm_interchange_315 = "Reyes Católicos (San Sebastián de los Reyes)",
    crtm_interchange_421 = "Reyes Católicos (Parla)",
    crtm_interchange_332 = "San Fernando (Metro)",
    crtm_interchange_333 = "San Fernando de Henares",
    crtm_interchange_062_callao = "Callao",
    crtm_interchange_062_santo_domingo = "Santo Domingo"
  )
}

ordered_collapse <- function(x, levels = NULL, separator = "|") {
  x <- unique(x[!is.na(x) & nzchar(x)])
  if (!is.null(levels)) x <- x[order(match(x, levels), x, na.last = TRUE)] else x <- sort(x)
  paste(x, collapse = separator)
}

maximum_point_separation <- function(x, y) {
  if (length(x) <= 1L) return(0)
  max(as.matrix(stats::dist(cbind(x, y))))
}
