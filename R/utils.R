read_config <- function(path = "config.yml") {
  if (!requireNamespace("yaml", quietly = TRUE)) stop("Package 'yaml' is required.")
  yaml::read_yaml(path)
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

trim_na <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

safe_number <- function(x) {
  suppressWarnings(as.numeric(trim_na(x)))
}

write_table <- function(x, path_without_extension) {
  ensure_dir(dirname(path_without_extension))
  data.table::fwrite(x, paste0(path_without_extension, ".csv"), na = "")
  saveRDS(x, paste0(path_without_extension, ".rds"), compress = "xz")
  if (requireNamespace("arrow", quietly = TRUE)) {
    arrow::write_parquet(x, paste0(path_without_extension, ".parquet"), compression = "zstd")
  }
  invisible(x)
}

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

iso_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

append_source_manifest <- function(rows, path = file.path("data", "raw", "source_manifest.csv")) {
  rows <- data.table::as.data.table(rows)
  if (file.exists(path)) {
    existing <- data.table::fread(path, colClasses = "character")
    if ("path" %in% names(existing)) {
      if (!"local_path" %in% names(existing)) existing[, local_path := NA_character_]
      existing[is.na(local_path) | local_path == "", local_path := path]
    }
    if ("retrieved_at" %in% names(existing)) {
      if (!"retrieval_time" %in% names(existing)) existing[, retrieval_time := NA_character_]
      existing[is.na(retrieval_time) | retrieval_time == "", retrieval_time := retrieved_at]
    }
    existing[, c(intersect(c("path", "retrieved_at"), names(existing))) := NULL]
    rows <- data.table::rbindlist(list(existing, rows), use.names = TRUE, fill = TRUE)
  }
  columns <- c("source", "municipality_code", "municipality_name", "snapshot_date", "source_date", "retrieval_time", "url", "local_path", "bytes", "sha256", "row_count", "licence_url", "note")
  for (nm in setdiff(columns, names(rows))) data.table::set(rows, j = nm, value = NA_character_)
  rows[grepl("^Catastro CAT", source) & (is.na(snapshot_date) | snapshot_date == ""), snapshot_date := "2026-08-14"]
  rows <- unique(rows[, ..columns], by = c("local_path", "sha256"), fromLast = TRUE)
  data.table::fwrite(rows, path)
  invisible(rows)
}
