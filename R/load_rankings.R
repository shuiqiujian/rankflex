#' Load ranking data (main core function)
#'
#' Robust file loader for CSV/Excel ranking files with auto encoding detection.
#' Can attach source, year and custom metadata columns.
#'
#' @param file_path Path to input file (CSV or Excel).
#' @param source Source identifier string (e.g., "QS", "THE").
#' @param year Year value to attach to each row.
#' @param sheet Optional Excel sheet name or index.
#' @param metadata Optional named list with extra metadata columns to add.
#' @param skip Number of initial rows to skip when reading file.
#' @param encoding Custom file encoding for reading text files.
#' @param quiet Logical, if TRUE suppress console progress messages.
#'
#' @return A data frame with optional source, year and metadata columns.
#'
#' @export
load_data <- function(file_path,
                      source = NULL,
                      year = NULL,
                      sheet = NULL,
                      metadata = NULL,
                      skip = 0,
                      encoding = NULL,
                      quiet = FALSE) {

  if (!quiet) cat(sprintf("Loading: %s\n", basename(file_path)))

  df <- .safe_read_file(file_path, sheet, skip)

  if (is.null(df) || nrow(df) == 0) {
    stop("Failed to load file: ", file_path)
  }

  if (!is.null(source)) df$source <- source
  if (!is.null(year)) df$year <- year

  if (!is.null(metadata) && is.list(metadata)) {
    for (name in names(metadata)) {
      df[[name]] <- metadata[[name]]
    }
  }

  if (!quiet) {
    cat(sprintf("   %d rows  %d cols\n", nrow(df), ncol(df)))
  }

  return(df)
}


#' Load rankings (simple backward compatibility wrapper)
#'
#' Simplified short version of \code{\link{load_data}} with minimal arguments.
#'
#' @param file_path Path to input file (CSV or Excel).
#' @param source Source identifier string (e.g., "QS", "THE").
#' @param year Year value to attach to each row.
#' @param sheet Optional Excel sheet name or index.
#'
#' @return A data frame with source and year columns added.
#'
#' @export
load_rankings <- function(file_path, source, year, sheet = NULL) {
  load_data(file_path, source = source, year = year, sheet = sheet, quiet = FALSE)
}


#' Internal file reading helper
#'
#' Auto-detect encoding and format for reading ranking files.
#'
#' @keywords internal
.safe_read_file <- function(file_path, sheet = NULL, skip = 0) {

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  ext <- tolower(tools::file_ext(file_path))

  if (ext %in% c("xlsx", "xls")) {
    tryCatch({
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Install readxl: install.packages('readxl')")
      }
      return(readxl::read_excel(file_path, sheet = sheet, skip = skip))
    }, error = function(e) {
      cat(sprintf("  Warning: Excel read failed: %s\n", e$message))
      return(NULL)
    })
  }

  encodings <- c("UTF-8", "GBK", "GB2312", "Latin-1", "CP1252", "BIG5", "UTF-16")

  for (enc in encodings) {
    df <- tryCatch({
      utils::read.csv(
        file_path,
        fileEncoding = enc,
        stringsAsFactors = FALSE,
        skip = skip,
        check.names = FALSE,
        warn = FALSE,
        sep = ",",
        quote = "\"",
        na.strings = c("", "NA", "N/A")
      )
    }, error = function(e) NULL, warning = function(w) NULL)

    if (!is.null(df) && nrow(df) > 0 && ncol(df) > 1) {
      cat(sprintf("   Read successfully with encoding: %s\n", enc))
      return(df)
    }
  }

  alt_seps <- c(";", "\t", "|")
  for (sep in alt_seps) {
    for (enc in encodings) {
      df <- tryCatch({
        utils::read.csv(
          file_path,
          sep = sep,
          fileEncoding = enc,
          stringsAsFactors = FALSE,
          skip = skip,
          check.names = FALSE,
          warn = FALSE,
          quote = "\"",
          na.strings = c("", "NA", "N/A")
        )
      }, error = function(e) NULL, warning = function(w) NULL)

      if (!is.null(df) && nrow(df) > 0 && ncol(df) > 1) {
        cat(sprintf("   Success with sep='%s', encoding=%s\n", sep, enc))
        return(df)
      }
    }
  }

  tryCatch({
    if (requireNamespace("readr", quietly = TRUE)) {
      df <- readr::read_csv(file_path, skip = skip, show_col_types = FALSE)
      if (!is.null(df) && nrow(df) > 0) {
        cat("   readr succeeded\n")
        return(as.data.frame(df))
      }
    }
  }, error = function(e) NULL)

  tryCatch({
    if (requireNamespace("data.table", quietly = TRUE)) {
      cat("  Trying data.table package...\n")
      df <- data.table::fread(file_path, skip = skip)
      if (!is.null(df) && nrow(df) > 0) {
        cat("   data.table succeeded\n")
        return(as.data.frame(df))
      }
    }
  }, error = function(e) NULL)

  cat("  Warning: All encoding methods failed\n")
  return(NULL)
}
