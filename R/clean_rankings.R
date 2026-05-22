#' Clean and standardize data (Generic)
#'
#' Standardizes column names, cleans text encoding, converts numeric fields.
#' Works with any data type containing entity/item, rank, and score columns.
#'
#' @param df Raw data frame
#' @param entity_patterns Regex patterns to detect entity column (default: "name|item|entity|institution|university")
#' @param rank_patterns Regex patterns to detect rank column (default: "rank|position")
#' @param score_patterns Regex patterns to detect score column (default: "score|total|value|points")
#' @param keep_cols Additional columns to keep (character vector)
#' @param numeric_cols Columns to convert to numeric
#' @param remove_na Remove rows with missing ranks/entities (default: TRUE)
#' @param quiet Suppress progress messages (default: FALSE)
#'
#' @return Cleaned data frame
#' @export
clean_data <- function(df,
                       entity_patterns = "name|item|entity|institution|university",
                       rank_patterns = "rank|position",
                       score_patterns = "score|total|value|points",
                       keep_cols = NULL,
                       numeric_cols = NULL,
                       remove_na = TRUE,
                       quiet = FALSE) {

  df <- as.data.frame(df)
  names(df) <- tolower(names(df))

  if (!quiet) cat("Cleaning data...\n")

  clean_text <- function(x) {
    x <- as.character(x)
    if (requireNamespace("stringi", quietly = TRUE)) {
      x <- stringi::stri_enc_toutf8(x, is_unknown_8bit = TRUE)
      x <- stringi::stri_replace_all_regex(x, "[\\p{C}]", "")
    }
    x <- trimws(x)
    x
  }

  entity_col <- grep(entity_patterns, names(df), ignore.case = TRUE, value = TRUE)[1]
  rank_col   <- grep(rank_patterns, names(df), ignore.case = TRUE, value = TRUE)[1]
  score_col  <- grep(score_patterns, names(df), ignore.case = TRUE, value = TRUE)[1]

  if (is.na(entity_col)) entity_col <- names(df)[1]
  if (is.na(rank_col))   rank_col   <- names(df)[2]

  entity <- clean_text(df[[entity_col]])

  rank_raw <- clean_text(df[[rank_col]])
  if (requireNamespace("stringr", quietly = TRUE)) {
    rank_num <- stringr::str_extract(rank_raw, "\\d+")
  } else {
    rank_num <- sub("^.*?(\\d+).*$", "\\1", rank_raw)
  }
  rank <- suppressWarnings(as.numeric(rank_num))

  score <- NA
  if (!is.na(score_col)) {
    score_raw <- clean_text(df[[score_col]])
    if (requireNamespace("stringr", quietly = TRUE)) {
      score_num <- stringr::str_extract(score_raw, "\\d+\\.?\\d*")
    } else {
      score_num <- sub("^.*?(\\d+\\.?\\d*).*$", "\\1", score_raw)
    }
    score <- suppressWarnings(as.numeric(score_num))
  }

  result <- data.frame(
    entity = entity,
    rank = rank,
    score = score,
    stringsAsFactors = FALSE
  )

  if (!is.null(keep_cols)) {
    for (col in keep_cols) {
      if (col %in% names(df)) {
        if (col %in% names(result)) {
          next
        }
        result[[col]] <- df[[col]]
      }
    }
  }

  if ("source" %in% names(df)) result$source <- df$source
  if ("year" %in% names(df)) result$year <- df$year

  if (!is.null(numeric_cols)) {
    for (col in numeric_cols) {
      if (col %in% names(result)) {
        result[[col]] <- suppressWarnings(as.numeric(result[[col]]))
      }
    }
  }

  if (remove_na) {
    result <- result[!is.na(result$rank) & result$entity != "", ]
  }

  if (!quiet) {
    cat(sprintf("  %d rows kept\n", nrow(result)))
  }

  return(result)
}


#' Clean and standardize ranking data (Backward compatibility)
#'
#' Standardizes column names, cleans text encoding, and converts
#' ranking/score fields into numeric format. Works specifically with ranking data.
#'
#' @param df Raw data frame
#' @param source Source name (optional, used for metadata)
#'
#' @return Cleaned data frame with columns: entity, rank, score, source, year
#' @export
clean_rankings <- function(df, source = NULL) {
  clean_data(df,
             entity_patterns = "name|institution|university",
             rank_patterns = "rank",
             score_patterns = "score|total",
             remove_na = TRUE,
             quiet = FALSE)
}
