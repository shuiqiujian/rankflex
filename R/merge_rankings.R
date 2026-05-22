#' Merge datasets into wide format (Generic)
#'
#' Combines multiple data sources and reshapes into wide format
#' with separate columns for each source. Works with any ranked data.
#'
#' @param df_list List of data frames with 'entity', 'year', 'source', 'rank', 'score' columns
#' @param entity_col Name of the entity column (default: "entity")
#' @param year_col Name of the year column (default: "year")
#' @param source_col Name of the source column (default: "source")
#' @param rank_col Name of the rank column (default: "rank")
#' @param score_col Name of the score column (default: "score")
#' @param save_path Optional path to save results (if NULL, no saving)
#' @param verbose Logical, print progress messages (default: TRUE)
#'
#' @return A wide-format data frame
#' @export
merge_datasets <- function(df_list,
                           entity_col = "entity",
                           year_col = "year",
                           source_col = "source",
                           rank_col = "rank",
                           score_col = "score",
                           save_path = NULL,
                           verbose = TRUE) {

  combined_data <- do.call(rbind, df_list)
  rownames(combined_data) <- NULL

  if (verbose) {
    cat("\n========================================\n")
    cat("Merging datasets...\n")
    cat("========================================\n\n")
    cat(sprintf("Total rows: %d\n", nrow(combined_data)))
  }

  years <- sort(unique(combined_data[[year_col]]))
  sources <- unique(combined_data[[source_col]])

  if (verbose) {
    cat(sprintf("Years: %s\n", paste(years, collapse = ", ")))
    cat(sprintf("Sources: %s\n", paste(sources, collapse = ", ")))
    cat("\nData statistics:\n")
    for (src in sources) {
      src_count <- nrow(combined_data[combined_data[[source_col]] == src, ])
      cat(sprintf("  %s: %d rows\n", src, src_count))
    }
  }

  wide_tables <- list()

  cat("\n========================================\n")
  cat("Creating wide table...\n")
  cat("========================================\n")

  for (yr in years) {
    if (verbose) cat(sprintf("\nProcessing year %s:\n", yr))
    yr_data <- combined_data[combined_data[[year_col]] == yr, ]
    merged_yr <- NULL

    for (src in sources) {
      src_data <- yr_data[yr_data[[source_col]] == src,
                          c(entity_col, rank_col, score_col)]

      if (nrow(src_data) > 0) {
        names(src_data)[names(src_data) == rank_col] <- paste0("rank_", src)
        names(src_data)[names(src_data) == score_col] <- paste0("score_", src)

        if (verbose) {
          cat(sprintf("  %s: %d entities\n", src, nrow(src_data)))
        }

        if (is.null(merged_yr)) {
          merged_yr <- src_data
        } else {
          merged_yr <- merge(merged_yr, src_data, by = entity_col, all = TRUE)
        }
      }
    }

    if (!is.null(merged_yr)) {
      merged_yr[[year_col]] <- yr
      wide_tables[[as.character(yr)]] <- merged_yr

      if (verbose) {
        cat(sprintf("  Merged: %d entities\n", nrow(merged_yr)))
      }
    }
  }

  if (length(wide_tables) > 0) {
    all_cols <- unique(unlist(lapply(wide_tables, names)))
    rank_cols <- sort(grep("^rank_", all_cols, value = TRUE))
    score_cols <- sort(grep("^score_", all_cols, value = TRUE))
    col_order <- c(entity_col, year_col, rank_cols, score_cols)
    col_order <- col_order[col_order %in% all_cols]

    for (i in seq_along(wide_tables)) {
      missing_cols <- setdiff(col_order, names(wide_tables[[i]]))
      for (col in missing_cols) {
        wide_tables[[i]][[col]] <- NA
      }
      wide_tables[[i]] <- wide_tables[[i]][, col_order, drop = FALSE]
    }

    final_wide <- do.call(rbind, wide_tables)
    rownames(final_wide) <- NULL
  } else {
    final_wide <- NULL
  }

  if (is.null(final_wide) || nrow(final_wide) == 0) {
    stop("No data to merge!")
  }

  final_wide <- final_wide[order(final_wide[[year_col]], final_wide[[entity_col]]), ]

  if (verbose) {
    cat("\nWide table creation completed!\n")
    cat(sprintf("   Total rows: %d\n", nrow(final_wide)))
    cat(sprintf("   Total columns: %d\n", ncol(final_wide)))
  }

  if (!is.null(save_path)) {
    if (verbose) cat("\n========================================\n")
    if (verbose) cat("Saving results...\n")
    if (verbose) cat("========================================\n\n")

    if (!dir.exists(save_path)) dir.create(save_path, recursive = TRUE)

    for (src in sources) {
      src_data <- combined_data[combined_data[[source_col]] == src, ]
      src_file <- file.path(save_path, paste0(src, "_merged.csv"))
      write.csv(src_data, src_file, row.names = FALSE)
      if (verbose) cat(sprintf("Saved: %s\n", basename(src_file)))
    }

    wide_file <- file.path(save_path, "merged_wide_table.csv")
    write.csv(final_wide, wide_file, row.names = FALSE)
    if (verbose) cat(sprintf("Saved: %s\n", basename(wide_file)))
  }

  return(final_wide)
}

#' Merge rankings (backward compatibility wrapper)
#'
#' Wraps \code{\link{merge_datasets}} for backward compatibility.
#'
#' @param df_list List of data frames with entity, year, source, rank, score columns.
#' @return A wide-format data frame.
#' @export
merge_rankings <- function(df_list) {
  merge_datasets(df_list, save_path = NULL, verbose = TRUE)
}
