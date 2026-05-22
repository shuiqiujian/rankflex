#' Compare Composite vs Original Rankings
#'
#' Identifies over-performing or under-performing entities by comparing
#' composite rank and each original source rank.
#'
#' diff = composite_rank - original_rank:
#'   negative -> our composite ranks the entity HIGHER than the source  -> under_rated by source
#'   positive -> our composite ranks the entity LOWER  than the source  -> over_rated  by source
#'
#' @param df Data frame with composite_rank and original source ranks
#' @param source_ranks Vector of original rank column names
#' @return Data frame with difference columns and label (over_rated / under_rated / same)
#' @export
compare_rankings <- function(df, source_ranks) {
  for (src in source_ranks) {
    diff_col  <- paste0("diff_", src)
    label_col <- paste0("label_", src)

    df[[diff_col]] <- df$composite_rank - df[[src]]

    df[[label_col]] <- dplyr::case_when(
      df[[diff_col]] < 0 ~ "under_rated",
      df[[diff_col]] > 0 ~ "over_rated",
      TRUE               ~ "same"
    )
  }
  return(df)
}
