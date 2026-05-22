#' List Available Indicators
#'
#' Returns the names of all numeric score indicators available for composite
#' ranking. Automatically excludes rank columns (rank_*), year, and composite
#' output columns so only genuine indicators are returned.
#'
#' @param df Cleaned and merged ranking data frame
#' @param exclude Additional columns to exclude beyond the defaults
#' @return Vector of indicator column names
#' @export
get_indicators <- function(df,
                           exclude = c("year", "composite_score", "composite_rank")) {
  nums    <- sapply(df, is.numeric)
  all_num <- names(df)[nums]

  rank_cols <- grep("^rank_", all_num, value = TRUE)

  avail <- setdiff(all_num, c(exclude, rank_cols))
  return(avail)
}
