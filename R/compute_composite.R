#' Compute Composite Score and Rank
#'
#' Computes weighted composite score and final rank using standardized scores.
#'
#' @param df Data frame with _std columns
#' @param weights Named numeric vector of weights (will be auto-normalized to sum to 1)
#' @param years Optional integer or vector of years to filter before computing
#' @param top_n Optional integer: only return entities ranked within top_n per year
#' @return Data frame with composite_score and composite_rank
#' @export
compute_composite <- function(df, weights, years = NULL, top_n = NULL) {
  if (!is.null(years)) {
    df <- df[df$year %in% years, ]
  }

  indicators <- names(weights)
  std_cols   <- paste0(indicators, "_std")

  if (!all(std_cols %in% colnames(df))) {
    stop("Please run standardized_scores() first.")
  }

  weights <- weights / sum(weights)

  df$composite_score <- 0
  for (i in seq_along(indicators)) {
    df$composite_score <- df$composite_score + weights[i] * df[[std_cols[i]]]
  }

  df <- df %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(composite_rank = dplyr::min_rank(-composite_score)) %>%
    dplyr::ungroup()

  if (!is.null(top_n)) {
    df <- df[!is.na(df$composite_rank) & df$composite_rank <= top_n, ]
  }

  return(df)
}
