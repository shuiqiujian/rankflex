#' Min-Max Normalization to 0-100 Scale
#'
#' This function takes a merged rankings data frame and applies Min-Max scaling
#' to map all indicator scores to a 0-100 range.
#'
#' @param df A data frame from merge_rankings()
#' @param indicators Vector of indicator column names to standardize
#' @return Data frame with standardized columns (ending in _std)
#' @export
standardized_scores <- function(df, indicators) {
  if (!all(indicators %in% colnames(df))) {
    stop("Some indicators are not present in the data frame.")
  }

  for (ind in indicators) {
    vec <- df[[ind]]
    min_val <- min(vec, na.rm = TRUE)
    max_val <- max(vec, na.rm = TRUE)
    df[[paste0(ind, "_std")]] <- 100 * (vec - min_val) / (max_val - min_val)
  }

  return(df)
}
