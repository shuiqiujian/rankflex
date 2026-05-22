#' Perform sensitivity analysis on composite ranking weights
#'
#' Tests how stable an entity's composite ranking is when weights are
#' randomly perturbed around the base values. Entities with low rank
#' standard deviation are considered robust regardless of the exact
#' weighting scheme chosen.
#'
#' @param data A normalized data frame from \code{\link{standardized_scores}}.
#' @param weights A named numeric vector of base weights (same format as
#'   \code{\link{compute_composite}}).
#' @param n_sim Integer. Number of random weight perturbations to simulate.
#'   Default is 100. Must be at least 10.
#' @param perturb Numeric. Maximum relative perturbation applied to each
#'   weight. Default is 0.2 (plus or minus 20 percent).
#' @param top_n Integer. Only analyse entities ranked within this threshold
#'   in the base ranking. Default is 50.
#' @param year Integer. Which year to run the analysis on. If NULL, the
#'   most recent year is used.
#'
#' @return A data frame with columns \code{entity},
#'   \code{base_rank}, \code{mean_rank}, \code{sd_rank},
#'   \code{min_rank}, \code{max_rank}, and \code{rank_range},
#'   sorted by \code{base_rank}.
#'
#' @examples
#' \dontrun{
#' weights <- c(
#'   QS_score_academic_reputation = 0.25,
#'   QS_score_citations_faculty   = 0.25,
#'   THE_score_teaching           = 0.25,
#'   ARWU_score_pub               = 0.25
#' )
#' sa <- sensitivity_analysis(normalized, weights, n_sim = 100, top_n = 30)
#' print(sa)
#' }
#'
#' @export
sensitivity_analysis <- function(data,
                                 weights,
                                 n_sim   = 100,
                                 perturb = 0.2,
                                 top_n   = 50,
                                 year    = NULL) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame from standardize_scores().")
  }
  if (!is.numeric(weights) || is.null(names(weights))) {
    stop("'weights' must be a named numeric vector.")
  }
  if (n_sim < 10) {
    stop("'n_sim' must be at least 10.")
  }

  if (is.null(year)) {
    year <- max(data$year, na.rm = TRUE)
    message("Using year: ", year)
  }

  df_yr <- data[data$year == year, ]
  if (nrow(df_yr) == 0) {
    stop("No data found for year ", year, ".")
  }

  base_result <- compute_composite(data, weights = weights, years = year, top_n = top_n)
  base_ranks  <- stats::setNames(base_result$composite_rank, base_result$entity)
  unis        <- names(base_ranks)

  message("Running ", n_sim, " simulations...")

  sim_ranks <- matrix(
    NA_real_,
    nrow     = length(unis),
    ncol     = n_sim,
    dimnames = list(unis, NULL)
  )

  for (i in seq_len(n_sim)) {
    noise       <- stats::runif(length(weights), -perturb, perturb)
    w_perturbed <- weights * (1 + noise)
    w_perturbed <- pmax(w_perturbed, 0)


    if (sum(w_perturbed) == 0) next
    w_perturbed <- w_perturbed / sum(w_perturbed)

    sim_result <- tryCatch(
      compute_composite(data, weights = w_perturbed, years = year, top_n = top_n * 2),
      error = function(e) NULL
    )
    if (is.null(sim_result)) next

    for (uni in unis) {
      idx <- sim_result$entity == uni
      if (any(idx)) {
        sim_ranks[uni, i] <- sim_result$composite_rank[which(idx)[1]]
      }
    }
  }

  result <- data.frame(
    entity    = unis,
    base_rank = as.integer(base_ranks[unis]),
    mean_rank = round(apply(sim_ranks, 1, mean, na.rm = TRUE), 1),
    sd_rank   = round(apply(sim_ranks, 1, stats::sd, na.rm = TRUE), 1),
    min_rank  = apply(sim_ranks, 1, function(x) min(x, na.rm = TRUE)),
    max_rank  = apply(sim_ranks, 1, function(x) max(x, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )
  result$rank_range <- result$max_rank - result$min_rank
  result <- result[order(result$base_rank), ]
  rownames(result) <- NULL

  message("sensitivity_analysis() done. ",
          "Low sd_rank = stable ranking regardless of weights.")
  result
}
