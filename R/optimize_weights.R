#' Optimize indicator weights to maximize a target entity's composite ranking
#'
#' Uses linear programming (\pkg{lpSolve}) to find a weight vector that
#' maximises the composite score of a specified target entity relative to
#' all other entities in the same year.  In other words, it finds the
#' \dQuote{best possible} weighting scheme for the target.
#'
#' @details
#' The optimization problem is formulated as follows.  Let \eqn{s_{ij}}
#' be the (normalized) indicator score of entity \eqn{i} on indicator
#' \eqn{j}, and let \eqn{t} denote the target entity.  We find weights
#' \eqn{w_j \ge 0} such that:
#' \itemize{
#'   \item \eqn{\sum_j w_j = 1} (weights sum to 1),
#'   \item each individual weight \eqn{w_j \le} \code{max_weight}
#'         (no single indicator dominates),
#'   \item the composite score of the target entity
#'         \eqn{\sum_j w_j s_{tj}} is maximized.
#' }
#'
#' @param data A normalized data frame from \code{\link{standardized_scores}}.
#' @param target A character string giving the exact name of the entity
#'   to optimize for (must match a value in the \code{entity} column).
#' @param indicators A character vector of indicator column names to
#'   include in the optimization.  Use \code{\link{get_indicators}} to
#'   list available names.  If \code{NULL}, all available indicators are
#'   used.
#' @param year Integer.  Which year to optimize.  If \code{NULL}, the
#'   most recent year is used.
#' @param max_weight Numeric in (0, 1].  Upper bound on any single
#'   indicator's weight.  Default is \code{0.5}.  Set to \code{1} to
#'   allow a single indicator to receive all weight.
#' @param top_n Integer.  After finding optimal weights, return the
#'   composite ranking for the top \code{top_n} entities.
#'   Default is \code{50}.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{\code{optimal_weights}}{Named numeric vector of optimal weights
#'       (only indicators with weight > 0.001 are shown).}
#'     \item{\code{target_score}}{The target entity's composite score under
#'       the optimal weights.}
#'     \item{\code{target_rank}}{The target entity's composite rank under
#'       the optimal weights.}
#'     \item{\code{ranking}}{A data frame with the full composite ranking
#'       (top \code{top_n} entities) under the optimal weights.}
#'   }
#'
#' @examples
#' \dontrun{
#' indicators <- get_indicators(normalized)$indicator
#' opt <- optimize_weights(
#'   normalized,
#'   target     = "Peking University",
#'   indicators = indicators,
#'   year       = 2025,
#'   max_weight = 0.4
#' )
#' print(opt$optimal_weights)
#' print(opt$target_rank)
#' head(opt$ranking)
#' }
#'
#' @importFrom lpSolve lp
#' @export
optimize_weights <- function(data,
                             target,
                             indicators  = NULL,
                             year        = NULL,
                             max_weight  = 0.5,
                             top_n       = 50) {

  if (!requireNamespace("lpSolve", quietly = TRUE)) {
    stop("Package 'lpSolve' is required. ",
         "Install it with: install.packages('lpSolve')")
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame from standardize_scores().")
  }
  if (!is.character(target) || length(target) != 1) {
    stop("'target' must be a single character string.")
  }
  if (max_weight <= 0 || max_weight > 1) {
    stop("'max_weight' must be in (0, 1].")
  }


  if (is.null(year)) {
    year <- max(data$year, na.rm = TRUE)
    message("Using year: ", year)
  }
  df <- data[data$year == year, ]
  if (nrow(df) == 0) stop("No data found for year ", year, ".")

  all_score_cols <- names(df)[
    grepl("score", names(df), ignore.case = TRUE) &
      !grepl("_rank$", names(df)) &
      !names(df) %in% c("entity", "year", "source", "rank",
                        "n_sources", "composite_score", "composite_rank")
  ]
  if (is.null(indicators)) {
    indicators <- all_score_cols
  } else {
    missing_ind <- indicators[!indicators %in% names(df)]
    if (length(missing_ind) > 0) {
      stop("The following indicators are not found in data:\n  ",
           paste(missing_ind, collapse = "\n  "))
    }
    indicators <- indicators[indicators %in% all_score_cols]
  }

  if (length(indicators) == 0) {
    stop("No valid indicator columns found. ",
         "Use get_indicators() to list available indicators.")
  }

  if (!target %in% df$entity) {
    close_matches <- df$entity[
      grepl(target, df$entity, ignore.case = TRUE)
    ]
    hint <- if (length(close_matches) > 0) {
      paste0("\nDid you mean one of: ",
             paste(head(close_matches, 5), collapse = ", "), "?")
    } else {
      ""
    }
    stop("Target entity '", target,
         "' not found in data for year ", year, ".", hint)
  }

  score_mat <- as.matrix(df[, indicators, drop = FALSE])
  for (j in seq_len(ncol(score_mat))) {
    na_idx <- is.na(score_mat[, j])
    if (any(na_idx)) {
      med <- stats::median(score_mat[, j], na.rm = TRUE)
      score_mat[na_idx, j] <- if (is.na(med)) 0 else med
    }
  }

  target_idx    <- which(df$entity == target)[1]
  target_scores <- score_mat[target_idx, ]
  n_ind         <- length(indicators)


  f.obj <- target_scores

  f.con <- rbind(
    rep(1, n_ind),
    diag(n_ind)
  )
  f.dir <- c("=",  rep("<=", n_ind))
  f.rhs <- c(1,    rep(max_weight, n_ind))

  lp_result <- lpSolve::lp(
    direction    = "max",
    objective.in = f.obj,
    const.mat    = f.con,
    const.dir    = f.dir,
    const.rhs    = f.rhs
  )

  if (lp_result$status != 0) {
    stop("LP solver did not find a feasible solution. ",
         "Try relaxing 'max_weight' or reducing the indicator set.")
  }

  optimal_w <- lp_result$solution
  names(optimal_w) <- indicators


  ranking <- compute_composite(
    data,
    weights = optimal_w,
    years   = year,
    top_n   = top_n
  )

  target_row  <- ranking[ranking$entity == target, ]
  target_score <- if (nrow(target_row) > 0) target_row$composite_score[1] else NA
  target_rank  <- if (nrow(target_row) > 0) target_row$composite_rank[1]  else NA

  nonzero_w <- optimal_w[optimal_w > 0.001]
  nonzero_w <- sort(nonzero_w, decreasing = TRUE)

  message("optimize_weights() done.")
  message("  Target: ", target)
  message("  Year:   ", year)
  message("  Optimal rank:  ", target_rank)
  message("  Optimal score: ", round(target_score, 2))
  message("  Non-zero weights (", length(nonzero_w), "/", n_ind, "):")
  for (nm in names(nonzero_w)) {
    message("    ", sprintf("%-45s %.4f", nm, nonzero_w[nm]))
  }

  list(
    optimal_weights = nonzero_w,
    target_score    = target_score,
    target_rank     = target_rank,
    ranking         = ranking
  )
}
