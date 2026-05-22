set.seed(123)
n_unis <- 20
syn_df <- data.frame(
  entity        = rep(paste0("University_", LETTERS[seq_len(n_unis)]), 3),
  year          = rep(c(2023L, 2024L, 2025L), each = n_unis),
  source        = "TEST",
  rank          = rep(seq_len(n_unis), 3),
  score_acad    = runif(n_unis * 3, 50, 100),
  score_employ  = runif(n_unis * 3, 40,  95),
  score_cite    = runif(n_unis * 3, 30,  90),
  stringsAsFactors = FALSE
)
syn_inds <- get_indicators(syn_df)
syn_std  <- standardized_scores(syn_df, indicators = syn_inds)
syn_w    <- setNames(rep(1 / length(syn_inds), length(syn_inds)), syn_inds)

test_that("sensitivity_analysis returns expected columns and structure", {
  result <- sensitivity_analysis(syn_std, weights = syn_w,
                                 n_sim = 20, top_n = 10, year = 2025)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("entity", "base_rank", "mean_rank",
                    "sd_rank", "min_rank", "max_rank",
                    "rank_range") %in% names(result)))
  expect_lte(nrow(result), 10)
  expect_true(all(result$sd_rank >= 0, na.rm = TRUE))
  expect_true(all(result$rank_range >= 0, na.rm = TRUE))
  expect_equal(result$base_rank, sort(result$base_rank))
})

test_that("sensitivity_analysis errors when n_sim < 10", {
  expect_error(
    sensitivity_analysis(syn_std, weights = syn_w, n_sim = 5, year = 2025),
    "n_sim"
  )
})

test_that("sensitivity_analysis errors when year has no data", {
  expect_error(
    sensitivity_analysis(syn_std, weights = syn_w, n_sim = 10, year = 1999),
    "No data found"
  )
})

test_that("optimize_weights returns list with correct elements", {
  result <- optimize_weights(syn_std, target = "University_A",
                             indicators = syn_inds,
                             year = 2025, max_weight = 0.5, top_n = 10)
  expect_type(result, "list")
  expect_named(result, c("optimal_weights", "target_score",
                         "target_rank", "ranking"))
})

test_that("optimize_weights optimal_weights sum to ~1", {
  result <- optimize_weights(syn_std, target = "University_A",
                             indicators = syn_inds, year = 2025,
                             max_weight = 0.5, top_n = 10)
  expect_lte(sum(result$optimal_weights), 1 + 1e-6)
  expect_gt( sum(result$optimal_weights), 0)
})

test_that("optimize_weights respects max_weight upper bound", {
  result <- optimize_weights(syn_std, target = "University_A",
                             indicators = syn_inds, year = 2025,
                             max_weight = 0.4, top_n = 10)
  expect_true(all(result$optimal_weights <= 0.4 + 1e-6))
})

test_that("optimize_weights errors on unknown target entity", {
  expect_error(
    optimize_weights(syn_std, target = "Ghost University",
                     indicators = syn_inds, year = 2025),
    "not found"
  )
})

test_that("optimize_weights errors when max_weight is out of (0, 1]", {
  expect_error(
    optimize_weights(syn_std, target = "University_A",
                     indicators = syn_inds, year = 2025, max_weight = 0),
    "max_weight"
  )
})
