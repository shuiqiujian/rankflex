data(rankings_sample)

qs_df   <- rankings_sample[rankings_sample$source == "QS", ]
qs_inds <- c("score_overall", "score_academic_reputation",
             "score_employer_reputation")
qs_inds <- qs_inds[qs_inds %in% names(qs_df)]
qs_std  <- standardized_scores(qs_df, indicators = qs_inds)
qs_w    <- setNames(rep(1 / length(qs_inds), length(qs_inds)), qs_inds)

test_that("compute_composite adds composite_score and composite_rank", {
  result <- compute_composite(qs_std, weights = qs_w, years = 2025)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("composite_score", "composite_rank") %in% names(result)))
  expect_gt(nrow(result), 0)
  expect_equal(min(result$composite_rank, na.rm = TRUE), 1L)
  expect_type(result$composite_rank, "integer")
})

test_that("compute_composite top_n argument limits output rows", {
  result <- compute_composite(qs_std, weights = qs_w, years = 2025, top_n = 10)
  expect_lte(nrow(result), 10)
  expect_true(all(result$composite_rank <= 10, na.rm = TRUE))
})

test_that("compute_composite auto-normalizes weights (sum != 1 is fine)", {
  big_w   <- qs_w * 3
  r_big   <- compute_composite(qs_std, weights = big_w,   years = 2025)
  r_norm  <- compute_composite(qs_std, weights = qs_w,    years = 2025)
  expect_equal(r_big$composite_rank, r_norm$composite_rank)
})

test_that("compute_composite errors when _std columns are absent", {
  expect_error(
    compute_composite(qs_df, weights = qs_w, years = 2025),
    "standardized_scores"
  )
})

test_that("compare_rankings adds diff and label columns", {
  comp <- compute_composite(qs_std, weights = qs_w, years = 2025)
  result <- compare_rankings(comp, source_ranks = "rank")
  expect_true(all(c("diff_rank", "label_rank") %in% names(result)))
  expect_true(all(result$label_rank %in% c("under_rated", "over_rated", "same")))
})

test_that("compare_rankings label logic is correct", {
  comp <- compute_composite(qs_std, weights = qs_w, years = 2025)
  result <- compare_rankings(comp, source_ranks = "rank")
  result_clean <- result[!is.na(result$diff_rank), ]

  under <- result_clean[result_clean$label_rank == "under_rated", ]
  over  <- result_clean[result_clean$label_rank == "over_rated",  ]
  same  <- result_clean[result_clean$label_rank == "same",        ]

  if (nrow(under) > 0) expect_true(all(under$diff_rank < 0))
  if (nrow(over)  > 0) expect_true(all(over$diff_rank  > 0))
  if (nrow(same)  > 0) expect_true(all(same$diff_rank  == 0))
})
