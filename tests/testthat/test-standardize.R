data(rankings_sample)

qs_df <- rankings_sample[rankings_sample$source == "QS", ]

test_that("get_indicators excludes rank_*, year, and composite columns", {
  inds <- get_indicators(rankings_sample)
  expect_type(inds, "character")
  expect_gt(length(inds), 0)
  expect_false(any(grepl("^rank_", inds)))
  expect_false("year" %in% inds)
  expect_false("composite_score" %in% inds)
  expect_false("composite_rank"  %in% inds)
})

test_that("standardized_scores adds _std columns in 0-100 range", {
  inds   <- c("score_overall", "score_academic_reputation")
  inds   <- inds[inds %in% names(qs_df)]
  result <- standardized_scores(qs_df, indicators = inds)

  std_cols <- paste0(inds, "_std")
  expect_true(all(std_cols %in% names(result)))
  for (col in std_cols) {
    vals <- result[[col]]
    expect_gte(min(vals, na.rm = TRUE), 0)
    expect_lte(max(vals, na.rm = TRUE), 100)
  }
})

test_that("standardized_scores errors when indicator is missing", {
  expect_error(
    standardized_scores(qs_df, indicators = "nonexistent_col"),
    "not present"
  )
})
