data(rankings_sample)

.prep <- function(src) {
  d <- rankings_sample[rankings_sample$source == src, ]
  d$score <- d$score_overall
  d
}

test_that("merge_datasets produces wide format with correct columns", {
  result <- merge_datasets(list(.prep("QS"), .prep("THE")), verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("entity", "year",
                    "rank_QS", "rank_THE",
                    "score_QS", "score_THE") %in% names(result)))
  expect_gt(nrow(result), 0)
})

test_that("merge_datasets handles three sources correctly", {
  result <- merge_datasets(list(.prep("QS"), .prep("THE"), .prep("ARWU")),
                           verbose = FALSE)
  expect_true(all(c("rank_QS", "rank_THE", "rank_ARWU") %in% names(result)))
})

test_that("merge_datasets covers all three years", {
  result <- merge_datasets(list(.prep("QS"), .prep("THE")), verbose = FALSE)
  expect_setequal(unique(result$year), c(2023, 2024, 2025))
})

test_that("merge_datasets stops when given an empty list", {
  expect_error(merge_datasets(list(), verbose = FALSE))
})

test_that("merge_rankings backward-compat wrapper runs without error", {
  result <- merge_rankings(list(.prep("QS"), .prep("ARWU")))
  expect_s3_class(result, "data.frame")
  expect_true(all(c("rank_QS", "rank_ARWU") %in% names(result)))
})
