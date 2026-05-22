test_that("clean_data auto-detects columns and cleans rank strings", {
  dirty <- data.frame(
    university_name = c(" Harvard University ", " Stanford University ", ""),
    ranking         = c("=1", "2", "3-5"),
    total_score     = c("98.5", "97.2", "90+"),
    stringsAsFactors = FALSE
  )
  result <- clean_data(dirty, quiet = TRUE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("entity", "rank", "score") %in% names(result)))
  expect_type(result$rank, "double")
  expect_equal(result$rank, c(1, 2))
  expect_equal(trimws(result$entity),
               c("Harvard University", "Stanford University"))
})

test_that("clean_data drops rows with NA rank when remove_na = TRUE", {
  df <- data.frame(name = c("A", "B"), rank = c("1", "abc"),
                   stringsAsFactors = FALSE)
  result <- clean_data(df, quiet = TRUE)
  expect_equal(nrow(result), 1)
})

test_that("clean_data keeps NA-rank rows when remove_na = FALSE", {
  df <- data.frame(name = c("A", "B"), rank = c("1", "abc"),
                   stringsAsFactors = FALSE)
  result <- clean_data(df, remove_na = FALSE, quiet = TRUE)
  expect_equal(nrow(result), 2)
})

test_that("clean_rankings wrapper runs on rankings_sample subset", {
  data(rankings_sample)
  result <- clean_rankings(rankings_sample[1:10, ])
  expect_s3_class(result, "data.frame")
  expect_true(all(c("entity", "rank", "score") %in% names(result)))
})
