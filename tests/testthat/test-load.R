test_that("load_data reads CSV and attaches source/year metadata", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(institution = c("Uni A", "Uni B"),
                       rank = c(1, 2), score = c(95, 90)),
            tmp, row.names = FALSE)
  on.exit(unlink(tmp))

  result <- load_data(tmp, source = "TEST", year = 2025, quiet = TRUE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("source", "year") %in% names(result)))
  expect_equal(unique(result$source), "TEST")
  expect_equal(unique(result$year), 2025)
})

test_that("load_data attaches arbitrary metadata columns", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(name = "Uni X", rank = 1), tmp, row.names = FALSE)
  on.exit(unlink(tmp))

  result <- load_data(tmp, source = "QS", year = 2024,
                      metadata = list(region = "Asia"), quiet = TRUE)
  expect_true("region" %in% names(result))
  expect_equal(result$region, "Asia")
})

test_that("load_data stops on a non-existent file", {
  expect_error(load_data("no_such_file.csv", quiet = TRUE), "File not found")
})

test_that("load_rankings backward-compat wrapper runs and adds metadata", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(institution = c("Uni X", "Uni Y"),
                       rank = c(5, 10), score = c(80, 70)),
            tmp, row.names = FALSE)
  on.exit(unlink(tmp))

  result <- load_rankings(tmp, source = "WRAP", year = 2024)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(unique(result$source), "WRAP")
})
