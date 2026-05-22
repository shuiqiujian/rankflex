data(rankings_sample)

qs_df   <- rankings_sample[rankings_sample$source == "QS", ]
qs_inds <- c("score_overall", "score_academic_reputation",
             "score_employer_reputation")
qs_inds <- qs_inds[qs_inds %in% names(qs_df)]
qs_std  <- standardized_scores(qs_df, indicators = qs_inds)
qs_w    <- setNames(rep(1 / length(qs_inds), length(qs_inds)), qs_inds)
qs_comp <- compute_composite(qs_std, weights = qs_w)

test_that("plot_ranking_static returns a ggplot object", {
  p <- plot_ranking_static(qs_comp, year = 2025, top_n = 10)
  expect_s3_class(p, "ggplot")
})

test_that("plot_ranking_static accepts a custom title", {
  p <- plot_ranking_static(qs_comp, year = 2025, top_n = 10,
                           title = "My Custom Title")
  expect_equal(p$labels$title, "My Custom Title")
})

test_that("plot_ranking_static errors when composite_score column is absent", {
  expect_error(
    plot_ranking_static(data.frame(entity = "X", year = 2025), year = 2025),
    "composite_score"
  )
})

test_that("plot_ranking_static errors when year has no data", {
  expect_error(
    plot_ranking_static(qs_comp, year = 1900),
    regexp = "No data found"
  )
})

test_that("plot_ranking_dynamic returns a gganim object", {
  skip_if_not_installed("gganimate")
  skip_if_not_installed("gifski")
  p <- plot_ranking_dynamic(qs_comp, top_n = 5)
  expect_s3_class(p, "gganim")
})

test_that("plot_ranking_dynamic errors with fewer than 2 years of data", {
  skip_if_not_installed("gganimate")
  skip_if_not_installed("gifski")
  single_year <- qs_comp[qs_comp$year == 2025, ]
  expect_error(
    plot_ranking_dynamic(single_year, top_n = 5),
    "2 years"
  )
})
