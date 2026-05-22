## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 7,
  fig.height = 5,
  warning  = FALSE,
  message  = FALSE
)

## ----load---------------------------------------------------------------------
library(rankflex)
data(rankings_sample)
cat("Dimensions:", nrow(rankings_sample), "rows ×",
    ncol(rankings_sample), "columns\n")
cat("Sources:", paste(unique(rankings_sample$source), collapse = ", "), "\n")
cat("Years  :", paste(sort(unique(rankings_sample$year)),  collapse = ", "), "\n")

## ----glimpse------------------------------------------------------------------
head(rankings_sample[rankings_sample$source == "QS" &
                     rankings_sample$year   == 2025,
                     c("entity", "source", "rank",
                       "score_overall", "score_academic_reputation",
                       "score_teaching")], 5)

## ----load-demo, eval = FALSE--------------------------------------------------
# # Not run — replace with your own file paths
# qs_raw   <- load_rankings("qs_2025.csv",            source = "QS",   year = 2025)
# the_raw  <- load_rankings("THE_2016_to_2026.csv",   source = "THE",  year = 2025)
# arwu_raw <- load_rankings("all_years_combined.csv", source = "ARWU", year = 2025)

## ----clean--------------------------------------------------------------------
dirty <- data.frame(
  university_name = c(" Harvard University ", " Stanford University ", ""),
  ranking         = c("=1", "2", "3-5"),
  total_score     = c("98.5", "97.2", "90+"),
  stringsAsFactors = FALSE
)
cleaned <- clean_data(dirty, quiet = TRUE)
cleaned

## ----merge--------------------------------------------------------------------
qs_long   <- rankings_sample[rankings_sample$source == "QS",   ]
the_long  <- rankings_sample[rankings_sample$source == "THE",  ]
arwu_long <- rankings_sample[rankings_sample$source == "ARWU", ]

qs_long$score   <- qs_long$score_overall
the_long$score  <- the_long$score_overall
arwu_long$score <- arwu_long$score_overall

final_wide <- merge_datasets(
  df_list = list(qs_long, the_long, arwu_long),
  verbose = FALSE
)

cat("Wide table:", nrow(final_wide), "rows ×", ncol(final_wide), "columns\n")
names(final_wide)

## ----merge-preview------------------------------------------------------------
df_2025 <- final_wide[final_wide$year == 2025, ]
complete_2025 <- df_2025[
  !is.na(df_2025$rank_QS) &
  !is.na(df_2025$rank_THE) &
  !is.na(df_2025$rank_ARWU), ]
head(complete_2025[order(complete_2025$rank_QS),
                   c("entity", "rank_QS", "rank_THE", "rank_ARWU")], 10)

## ----standardize--------------------------------------------------------------
qs_df <- rankings_sample[rankings_sample$source == "QS", ]
cat("QS rows:", nrow(qs_df), "\n")
qs_indicators <- get_indicators(qs_df)
cat("QS indicators:\n")
cat(paste(" •", qs_indicators), sep = "\n")

## ----standardize-apply--------------------------------------------------------
chosen <- c(
  "score_overall",
  "score_academic_reputation",
  "score_employer_reputation",
  "score_faculty_student",
  "score_citations_faculty",
  "score_intl_students"
)
chosen <- chosen[chosen %in% names(qs_df)]
qs_std <- standardized_scores(qs_df, indicators = chosen)
std_cols <- paste0(chosen, "_std")
sapply(qs_std[, std_cols, drop = FALSE],
       function(x) round(range(x, na.rm = TRUE), 2))

## ----composite----------------------------------------------------------------
weights <- c(
  score_overall                = 0.25,
  score_academic_reputation    = 0.25,
  score_employer_reputation    = 0.15,
  score_faculty_student        = 0.15,
  score_citations_faculty      = 0.15,
  score_intl_students          = 0.05
)
weights <- weights[names(weights) %in% chosen]
composite_df <- compute_composite(qs_std, weights = weights)
top10 <- composite_df[composite_df$year == 2025 &
                      !is.na(composite_df$composite_rank), ]
top10 <- top10[order(top10$composite_rank), ]
head(top10[, c("entity", "rank", "composite_score", "composite_rank")], 10)

## ----compare------------------------------------------------------------------
comp_2025 <- composite_df[composite_df$year == 2025 &
                           !is.na(composite_df$composite_rank), ]
compared <- compare_rankings(comp_2025, source_ranks = "rank")

under <- compared[compared$label_rank == "under_rated", ]
under <- under[order(under$diff_rank), ]
head(under[, c("entity", "rank", "composite_rank", "diff_rank")], 8)

## ----compare-over-------------------------------------------------------------
over <- compared[compared$label_rank == "over_rated", ]
over <- over[order(over$diff_rank, decreasing = TRUE), ]
head(over[, c("entity", "rank", "composite_rank", "diff_rank")], 8)

## ----sensitivity, message = TRUE----------------------------------------------
sa <- sensitivity_analysis(
  data    = qs_std,
  weights = weights,
  n_sim   = 100,
  top_n   = 15,
  year    = 2025
)
sa

## ----optimize, message = TRUE-------------------------------------------------
# Which indicator set gives Peking University its best possible ranking?
opt <- optimize_weights(
  data       = qs_std,
  target     = "Peking University",
  indicators = chosen,
  year       = 2025,
  max_weight = 0.5,
  top_n      = 20
)
cat("Optimal weights:\n")
print(round(opt$optimal_weights, 4))
cat(sprintf(
  "\nPeking University under optimal weights:\n  Composite score : %.2f\n  Composite rank  : %d\n",
  opt$target_score, opt$target_rank
))

## ----optimize-table-----------------------------------------------------------
head(opt$ranking[order(opt$ranking$composite_rank),
                 c("entity", "composite_score", "composite_rank")], 10)

## ----plot-static, fig.cap = "Top 20 universities by composite score (QS 2025)"----
p <- plot_ranking_static(
  composite_df,
  year  = 2025,
  top_n = 20,
  title = "QS 2025 Top 20 Composite Scores"
)
p

## ----plot-dynamic, eval = FALSE-----------------------------------------------
# # install.packages(c("gganimate", "gifski"))   # if not yet installed
# anim <- plot_ranking_dynamic(composite_df, top_n = 10, fps = 5, duration = 10)
# gganimate::animate(anim, fps = 5, duration = 10)
# gganimate::anim_save("university_ranking_race.gif", anim)

## ----multi-source-------------------------------------------------------------
tri_source <- complete_2025[, c("entity", "year",
                                "rank_QS", "rank_THE", "rank_ARWU",
                                "score_QS", "score_THE", "score_ARWU")]

tri_std <- standardized_scores(
  tri_source,
  indicators = c("score_QS", "score_THE", "score_ARWU")
)

tri_weights <- c(score_QS = 1/3, score_THE = 1/3, score_ARWU = 1/3)
tri_composite <- compute_composite(tri_std, weights = tri_weights)

tri_top <- tri_composite[!is.na(tri_composite$composite_rank), ]
tri_top  <- tri_top[order(tri_top$composite_rank), ]
head(tri_top[, c("entity", "rank_QS", "rank_THE", "rank_ARWU",
                 "composite_score", "composite_rank")], 10)

