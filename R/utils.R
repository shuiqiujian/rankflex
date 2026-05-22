#' @importFrom magrittr %>%
#' @importFrom utils head write.csv read.csv
#' @importFrom stats runif sd setNames median
#' @importFrom dplyr group_by mutate ungroup min_rank case_when
#' @importFrom ggplot2 ggplot aes geom_col geom_text coord_flip labs
#'   theme_minimal theme element_text scale_y_continuous scale_fill_gradient
#'   scale_x_reverse element_blank element_line
#' @importFrom gganimate transition_states ease_aes enter_fade exit_fade
#' @importFrom lpSolve lp
NULL

utils::globalVariables(c(
  "year", "composite_score", "composite_rank",
  "display_rank", "entity"
))
