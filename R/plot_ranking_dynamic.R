#' Plot an animated ranking chart across multiple years
#'
#' Creates an animated horizontal bar chart (racing bar chart) showing
#' how entity composite rankings evolve over time using \pkg{gganimate}.
#'
#' @param composite A data frame returned by \code{\link{compute_composite}},
#'   containing data for at least two years.
#' @param top_n Integer.  Number of top entities to display per frame.
#'   Maximum is 20.  Default is \code{10}.
#' @param fps Integer.  Frames per second for the animation.
#'   Default is \code{5}.
#' @param duration Integer.  Total animation duration in seconds.
#'   Default is \code{10}.
#' @param title Character.  Plot title.  If \code{NULL}, a default is used.
#'
#' @return A \code{gganimate} animation object.  Render it with
#'   \code{gganimate::animate()} or save with
#'   \code{gganimate::anim_save()}.
#'
#' @examples
#' \dontrun{
#' anim <- plot_ranking_dynamic(result, top_n = 10)
#' gganimate::animate(anim, fps = 5, duration = 10)
#' gganimate::anim_save("ranking.gif", anim)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_text coord_flip labs
#'   theme_minimal theme element_text scale_fill_gradient
#'   scale_y_continuous scale_x_reverse
#' @importFrom gganimate transition_states ease_aes enter_fade exit_fade
#' @export
plot_ranking_dynamic <- function(composite,
                                 top_n    = 10,
                                 fps      = 5,
                                 duration = 10,
                                 title    = NULL) {

  if (!requireNamespace("gifski", quietly = TRUE)) {
    stop("Package 'gifski' is required for animation. ",
         "Install it with: install.packages('gifski')")
  }
  if (!is.data.frame(composite)) {
    stop("'composite' must be a data frame from compute_composite().")
  }
  if (!"composite_score" %in% names(composite)) {
    stop("'composite' must contain a 'composite_score' column.")
  }

  top_n <- min(top_n, 20)

  years <- sort(unique(composite$year))
  if (length(years) < 2) {
    stop("At least 2 years of data are required for animation. ",
         "Found: ", paste(years, collapse = ", "))
  }

  df <- do.call(rbind, lapply(years, function(yr) {
    sub <- composite[composite$year == yr &
                       !is.na(composite$composite_rank), ]
    sub <- sub[order(sub$composite_rank), ]
    sub <- head(sub, top_n)
    sub$display_rank <- seq_len(nrow(sub))
    sub
  }))
  df$year <- as.integer(df$year)
  df <- df[, c("entity", "year", "composite_score",
               "composite_rank", "display_rank")]

  all_unis <- unique(df$entity)
  df <- do.call(rbind, lapply(unique(df$year), function(yr) {
    yr_df        <- df[df$year == yr, ]
    missing_unis <- setdiff(all_unis, yr_df$entity)
    if (length(missing_unis) > 0) {
      missing_df <- data.frame(
        entity          = missing_unis,
        year            = yr,
        composite_score = 0,
        composite_rank  = NA_integer_,
        display_rank    = top_n + seq_along(missing_unis),
        stringsAsFactors = FALSE
      )
      yr_df <- rbind(yr_df, missing_df)
    }
    yr_df
  }))

  df <- df[!is.na(df$composite_rank), ]
  if (nrow(df) == 0) stop("No valid data found.")

  if (is.null(title)) {
    title <- paste0("Top ", top_n, " Entities - Composite Ranking")
  }

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x     = display_rank,
      y     = composite_score,
      fill  = -display_rank,
      group = entity
    )
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = entity),
      hjust = 1.05, size = 3.5, color = "white", fontface = "bold"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(composite_score, 1)),
      hjust = -0.1, size = 3.2, color = "grey30"
    ) +
    ggplot2::scale_fill_gradient(
      low   = "#c6dbef",
      high  = "#08306b",
      guide = "none"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, max(df$composite_score, na.rm = TRUE) * 1.15),
      expand = c(0, 0)
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_reverse() +
    ggplot2::labs(
      title = paste0(title, " - {closest_state}"),
      x     = NULL,
      y     = "Composite Score (0-100)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold", size = 14),
      axis.text.y        = ggplot2::element_blank(),
      axis.ticks.y       = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank()
    ) +
    gganimate::transition_states(
      year,
      transition_length = 2,
      state_length      = 3
    ) +
    gganimate::ease_aes("cubic-in-out") +
    gganimate::enter_fade() +
    gganimate::exit_fade()

  message("Use gganimate::animate() to render, ",
          "or gganimate::anim_save() to save.")
  p
}
