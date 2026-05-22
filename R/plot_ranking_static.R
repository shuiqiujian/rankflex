#' Plot a static ranking chart for a given year
#'
#' Creates a horizontal bar chart showing composite scores for the
#' top-ranked entities in a specified year.
#'
#' @param composite A data frame returned by \code{\link{compute_composite}}.
#' @param year Integer.  The year to plot.  If \code{NULL}, the most
#'   recent year available is used.
#' @param top_n Integer.  Number of top entities to display.
#'   Default is \code{20}.
#' @param title Character.  Plot title.  If \code{NULL}, a default
#'   title is generated automatically.
#'
#' @return A \code{ggplot2} object.
#'
#' @examples
#' \dontrun{
#' plot_ranking_static(result, year = 2025, top_n = 20)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_text coord_flip labs
#'   theme_minimal theme element_text scale_y_continuous
#'   scale_fill_gradient
#' @export
plot_ranking_static <- function(composite,
                                year  = NULL,
                                top_n = 20,
                                title = NULL) {

  if (!is.data.frame(composite)) {
    stop("'composite' must be a data frame from compute_composite().")
  }
  if (!"composite_score" %in% names(composite)) {
    stop("'composite' must contain a 'composite_score' column.")
  }

  if (is.null(year)) {
    year <- max(composite$year, na.rm = TRUE)
  }

  df <- composite[composite$year == year &
                    !is.na(composite$composite_rank), ]
  if (nrow(df) == 0) stop("No data found for year ", year, ".")

  df <- df[df$composite_rank <= top_n, ]
  df <- df[order(df$composite_rank), ]
  df$entity <- factor(df$entity, levels = rev(df$entity))

  if (is.null(title)) {
    title <- paste0("Top ", top_n,
                    " Entities - Composite Ranking (", year, ")")
  }

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = entity,
                 y = composite_score,
                 fill = composite_score)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0("#", composite_rank,
                                  "  ", round(composite_score, 1))),
      hjust = -0.05, size = 3.2, color = "grey30"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_gradient(
      low   = "#c6dbef",
      high  = "#08306b",
      guide = "none"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, max(df$composite_score, na.rm = TRUE) * 1.15),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title    = title,
      subtitle = paste("Year:", year),
      x        = NULL,
      y        = "Composite Score (0-100)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle   = ggplot2::element_text(color = "grey50"),
      axis.text.y     = ggplot2::element_text(size = 10),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank()
    )
}
