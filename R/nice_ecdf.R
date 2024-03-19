#' Provide nice defaults for stat_ecdf().
#'
#' @return A list containing ggplot2 objects.
#' @export
nice_ecdf <- function() {
  list(
    ggplot2::stat_ecdf(),
    ggplot2::scale_y_continuous(
      name = "% Below",
      breaks = seq(from = 0, to = 1, by = 0.1),
      labels = scales::label_percent()
    )
  )
}
