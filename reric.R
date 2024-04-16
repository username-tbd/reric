pcount <- function(x, ..., wt = NULL, sort = TRUE,
                   name = NULL, .drop = group_by_drop_default(x)) {
  df <- count(x, ..., wt = {{ wt }}, sort = sort, name = name, .drop = .drop)
  final_name <- colnames(df)[ncol(df)]
  df |>
    mutate(p = round(100 * .data[[final_name]] / sum(.data[[final_name]]), 2))
}

date_dim_fillout <- function(df, date_var, max_date, ...) {
  df_filler <- df |>
    group_by(...) |>
    reframe({{ date_var }} := seq.Date(from = min({{ date_var }}),
                                       to = max_date,
                                       by = "1 day"))

  df_filler |>
    left_join(df)
}

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

notebook_load <- function(loading_function, file, overwrite = FALSE) {
  if (overwrite || !file.exists(file)) {
    data <- loading_function()
    saveRDS(data, file = file)
    return(data)
  } else {
    return(readRDS(file))
  }
}

query_snowflake_clean <- function(conn, query_file) {
  dbGetQuery(conn, read_file(query_file)) |>
    clean_names()
}
