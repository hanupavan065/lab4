
#' Visualize Mean Arrival Delays by Airport
#'
#' Calculates mean flight arrival delays for each destination airport
#' and plots the airports by longitude and latitude.
#'
#' @return A ggplot object.
#' @export
visualize_airport_delays <- function() {

  delays <- nycflights13::flights |>
    dplyr::filter(!is.na(arr_delay)) |>
    dplyr::group_by(dest) |>
    dplyr::summarise(
      mean_delay = mean(arr_delay),
      .groups = "drop"
    )

  airports <- nycflights13::airports |>
    dplyr::transmute(
      dest = faa,
      lon,
      lat,
      name
    )

  plot_data <- dplyr::inner_join(
    delays, airports, by = "dest"
  ) |>
    dplyr::filter(
      is.finite(lon),
      is.finite(lat)
    )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = lon,
      y = lat,
      color = mean_delay
    )
  ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0
    ) +
    ggplot2::labs(
      title = "Mean Arrival Delay by Airport",
      x = "Longitude",
      y = "Latitude",
      color = "Delay (minutes)"
    ) +
    ggplot2::theme_minimal()
}
utils::globalVariables(c(
  "arr_delay",
  "dest",
  "faa",
  "lon",
  "lat",
  "name",
  "mean_delay"
))
