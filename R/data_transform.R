

#' data_transform 
#' 
#' @training_data training_data from the function
#' @test_data_bool indicates if we have target or not default it taht we have target
#' @export
#' @returns tibble
data_transform <- function(
  training_data,
  test_data_bool = F
) {
  if (test_data_bool == F) {
    t_median <- median(training_data$target)
    t_mad <- stats::mad(training_data$target)
    training_data <- training_data |>
      dplyr::mutate(
        target = asinh((target - t_median) / t_mad)
      )
  }

  # result <- training_data_initial_transform |>
  result <- training_data |>
    tidyr::complete(
      market,
      delivery_start = seq(
        from = min(delivery_start, na.rm = TRUE),
        to = max(delivery_start, na.rm = TRUE),
        by = "hour"
      )
    ) |>
    dplyr::group_by(market) |>
    dplyr::arrange(market, delivery_start) |>
    dplyr::mutate(
      residual_load = load_forecast - solar_forecast - wind_forecast,
      load_forecast_lag_24 = dplyr::lag(load_forecast, n = 24),
      load_momentum = load_forecast - dplyr::lag(load_forecast, n = 3),
      residual_load_ma_6h = slider::slide_dbl(
        .x = residual_load,
        .f = mean,
        .before = 6,
        .complete = FALSE
      ),
      extreme_load_risk = residual_load^2, 
      # time curve
      days_since_start = base::as.numeric(
        base::difftime(
          delivery_start, 
          min(training_data$delivery_start),
          units = "days"
        )
      ),
      recency_weight = hardhat::importance_weights(exp(days_since_start / 180))
    ) |>
    dplyr::arrange(id, delivery_start) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(id))

  return(result)
}