

#' data_transform
#'
#' @training_data training_data from the function
#' @test_data_bool indicates if we have target or not default it taht we have target
#' @export
#' @returns tibble
data_transform_v1 <- function(
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
      # time curve
      days_since_start = base::as.numeric(
        base::difftime(
          delivery_start,
          min(training_data$delivery_start),
          units = "days"
        )
      ),
      surface_pressure_lag_24 = dplyr::lag(surface_pressure, n = 24),
      recency_weight = hardhat::importance_weights(exp(days_since_start / 180)),
      wind_speed_80m = wind_speed_80m^3,
      wind_dir_sin = sin(wind_direction_80m * (pi / 180)),
      wind_dir_cos = cos(wind_direction_80m * (pi / 180)),
      temp_index = pmax(0, wet_bulb_temperature_2m - 22) +
        pmax(0, 18 - air_temperature_2m),
      convective_threat = convective_available_potential_energy *
        (1 / (convective_inhibition + 0.01)) *
        cloud_cover_high,
      icing_risk = dplyr::if_else(
        freezing_level_height < 150 & relative_humidity_2m > 90,
        1,
        0
      )
    ) |>
    dplyr::arrange(id, delivery_start) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(id))  |>
    dplyr::select(-days_since_start) # tilføj evt senere

  return(result)
}


data_transform_notargetadjust <- function(
    training_data,
    test_data_bool
) {

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
      # time curve
      days_since_start = base::as.numeric(
        base::difftime(
          delivery_start,
          min(training_data$delivery_start),
          units = "days"
        )
      ),
      surface_pressure_lag_24 = dplyr::lag(surface_pressure, n = 24),
      recency_weight = hardhat::importance_weights(exp(days_since_start / 180)),
      wind_speed_80m = wind_speed_80m^3,
      wind_dir_sin = sin(wind_direction_80m * (pi / 180)),
      wind_dir_cos = cos(wind_direction_80m * (pi / 180)),
      temp_index = pmax(0, wet_bulb_temperature_2m - 22) +
        pmax(0, 18 - air_temperature_2m),
      convective_threat = convective_available_potential_energy *
        (1 / (convective_inhibition + 0.01)) *
        cloud_cover_high,
      icing_risk = dplyr::if_else(
        freezing_level_height < 150 & relative_humidity_2m > 90,
        1,
        0
      )
    ) |>
    dplyr::arrange(id, delivery_start) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(id))  |>
    dplyr::select(-days_since_start) # tilføj evt senere

  return(result)
}

final_data_transformation <- function(training_split) {
  train_targets_mad <- stats::mad(training_split$target)
  train_targets_median <- median(training_split$target)
  function(df, modifyTarget = T) {
  result <- df |>
    #Fill out missing market rows to make sure lags take from the correct time and don't go more hours back than desired.
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
      air_density          = (surface_pressure * 100) / (287.05 * (air_temperature_2m + 273.15)),
      air_density_lag_1 = dplyr::lag(air_density, n = 1),
      air_density_lag_24 = dplyr::lag(air_density, n = 24),
      air_density_ma_6h = slider::slide_dbl(
        .x = air_density,
        .f = mean,
        .before = 6,
        .complete = FALSE
      ),
      wind_speed_80m_lag_1 = dplyr::lag(wind_speed_80m, n = 1),
      wind_speed_80m_lag_24 = dplyr::lag(wind_speed_80m, n = 1),
      wind_speed_80m_ma_6h = slider::slide_dbl(
        .x = wind_speed_80m,
        .f = mean,
        .before = 6,
        .complete = FALSE
      ),
      residual_load = load_forecast - solar_forecast - wind_forecast,
      residual_load_forecast_lag_1 = dplyr::lag(residual_load, n = 1),
      residual_load_forecast_lag_24 = dplyr::lag(residual_load, n = 24),
      residual_load_forecast_lag_48 = dplyr::lag(residual_load, n = 48),
      residual_load_ma_6h = slider::slide_dbl(
        .x = residual_load,
        .f = mean,
        .before = 6,
        .complete = FALSE
      ),
      residual_load_lag_ratio = residual_load_forecast_lag_1/residual_load_forecast_lag_24,
      wind_dir_sin = sin(wind_direction_80m * (pi / 180)),
      wind_dir_cos = cos(wind_direction_80m * (pi / 180)),
      temp_index = pmax(0, wet_bulb_temperature_2m - 22) +
        pmax(0, 18 - air_temperature_2m)
    ) |>
    dplyr::select(
      -c(
        wind_direction_80m,
        cloud_cover_low,
        cloud_cover_mid,
        cloud_cover_high,
        dew_point_temperature_2m,
        wet_bulb_temperature_2m,
        relative_humidity_2m,
        wind_gust_speed_10m,
        convective_inhibition,
        lifted_index
      )
    )

  if (modifyTarget) {
    result <- result |> dplyr::mutate(target = asinh((target - train_targets_median) / train_targets_mad))
  }
  result <- result |>
    dplyr::arrange(id, delivery_start) |>
    dplyr::ungroup()
  result <- result |>
    dplyr::filter(!is.na(id))

  return(result)
}
}






inv_asinh_trans <- function(mad, median) {
  function(t) {
  sinh(t) * mad + median
}
}





