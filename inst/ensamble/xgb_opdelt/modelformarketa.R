model_name <- model_name <- "xgb_Market A"
sub_model_name <- "seperatemodel"


market_a_transformation <- function(training_split) {
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

training_split_a <- rsample::training(splits) |> dplyr::filter(market == "Market A")
train_df_a <-   market_a_transformation(training_split_a)(training_split_a, F)

wkf_a <- workflow_helper(train_df_a)

df <- data.frame(mtry = 10, trees= 10000, min_n = 35, tree_depth = 5, learn_rate = 0.00182, .config = "pre0_mod27_post0")

final_market_a_wf <- wkf_a  |>
  tune::finalize_workflow(df)


## fit model på træningssættet ----
set.seed(1)
final_model_a_fit <- final_market_a_wf  |>
  parsnip::fit(data = train_df_a)


## predicitons ----
fit_on_all <- fit_final_model_on_all_data(model = final_model_a_fit,
                                          inverse_prediction_transformation = function(x) {x},
                                          #inverse_prediction_transformation = inv_asinh_trans(stats::mad(training_split_a$target), median(training_split_a$target)),
                                          data_transformation_function = function(x) {market_a_transformation(training_split)(x, F)})

extract_fit_subset(fit_on_all, testing_split |> dplyr::filter(market == "Market A")) |> yardstick::rmse(target.x, target.y)
extract_fit_subset(fit_on_all, training_split) |> yardstick::rmse(target.x, target.y)


# full fit

set.seed(1)
final_xgb_fit_full <- final_xgb_wf  |>
  parsnip::fit(data = training_data |> data_transformation_to_use())

fit_on_all <- fit_final_model_on_all_data(model = final_xgb_fit,
                                          inverse_prediction_transformation = function(x) {x},
                                          data_transformation_function = function(x) {data_transformation_to_use(x, F)})
fit_on_all <- fit_on_all |> dplyr::mutate(target = quantile(train_targets, probs = pnorm(target), na.rm=T))

extract_data_for_prediction(fit_on_all) |> save_results(model_name, postfix = sub_model_name)





