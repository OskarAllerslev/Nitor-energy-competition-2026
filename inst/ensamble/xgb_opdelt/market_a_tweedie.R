train_df_a <- rsample::training(splits) |> dplyr::filter(market == "Market A")


training_split_a <- train_df_a

offset_val <- abs(min(training_split_a$target, na.rm = TRUE)) + 1
training_split_a$target_shifted <- training_split_a$target + offset_val

glm_model <- glm(
  target_shifted  ~ (solar_forecast * wind_forecast * load_forecast)^2 + wind_speed_80m + apparent_temperature_2m
  + global_horizontal_irradiance^3 ,
  family = statmod::tweedie(var.power = 1.9999, link.power = 0),
  data = training_split_a
)

untransformed_data <- prepare_data_for_prediction(function(x) {x})

preds <- predict(glm_model, newdata = untransformed_data, type = "response")

predictions_A <- untransformed_data |>  dplyr::mutate(target = preds -offset_val)

predictions_A |> extract_fit_subset(testing_split |> dplyr::filter(market == "Market A")) |> yardstick::rmse(target.x, target.y)



# testing_split$pred_glm <- preds - offset_val
#
# mu_train <- mean(training_split$target, na.rm = TRUE)
#
# eval_results <- testing_split |>
#   dplyr::mutate(baseline_pred = mu_train) |>
#   dplyr::summarise(
#     rmse_glm = yardstick::rmse_vec(target, pred_glm),
#     rmse_baseline = yardstick::rmse_vec(target, baseline_pred)
#   )

