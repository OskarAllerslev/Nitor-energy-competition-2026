
training_data <- load_full_dataset()

set.seed(1)
splits <- rsample::initial_time_split(
  data = training_data,
  prop = 0.95
)
training_split <- rsample::training(splits)
testing_split <- rsample::testing(splits)


# fit modeller ----
wrks = 15

tune_model <- function(MARKED = "A", sub_model_name) {
  xgb_opdelt_market(
    MARKED = glue::glue("Market {MARKED}"),
    sub_model_name = sub_model_name,
    training_split = training_split,
    wrks =wrks,
    step = 20
  )
}

sub_model_name <- "andrefolds"
xgb_best_params_A <- tune_model("A", sub_model_name)
xgb_best_params_B <- tune_model("B", sub_model_name)
xgb_best_params_C <- tune_model("C", sub_model_name)
xgb_best_params_D <- tune_model("D", sub_model_name)
xgb_best_params_E <- tune_model("E", sub_model_name)
xgb_best_params_F <- tune_model("F", sub_model_name)

#xgb_best_params_A <- readRDS()

# så skal de fittes ----

get_fit <- function(best_params, MARKED = "A", training_split) {
  train_df <- get_train_df(glue::glue("Market {MARKED}"), training_split = training_split)
  finalize_workflow_helper(best_params, workflow_helper(train_df), train_df)
}

data_for_prediction <- prepare_data_for_prediction(function(x) {final_data_transformation(data.frame(target = 0))(x,F)})

predict_on_market_model <- function(data_for_prediction, MARKED = "A", market_fit, training_split) {
  split <- training_split |> dplyr::filter(market == glue::glue("Market {MARKED}"))
  train_targets_mad <- stats::mad(split$target)
  train_targets_median <- median(split$target)
  inverse_transformatin_function <- Vectorize(inv_asinh_trans(train_targets_mad, train_targets_median))

  data_to_predict_on <- data_for_prediction |> dplyr::filter(market == glue::glue("Market {MARKED}"))

  preds <- predict(market_fit, new_data=data_to_predict_on)
  data_to_predict_on <- data_to_predict_on |>
    dplyr::mutate(target = preds$.pred) |>
    dplyr::mutate(target = inverse_transformatin_function(target))
  return(data_to_predict_on)
}

## Fit on training split to get diagnostics

train_fit_a <- get_fit(xgb_best_params_A, "A", training_split)
train_fit_b <- get_fit(xgb_best_params_B, "B", training_split)
train_fit_c <- get_fit(xgb_best_params_C, "C", training_split)
train_fit_d <- get_fit(xgb_best_params_D, "D", training_split)
train_fit_e <- get_fit(xgb_best_params_E, "E", training_split)
train_fit_f <- get_fit(xgb_best_params_F, "F", training_split)



predictions_A <- predict_on_market_model(data_for_prediction, "A", train_fit_a, training_split)
predictions_B <- predict_on_market_model(data_for_prediction, "B", train_fit_b, training_split)
predictions_C <- predict_on_market_model(data_for_prediction, "C", train_fit_c, training_split)
predictions_D <- predict_on_market_model(data_for_prediction, "D", train_fit_d, training_split)
predictions_E <- predict_on_market_model(data_for_prediction, "E", train_fit_e, training_split)
predictions_F <- predict_on_market_model(data_for_prediction, "F", train_fit_f, training_split)

data.frame(market= c("A","B","C","D","E","F"),
      RMSE=rbind(predictions_A |> extract_fit_subset(testing_split) |> yardstick::rmse(target.x, target.y),
      predictions_B |> extract_fit_subset(testing_split) |> yardstick::rmse(target.x, target.y),
      predictions_C |> extract_fit_subset(testing_split) |> yardstick::rmse(target.x, target.y),
      predictions_D |> extract_fit_subset(testing_split) |> yardstick::rmse(target.x, target.y),
      predictions_E |> extract_fit_subset(testing_split) |> yardstick::rmse(target.x, target.y),
      predictions_F |> extract_fit_subset(testing_split) |> yardstick::rmse(target.x, target.y)))


combined_predictions <- rbind(predictions_A, predictions_B, predictions_C, predictions_D, predictions_E, predictions_F) |> dplyr::arrange(id)
combined_predictions |> extract_fit_subset(testing_split) |> yardstick::rmse(target.x, target.y)



## Fit on full dataset for final submission

# Use training_data instead of training_split to get all data including testing_split
full_fit_a <- get_fit(xgb_best_params_A, "A", training_data)
full_fit_b <- get_fit(xgb_best_params_B, "B", training_data)
full_fit_c <- get_fit(xgb_best_params_C, "C", training_data)
full_fit_d <- get_fit(xgb_best_params_D, "D", training_data)
full_fit_e <- get_fit(xgb_best_params_E, "E", training_data)
full_fit_f <- get_fit(xgb_best_params_F, "F", training_data)

predictions_A_full <- predict_on_market_model(data_for_prediction, "A", full_fit_a, training_data)
predictions_B_full <- predict_on_market_model(data_for_prediction, "B", full_fit_b, training_data)
predictions_C_full <- predict_on_market_model(data_for_prediction, "C", full_fit_c, training_data)
predictions_D_full <- predict_on_market_model(data_for_prediction, "D", full_fit_d, training_data)
predictions_E_full <- predict_on_market_model(data_for_prediction, "E", full_fit_e, training_data)
predictions_F_full <- predict_on_market_model(data_for_prediction, "F", full_fit_f, training_data)

combined_predictions_full <- rbind(predictions_A_full, predictions_B_full, predictions_C_full, predictions_D_full, predictions_E_full, predictions_F_full) |> dplyr::arrange(id)
combined_predictions_full |> extract_data_for_prediction() |> save_results("xgb_opdelt", "andrefolds")
