
training_data <- load_full_dataset()

set.seed(1)
splits <- rsample::initial_time_split(
  data = training_data,
  prop = 0.95
)
training_split <- rsample::training(splits) |> dplyr::filter(delivery_start >= "2023-10-01")
testing_split <- rsample::testing(splits)


# fit modeller ----
wrks = 15

tune_model <- function(MARKED = "A", sub_model_name, training_split, variance_damping = T) {
  xgb_opdelt_market(
    MARKED = glue::glue("Market {MARKED}"),
    sub_model_name = sub_model_name,
    training_split = training_split,
    wrks =wrks,
    step = 50,
    variance_damping = variance_damping
  )
}

sub_model_name <- "shorthorizonandtweedieforA"
#xgb_best_params_A <- tune_model("A", sub_model_name, training_split)
# xgb_best_params_B <- tune_model("B", sub_model_name, training_split)
# xgb_best_params_C <- tune_model("C", sub_model_name, training_split)
# xgb_best_params_D <- tune_model("D", sub_model_name, training_split)
# xgb_best_params_E <- tune_model("E", sub_model_name, training_split)
# xgb_best_params_F <- tune_model("F", sub_model_name, training_split)


#Final parameters after tuning
xgb_best_params_A <- readRDS("./inst/xgb_Market A/model/best_params_onebigfoldfirstperiodremoveddoneproperlyxgb_Market A22-02-2026 16-48-18.rds")
xgb_best_params_B <- readRDS("./inst/xgb_Market B/model/best_params_onebigfoldfirstperiodremoveddoneproperlyxgb_Market B22-02-2026 16-49-20.rds")
xgb_best_params_C <- readRDS("./inst/xgb_Market C/model/best_params_onebigfoldfirstperiodremoveddoneproperlyxgb_Market C22-02-2026 16-50-21.rds")
xgb_best_params_D <- readRDS("./inst/xgb_Market D/model/best_params_onebigfoldfirstperiodremoveddoneproperlyxgb_Market D22-02-2026 16-51-21.rds")
xgb_best_params_E <- readRDS("./inst/xgb_Market E/model/best_params_onebigfoldfirstperiodremoveddoneproperlyxgb_Market E22-02-2026 16-52-24.rds")
xgb_best_params_F <- readRDS("./inst/xgb_Market F/model/best_params_onebigfoldfirstperiodremoveddoneproperlyxgb_Market F22-02-2026 16-53-21.rds")

#xgb_best_params_A <- readRDS()

# så skal de fittes ----

get_fit <- function(best_params, MARKED = "A", training_split, variance_damping = T) {
  train_df <- get_train_df(glue::glue("Market {MARKED}"), training_split = training_split, variance_damping = variance_damping)
  finalize_workflow_helper(best_params, workflow_helper(train_df), train_df)
}

data_for_prediction <- prepare_data_for_prediction(function(x) {final_data_transformation(data.frame(target = 0))(x,F)})

predict_on_market_model <- function(data_for_prediction, MARKED = "A", market_fit, training_split, variance_damping = T) {
  split <- training_split |> dplyr::filter(market == glue::glue("Market {MARKED}"))
  train_targets_mad <- stats::mad(split$target)
  train_targets_median <- median(split$target)
  inverse_transformatin_function <- Vectorize(inv_asinh_trans(train_targets_mad, train_targets_median))

  data_to_predict_on <- data_for_prediction |> dplyr::filter(market == glue::glue("Market {MARKED}"))

  preds <- predict(market_fit, new_data=data_to_predict_on)
  data_to_predict_on <- data_to_predict_on |>
    dplyr::mutate(target = preds$.pred)
  if(variance_damping) {
    data_to_predict_on <- data_to_predict_on |> dplyr::mutate(target = inverse_transformatin_function(target))
  }
  return(data_to_predict_on)
}

## Fit on training split to get diagnostics
train_fit_b <- get_fit(xgb_best_params_B, "B", training_split)
train_fit_c <- get_fit(xgb_best_params_C, "C", training_split)
train_fit_d <- get_fit(xgb_best_params_D, "D", training_split)
train_fit_e <- get_fit(xgb_best_params_E, "E", training_split)
train_fit_f <- get_fit(xgb_best_params_F, "F", training_split)

#Seperate model for market A
predictions_A <- market_A_tweedie_model(training_split)  |> dplyr::filter(market == "Market A")

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


combined_predictionsNotA <- rbind(predictions_B, predictions_C, predictions_D, predictions_E, predictions_F)
rbind(extract_fit_subset(combined_predictionsNotA, testing_split) |> dplyr::select(id, target.x, target.y),
                          extract_fit_subset(predictions_A, testing_split) |> dplyr::select(id, target.x, target.y))|> yardstick::rmse(target.x, target.y)

## Fit on full dataset for final submission

# Use training_data instead of training_split to get all data including testing_split
full_fit_b <- get_fit(xgb_best_params_B, "B", training_data)
full_fit_c <- get_fit(xgb_best_params_C, "C", training_data)
full_fit_d <- get_fit(xgb_best_params_D, "D", training_data)
full_fit_e <- get_fit(xgb_best_params_E, "E", training_data)
full_fit_f <- get_fit(xgb_best_params_F, "F", training_data)

predictions_A_full <- market_A_tweedie_model(training_data)  |> dplyr::filter(market == "Market A")  |> dplyr::mutate(target = tidyr::replace_na(target, median(training_data$target, na.rm = T)))
predictions_B_full <- predict_on_market_model(data_for_prediction, "B", full_fit_b, training_data)
predictions_C_full <- predict_on_market_model(data_for_prediction, "C", full_fit_c, training_data)
predictions_D_full <- predict_on_market_model(data_for_prediction, "D", full_fit_d, training_data)
predictions_E_full <- predict_on_market_model(data_for_prediction, "E", full_fit_e, training_data)
predictions_F_full <- predict_on_market_model(data_for_prediction, "F", full_fit_f, training_data)

combined_predictions_full_notA <- rbind(predictions_B_full, predictions_C_full, predictions_D_full, predictions_E_full, predictions_F_full)
combined_predictions_full <- rbind(extract_data_for_prediction(combined_predictions_full_notA), extract_data_for_prediction(predictions_A_full)) |> dplyr::arrange(id)



combined_predictions_full |> save_results("xgb_opdelt", sub_model_name)




