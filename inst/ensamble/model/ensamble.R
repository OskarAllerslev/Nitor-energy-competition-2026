final_xgb <- final_xgb_fit







# make sub model ----
library(modeltime)
library(modeltime.ensemble)


## prophet ----
# model_spec_prophet <- modeltime::prophet_reg()  |>
#   parsnip::set_engine("prophet")
#
# wflw_fit_prophet <- workflows::workflow()  |>
#   workflows::add_model(model_spec_prophet)   |>
#   workflows::add_recipe(
#     initial_recipe  |> recipes::step_rm(recipes::all_predictors())
#   )  |>
#   parsnip::fit(train_df)

## EN ----
model_spec_glmnet <- parsnip::linear_reg(
  mixture = 0.9,
  penalty = 4.36e-6
)  |>
  parsnip::set_engine("glmnet")

wflw_fit_glmnet <- workflows::workflow()   |>
  workflows::add_model(model_spec_glmnet)  |>
  workflows::add_recipe(initial_recipe)  |>
  parsnip::fit(train_df)



# we make the ensemble ----
models_tibble <- modeltime::modeltime_table(
  wflw_fit_glmnet,
  # wflw_fit_prophet,
  final_xgb_fit
)

## make the ensemble ----
ensemble_fit <- models_tibble  |>
  modeltime.ensemble::ensemble_average(type = "median")


## calibration ----
calibration_tbl <- modeltime::modeltime_table(
  ensemble_fit
)  |>
  modeltime::modeltime_calibrate(test_df)


# accuracy ----
calibration_tbl  |>
  modeltime::modeltime_accuracy()  |>
  modeltime::table_modeltime_accuracy(.interactive = F)



all_data <- prepare_data_for_prediction(function(x) {data_transformation_to_use(x,F)})

preds <- calibration_tbl  |>
  modeltime::modeltime_forecast(
    new_data = all_data
  )

all_data_transformed <- all_data |> dplyr::mutate(target = preds$.value) |>

  dplyr::mutate(target = quantile(train_targets, probs = pnorm(target), na.rm=T)) |>
  extract_fit_subset(testing_split) |>
  yardstick::rmse(target.x, target.y)


preds_only <- preds  |>
  dplyr::filter(.key != "actual")  |>
  dplyr::select(.index, .value)

# dette er underligt

preds_only  |> dplyr::filter(is.na(.value))
