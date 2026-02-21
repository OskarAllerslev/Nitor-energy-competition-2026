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
final_glmnet_params <- readRDS(file = "inst/glmnet/model/lørdag_eftermiddagglmnet21-02-2026 15-39-37.rds")

glmnet_spec <- parsnip::linear_reg(
  penalty =tune::tune(),
  mixture =tune::tune()
) |>
  parsnip::set_engine("glmnet")

glmnet_wf <- workflows::workflow()  |>
  workflows::add_recipe(initial_recipe)  |>
  workflows::add_model(glmnet_spec)  |>
  tune::finalize_workflow(parameters = final_glmnet_params)

glmnet_fit <- glmnet_wf  |>
  parsnip::fit(data = train_df)



# we make the ensemble ----
models_tibble <- modeltime::modeltime_table(
  glmnet_fit,
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

all_data_transformed <- all_data |>
  dplyr::mutate(target = preds$.value) |>
  dplyr::mutate(target = quantile(train_targets, probs = pnorm(target), na.rm=T))

  all_data_transformed |>
  extract_fit_subset(testing_split) |>
  yardstick::rmse(target.x, target.y)


preds_only <- preds  |>
  dplyr::filter(.key != "actual")  |>
  dplyr::select(.index, .value)

# dette er underligt

preds_only  |> dplyr::filter(is.na(.value))
