
model_name <- "fredag_aften_arima"
data_transformation_to_use <- data_transform_v1



# opsætning af data ----
training_data <- load_full_dataset()  |> dplyr::filter(delivery_start > "2025-06-01")





## rsample::rolling_origin ----

set.seed(1)
splits <- rsample::initial_time_split(
  data = training_data,
  prop = 0.85
)
training_split <- rsample::training(splits)
testing_split <- rsample::testing(splits)

train_df_stats <- data.frame(
  median <- median(training_split$target),
  mad <- stats::mad(training_split$target)
)
train_df_test <- data.frame(
  median <- median(testing_split$target),
  mad <- stats::mad(testing_split$target)
)
train_df <- training_split  |>
  data_transformation_to_use()
test_df <- testing_split  |>
  data_transformation_to_use()


# splits  |> 
#   timetk::tk_time_series_cv_plan()  |> 
#   timetk::plot_time_series_cv_plan(delivery_start, target)


initial_recipe <- recipes::recipe(
  target ~ .,
  # data = train_df_engineered
  data = train_df
) |>
  recipes::update_role(id, new_role = "ID") |>
  recipes::step_rm(wind_direction_80m) |>
  recipes::step_dummy(market, one_hot = TRUE) |>
  recipes::step_date(
    delivery_start,
    features = c("dow", "month", "doy", "year"),
    label = FALSE,
    keep_original_cols = TRUE
  ) |>
  recipes::step_time(
    delivery_start,
    features = c("hour"),
    keep_original_cols = TRUE
  ) |>
  recipes::step_harmonic(
    #harmonic på ugedag
    delivery_start_dow,
    frequency = 1,
    cycle_size = 7
  ) |>
  recipes::step_harmonic(
    #harmonic på hour
    delivery_start_hour,
    frequency = 1,
    cycle_size = 24
  ) |>
  recipes::step_harmonic(
    #harmonic på year
    delivery_start_doy,
    frequency = 1,
    cycle_size = 365.25
  ) |>
  recipes::step_naomit(recipes::all_predictors()) |>
  # timetk::step_timeseries_signature(delivery_start)  |> 
  # recipes::step_rm(delivery_start) |>
  recipes::step_rm(delivery_end)

# initial_recipe  |> recipes::prep()  |> recipes::juice()


# make sub model ----
library(modeltime)
library(modeltime.ensemble)


## prophet ----
model_spec_prophet <- modeltime::prophet_reg()  |> 
  parsnip::set_engine("prophet")

wflw_fit_prophet <- workflows::workflow()  |> 
  workflows::add_model(model_spec_prophet)   |> 
  workflows::add_recipe(
    initial_recipe  |> recipes::step_rm(recipes::all_predictors(), -delivery_start)
  )  |> 
  parsnip::fit(train_df)

## EN ----
model_spec_glmnet <- parsnip::linear_reg(
  mixture = 0.9, 
  penalty = 4.36e-6
)  |> 
  parsnip::set_engine("glmnet")

wflw_fit_glmnet <- workflows::workflow()   |> 
  workflows::add_model(model_spec_glmnet)  |> 
  workflows::add_recipe(initial_recipe|>  recipes::step_rm(delivery_start))  |> 
  parsnip::fit(train_df)



# we make the ensemble ----
models_tibble <- modeltime::modeltime_table(
  wflw_fit_glmnet, 
  wflw_fit_prophet
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


preds <- calibration_tbl  |> 
  modeltime::modeltime_forecast(
    actual_data = train_df, 
    new_data = test_df
  )  |> 
  modeltime::plot_modeltime_forecast()
  dplyr::select(.index, .value, .model_desc, .key)


preds_only <- preds  |> 
  dplyr::filter(.key != "actual")  |> 
  dplyr::select(.index, .value)

# dette er underligt 

preds_only  |> dplyr::filter(is.na(.value))

