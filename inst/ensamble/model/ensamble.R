

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
final_glmnet_params <- readRDS(file = "inst/glmnet/model/lørdag_eftermiddagglmnet21-02-2026 16-11-42.rds")

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

## XGB ----
final_xgb_params <- readRDS(file = "./inst/xgboost_lørdag_kl16/model/best_params_større_assessment_pitxgboost_lørdag_kl1621-02-2026 16-29-47.rds")
final_xgb_params <- head(final_xgb_params, n = 1)

xgb_spec <- parsnip::boost_tree(
  trees = tune::tune(),
  tree_depth = tune::tune(),
  min_n = tune::tune(),
  loss_reduction = 0.001,
  sample_size = 0.7,
  mtry = tune::tune(),
  learn_rate = tune::tune()
)  |>
  parsnip::set_engine(
    "xgboost"#,
    #nthread = 100
    #tree_method = "gpu_hist",
    # device = "cuda"
  )  |>
  parsnip::set_mode("regression")

xgb_wf <- workflows::workflow()  |>
  workflows::add_recipe(initial_recipe)  |>
  workflows::add_model(xgb_spec)  |> 
  tune::finalize_workflow(final_xgb_params)

xgb_fit <- xgb_wf  |>
  parsnip::fit(data = train_df)



# we make the ensemble ----
models_tibble <- modeltime::modeltime_table(
  glmnet_fit,
  # glmnet_fit
  # wflw_fit_prophet,
  xgb_fit
)

## make the ensemble ----
acc_tbl <- modeltime::modeltime_accuracy(
  calibration_tbl
)

# vi laver en model til at væglge vægtene 
# fast_resample <- modeltime.resample::time_series_cv(
#   data = train_df,
#   date_var = delivery_start,
#   initial = "2 years", 
#   assess = "3 months", 
#   slice_limit = 1
# )
# fast_resamples_fitted <- models_tibble  |> 
#   modeltime.resample::modeltime_fit_resamples(
#     resamples = fast_resample, 
#     control = tune::control_resamples()
#   )

# meta_model <- parsnip::linear_reg(penalty = 0.01, mixture = 1)  |> 
#   parsnip::set_engine("glmnet")

# ensemble_fit <- fast_resamples_fitted |> 
#   modeltime.ensemble::ensemble_model_spec(
#     model_spec = meta_model, 
#     control = tune::control_grid()
#   )

# tmp <- ensemble_fit  |> 
#   modeltime.ensemble::ensemble_average()



ensemble_fit <- models_tibble  |>
  modeltime.ensemble::ensemble_weighted()
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
all_data_imp <- all_data  |> 
  dplyr::mutate(
    dplyr::across(
      dplyr::where(is.numeric), ~ tidyr::replace_na(.x, median(.x, na.rm = T))
    )
  )


preds <- calibration_tbl  |>
  modeltime::modeltime_forecast(
    new_data = all_data_imp
  )

all_data_transformed <- all_data |>
  dplyr::mutate(target = preds$.value) |>
  dplyr::mutate(target = quantile(train_targets, probs = pnorm(target), na.rm=T))  |> 
  extract_data_for_prediction()  |> 
  save_results(model_name = "ensamble", prefix = "45-8" )

all_data_transformed |>
  extract_fit_subset(testing_split) |>
  yardstick::rmse(target.x, target.y)


# ville lige plotte vores predictions
plot_data <- all_data_transformed  |> 
  extract_fit_subset(testing_split)

ggplot2::ggplot(data = plot_data, mapping = ggplot2::aes(x = id)) +
  ggplot2::geom_line(ggplot2::aes(y = target.x, color = "blue", alpha = 0.2)) +
  ggplot2::geom_line(ggplot2::aes(y = target.y, color = "black")) 

