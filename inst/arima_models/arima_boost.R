model_name <- "fredag_aften_arima"
data_transformation_to_use <- data_transform_v1



# opsætning af data ----
training_data <- load_full_dataset()

## tilføj kalman filter ----


training_data <- training_data |> dplyr::filter(delivery_start >= "2023-10-01")


## rsample::rolling_origin ----

set.seed(1)
splits <- rsample::initial_time_split(
  data = training_data,
  prop = 0.85
)
training_split <- rsample::training(splits)
testing_split <- rsample::testing(splits)

train_df_stats <- data.frame(
  median = median(training_split$target),
  mad = stats::mad(training_split$target)
)
train_df_test <- data.frame(
  median = median(testing_split$target),
  mad = stats::mad(testing_split$target)
)
train_df <- training_split  |>
  data_transformation_to_use()
test_df <- testing_split  |>
  data_transformation_to_use()



# folds ----
# TODO: Lav dem kumulative
folds <- rsample::sliding_period(
  data = train_df,
  index = delivery_start,
  period = "day",
  lookback = 28,
  assess_stop = 1,
  step = 30, #sæt til 1
  skip = 27
)

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
  # recipes::step_rm(delivery_start) |>
  recipes::step_naomit(recipes::all_predictors()) |>
  recipes::step_rm(delivery_end)


# opsætning af model xgb ----


xgb_spec <- modeltime::arima_boost(
  # ARIMA parametre (lader vi stå tomme for at tvinge auto.arima)

  # XGBoost hyperparametre
  trees = tune::tune(),
  tree_depth = tune::tune(),
  min_n = tune::tune(),
  mtry = tune::tune(),
  learn_rate = tune::tune(),
  sample_size = 0.7,
  loss_reduction = 0.001
) |>
  parsnip::set_engine(
    engine = "auto_arima_xgboost"
    # nthread kan også sættes her via list(nthread = cores) i fremtiden
  ) |>
  parsnip::set_mode("regression")


# workflow ----
xgb_wf <- workflows::workflow()  |>
  workflows::add_recipe(initial_recipe)  |>
  workflows::add_model(xgb_spec)

  # fjernet recency weights

  # workflows::add_case_weights(recency_weight)  |>
# parameterrum ----

xgb_hyper_space <- dials::grid_latin_hypercube(
  dials::trees(range = c(100L, 2000L)),
  dials::tree_depth(range = c(3L, 9L)),
  dials::min_n(range = c(15L, 100L)),
  dials::mtry(range = c(10L, 35L)),
  dials::learn_rate(range = c(-3, -1), trans = scales::log10_trans()),
  size =1
)

# fit model ----


# eval_metric <- yardstick::metric_set(yardstick::rmse)
eval_metric <- yardstick::metric_set(yardstick::rmse)


ctrl <- tune::control_grid(
  #save_pred = T,
  verbose = T,
  allow_par = T
)
set.seed(1)
# options(future.globals.maxSize = Inf)
# future::plan(future::sequential)
cores <- parallel::detectCores() - 1
# cl <- parallel::makePSOCKcluster(cores)
# doParallel::registerDoParallel(cl)

future::plan(future::multisession, workers = cores)
xgb_tune_res <- tune::tune_grid(
  object = xgb_wf,
  resamples = folds,
  grid = xgb_hyper_space,
  metrics = eval_metric,
  control = ctrl
)


# parallel::stopCluster(cl)


# se på de bedste hyperparametre ----


best_params <- tune::show_best(xgb_tune_res, metric = "rmse")

best_xgb_params <- tune::select_best(xgb_tune_res, metric = "rmse")

save_object(best_params, model_name, prefix = "best_params")
save_object(best_xgb_params, model_name, prefix = "best_xgb_params")

#df <- readRDS("./inst/torsdag_aften/model/best_params.rds")

df <- best_params

##  antag bedste params ----
best_params_final <- head(df, n = 1)

final_xgb_wf <- xgb_wf  |>
  tune::finalize_workflow(best_params_final)


## fit model på træningssættet ----
final_xgb_fit <- final_xgb_wf  |>
  parsnip::fit(data = train_df)


### importance ----
final_xgb_fit  |>
  workflows::extract_fit_parsnip()  |>
  vip::vi(method = "model")  |>
  print(n = 50)




inv_prediction_trans <- function(x) {x}


my_trans <- function(x) {x |>
    add_tail_covariates() |>
    data_transformation_to_use()  }

## predicitons ----
fit_on_all <- fit_final_model_on_all_data(model = final_xgb_fit,
                                          inverse_prediction_transformation = inv_prediction_trans,
                                          data_transformation_function = my_trans)


extract_fit_subset(fit_on_all, testing_split) |> yardstick::rmse(target.x, target.y)
extract_data_for_prediction(fit_on_all) |> save_results(model_name)



# tjek qqplot ----

# Vi samler data i en lille tibble til plottet
qq_data <- data.frame(
  actual = testing_split$target,
  predicted = extract_fit_subset(fit_on_all, testing_split)$target.x
)

library(ggplot2)
ggplot(qq_data, aes(sample = predicted)) +
  stat_qq(distribution = qt, dparams = list(df = 5)) + # Valgfrit: sammenlign med t-fordeling
  stat_qq_line() +
  labs(title = "QQ-plot: Model Predictions",
       subtitle = "Sammenligning af prædikteret fordeling mod teoretisk fordeling") +
  theme_minimal()

actual_data <- data.frame(
  id = (predictions_final |> dplyr::filter(id < 133627, id >= 112727))$id,
  actual = qq_data$actual,
  predicted = qq_data$predicted
)
ggplot2::ggplot(actual_data, ggplot2::aes(x = id)) +
  ggplot2::geom_line( ggplot2::aes(y = actual, color = "black", alpha = 0.7, linetype = "dashed")) +
  ggplot2::geom_line( ggplot2::aes(y = predicted, color = "red" ))


qq_data  |>
  yardstick::rmse(actual, predicted)
