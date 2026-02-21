model_name <- "mandag_morgen_multisession_xgboost"
data_transformation_to_use <- data_transform_v1



# opsætning af data ----
training_data <- load_full_dataset()

## tilføj kalman filter ----


training_data <- training_data


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
  median <- median(testing_split$target),
  mad <- stats::mad(testing_split$target)
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
  recipes::step_rm(delivery_start) |>
  recipes::step_naomit(recipes::all_predictors()) |>
  recipes::step_rm(delivery_end)


# opsætning af model xgb ----


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


# workflow ----
xgb_wf <- workflows::workflow()  |>
  workflows::add_case_weights(recency_weight)  |>
  workflows::add_recipe(initial_recipe)  |>
  workflows::add_model(xgb_spec)

# parameterrum ----

xgb_hyper_space <- dials::grid_latin_hypercube(
  dials::trees(range = c(100L, 2000L)),
  dials::tree_depth(range = c(3L, 9L)),
  dials::min_n(range = c(15L, 100L)),
  dials::mtry(range = c(10L, 35L)),
  dials::learn_rate(range = c(-3, -1), trans = scales::log10_trans()),
  size = 50
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
future::plan(future::multisession, workers = 15)
xgb_tune_res <- tune::tune_grid(
  object = xgb_wf,
  resamples = folds,
  grid = xgb_hyper_space,
  metrics = eval_metric,
  control = ctrl
)


future::plan(future::sequential)


# se på de bedste hyperparametre ----


best_params <- tune::show_best(xgb_tune_res, metric = "rmse")

best_xgb_params <- tune::select_best(xgb_tune_res, metric = "rmse")

save_object(best_params, model_name, prefix = "best_params")
save_object(best_xgb_params, model_name, prefix = "best_xgb_params")

df <- readRDS("./inst/mandag_morgen_multisession_xgboost/model/best_paramsmandag_morgen_multisession_xgboost21-02-2026 11-56-41.rds")

# df <- best_params

##  antag bedste params ----
best_params_final <- head(df, n = 1)

final_xgb_wf <- xgb_wf  |>
  tune::finalize_workflow(best_params_final)


## fit model på træningssættet ----
set.seed(1)
final_xgb_fit <- final_xgb_wf  |>
  parsnip::fit(data = train_df)


### importance ----
final_xgb_fit  |>
  workflows::extract_fit_parsnip()  |>
  vip::vi(method = "model")  |>
  print(n = 50)






inv_prediction_trans <- inv_asinh_trans(train_df_stats$mad, train_df_stats$median)


## predicitons ----
my_trans <- function(x) {x |>
    data_transformation_to_use(T)  }


fit_on_all <- fit_final_model_on_all_data(model = final_xgb_fit,
                                          inverse_prediction_transformation = inv_prediction_trans,
                                          data_transformation_function = my_trans)

extract_fit_subset(fit_on_all, testing_split) |> yardstick::rmse(target.x, target.y)
extract_data_for_prediction(fit_on_all) |> save_results(model_name)


## fit on full data set

fulldata_df <- load_full_dataset() |> data_transformation_to_use()

full_stats <- data.frame(
  median = median(fulldata_df$target),
  mad = stats::mad(fulldata_df$target)
)


final_xgb_fit <- final_xgb_wf  |>
  parsnip::fit(data = fulldata_df)


### importance ----
final_xgb_fit  |>
  workflows::extract_fit_parsnip()  |>
  vip::vi(method = "model")  |>
  print(n = 50)



inv_prediction_trans <- inv_asinh_trans(full_stats$mad, full_stats$median)


## predicitons ----
my_trans <- function(x) {x |>
    data_transformation_to_use(T)  }


fit_on_all <- fit_final_model_on_all_data(model = final_xgb_fit,
                                          inverse_prediction_transformation = inv_prediction_trans,
                                          data_transformation_function = my_trans)

extract_fit_subset(fit_on_all, testing_split) |> yardstick::rmse(target.x, target.y)
extract_data_for_prediction(fit_on_all) |> save_results(model_name)




## fit on 2023 october and forward data set

fulldata_df <- rbind(training_split, testing_split) |> data_transformation_to_use()

full_stats <- data.frame(
  median = median(fulldata_df$target),
  mad = stats::mad(fulldata_df$target)
)


final_xgb_fit <- final_xgb_wf  |>
  parsnip::fit(data = fulldata_df)


### importance ----
final_xgb_fit  |>
  workflows::extract_fit_parsnip()  |>
  vip::vi(method = "model")  |>
  print(n = 50)



inv_prediction_trans <- inv_asinh_trans(full_stats$mad, full_stats$median)


## predicitons ----
my_trans <- function(x) {x |>
    data_transformation_to_use(T)  }


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
