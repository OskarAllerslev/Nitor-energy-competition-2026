model_name <- "arima_models"
sub_model_name <- "arima_lørdag"



# opsætning af data ----
training_data <- load_full_dataset()

set.seed(1)
splits <- rsample::initial_split(
  data = training_data,
  prop = 0.85
)
training_split <- rsample::training(splits)
testing_split <- rsample::testing(splits)


train_targets <- training_split$target

inv_prediction_trans <- function(x) { quantile(train_targets, probs= pnorm(x), na.rm=TRUE)}


data_transformation_to_use <- function (df, modifyTarget = T) {




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
      residual_load = load_forecast - solar_forecast - wind_forecast,
      residual_load_forecast_lag_24 = dplyr::lag(residual_load, n = 24),
      residual_load_ma_6h = slider::slide_dbl(
        .x = residual_load,
        .f = mean,
        .before = 6,
        .complete = FALSE
      ),
      wind_dir_sin = sin(wind_direction_80m * (pi / 180)),
      wind_dir_cos = cos(wind_direction_80m * (pi / 180)),
      temp_index = pmax(0, wet_bulb_temperature_2m - 22) +
        pmax(0, 18 - air_temperature_2m)
    ) |>
    dplyr::select(-c(wind_direction_80m,
                     cloud_cover_low,
                     cloud_cover_mid,
                     cloud_cover_high,
                     dew_point_temperature_2m,
                     wet_bulb_temperature_2m,
                     relative_humidity_2m,
                     wind_gust_speed_10m,
                     convective_inhibition,
                     lifted_index))

  if(modifyTarget) {
    the_ecdf <- ecdf(train_targets)
    bounded_ecdf <- function(x) {
      p <- the_ecdf(x)
      pmin(pmax(p, 1e-4), 1 - 1e-4)
    }

    result <- result |> dplyr::mutate(target = qnorm(bounded_ecdf(target)))
  }
  result <- result |>
    dplyr::arrange(id, delivery_start) |>
    dplyr::ungroup()
  result <- result |>
    dplyr::filter(!is.na(id))

  return(result)
}

train_df <- training_split  |>
  data_transformation_to_use(F)
test_df <- testing_split  |>
  data_transformation_to_use(F)



# folds ----
folds <- rsample::sliding_period(
  data = train_df,
  index = delivery_start,
  period = "day",
  lookback = 28,
  assess_stop = 1,
  step = 3,
  skip = 0
)

initial_recipe <- recipes::recipe(
  target ~ .,
  # data = train_df_engineered
  data = train_df
) |>
  recipes::update_role(id, new_role = "ID") |>
  recipes::step_dummy(market, one_hot = TRUE) |>
  recipes::step_date(
    delivery_start,
    features = c("dow", "doy"),
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
  recipes::step_rm(delivery_end) |>
  recipes::step_naomit(recipes::all_predictors())


# test at alt er okay

# prepped_recipe <- recipes::prep(
#   initial_recipe,
#   training = train_df
# )
#
# transformed_train_data <- recipes::bake(
#   prepped_recipe,
#   new_data = NULL
# )


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

# parameterrum ----

xgb_hyper_space <- dials::grid_latin_hypercube(
  dials::trees(range = c(100L, 2000L)),
  dials::tree_depth(range = c(3L, 9L)),
  dials::min_n(range = c(15L, 100L)),
  dials::mtry(range = c(10L, 35L)),
  dials::learn_rate(range = c(-3, -1), trans = scales::log10_trans()),
  size = 20
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
start.time <- Sys.time()

future::plan(future::multisession, workers = 15)
xgb_tune_res <- tune::tune_grid(
  object = xgb_wf,
  resamples = folds,
  grid = xgb_hyper_space,
  metrics = eval_metric,
  control = ctrl
)
future::plan(future::sequential)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



# se på de bedste hyperparametre ----


best_params <- tune::show_best(xgb_tune_res, metric = "rmse")

best_xgb_params <- tune::select_best(xgb_tune_res, metric = "rmse")

#save_object(xgb_tune_res, model_name, prefix = glue::glue("tune_res_{sub_model_name}"))
save_object(best_params, model_name, prefix = glue::glue("best_params_{sub_model_name}"))
save_object(best_xgb_params, model_name, prefix = glue::glue("best_xgb_params_{sub_model_name}"))

#df <- readRDS("./inst/mandag_morgen_multisession_xgboost/model/best_paramsmandag_morgen_multisession_xgboost21-02-2026 11-56-41.rds")

df <- best_params

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




## predicitons ----
fit_on_all <- fit_final_model_on_all_data(model = final_xgb_fit,
                                          inverse_prediction_transformation = function(x) {x},
                                          data_transformation_function = function(x) {x |> data_transformation_to_use(modifyTarget = F)})


fit_on_all <- fit_on_all |> dplyr::mutate(target = quantile(train_targets, probs = pnorm(target), na.rm=T))

extract_fit_subset(fit_on_all, testing_split) |> yardstick::rmse(target.x, target.y)
extract_fit_subset(fit_on_all, training_split) |> yardstick::rmse(target.x, target.y)
extract_data_for_prediction(fit_on_all) |> save_results(model_name, postfix = sub_model_name)

