
workflow_helper <- function(
  train_df
) {
  initial_recipe <- recipes::recipe(
    target ~ .,
    # data = train_df_engineered
    data = train_df
  ) |>
    recipes::update_role(id, new_role = "ID") |>
    # recipes::step_dummy(market, one_hot = TRUE) |>
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
    recipes::step_rm(market) |>
    recipes::step_naomit(recipes::all_predictors())
  xgb_spec <- parsnip::boost_tree(
    trees = tune::tune(),
    tree_depth = tune::tune(),
    min_n = tune::tune(),
    loss_reduction = 0.001,
    sample_size = 0.7,
    mtry = tune::tune(),
    learn_rate = tune::tune()
  ) |>
    parsnip::set_engine(
      "xgboost"
    ) |>
    parsnip::set_mode("regression")

  xgb_wf <- workflows::workflow() |>
    workflows::add_recipe(initial_recipe) |>
    workflows::add_model(xgb_spec)
  return(xgb_wf)
}


finalize_workflow_helper <- function(
  best_params,
  workflow,
  train_df
) {

  # vi skal have params her
  xgb_wf_final <- workflow |>
    tune::finalize_workflow(best_params)

  res <- final_xgb_fit <- xgb_wf |>
    parsnip::fit(data = train_df)
  return(res)
}

xgb_opdelt_market <- function(
  MARKED = "Market A",
  sub_model_name = "",
  training_split,
  wrks = 1
) {
  # preopsætning----
  model_name <- glue::glue("xgb_", MARKED)
  # opsætning af data ----
  training_split <- training_split |> dplyr::filter(market == MARKED)
  train_targets <- training_split$target

  inv_prediction_trans <- function(x) {
    quantile(train_targets, probs = pnorm(x), na.rm = TRUE)
  }

  data_transformation_to_use <- function(df, modifyTarget = T) {
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
      dplyr::select(
        -c(
          wind_direction_80m,
          cloud_cover_low,
          cloud_cover_mid,
          cloud_cover_high,
          dew_point_temperature_2m,
          wet_bulb_temperature_2m,
          relative_humidity_2m,
          wind_gust_speed_10m,
          convective_inhibition,
          lifted_index
        )
      )

    if (modifyTarget) {
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
  train_df <- training_split |>
    data_transformation_to_use()
  # recipie ----

  # tune ----

  folds <- rsample::sliding_period(
    data = train_df,
    index = delivery_start,
    period = "day",
    lookback = 28,
    assess_stop = 1,
    step = 3,
    skip = 0
  )

  xgb_wf <- workflow_helper(train_df)
  # xgb_wf <- workflows::workflow() |>
  #   workflows::add_recipe(initial_recipe) |>
  #   workflows::add_model(xgb_spec)

  xgb_hyper_space <- dials::grid_latin_hypercube(
    dials::trees(range = c(1000L, 3000)),
    dials::tree_depth(range = c(3L, 30L)),
    dials::min_n(range = c(15L, 100L)),
    dials::mtry(range = c(10L, 35L)),
    dials::learn_rate(range = c(-3, -1), trans = scales::log10_trans()),
    size = 5
  )

  eval_metric <- yardstick::metric_set(yardstick::rmse)

  ctrl <- tune::control_grid(
    #save_pred = T,
    verbose = T,
    allow_par = T
  )

  set.seed(1)
  # options(future.globals.maxSize = Inf)
  start.time <- Sys.time()

  future::plan(future::multisession, workers = wrks)
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
  best_params_final <- tune::select_best(xgb_tune_res, metric = "rmse")
  return(best_params_final)
}

