get_train_df <- function (MARKED = "Market A", training_split) {
  split <- training_split |> dplyr::filter(market == MARKED)
  final_data_transformation(split)(split)
}


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

  res <- final_xgb_fit <- xgb_wf_final |>
    parsnip::fit(data = train_df)
  return(res)
}

xgb_opdelt_market <- function(
  MARKED = "Market A",
  sub_model_name = "",
  training_split,
  wrks = 1,
  step = 5
) {
  # preopsætning----
  model_name <- glue::glue("xgb_", MARKED)
  # opsætning af data ----

  train_df <- get_train_df(MARKED = MARKED, training_split = training_split)

  # recipie ----

  # tune ----

  folds <- rsample::sliding_period(
    data = train_df,
    index = delivery_start,
    period = "day",
    lookback = 28,
    assess_stop = 14,
    step = 7,
    skip = 0
  )

  xgb_wf <- workflow_helper(train_df)

  xgb_hyper_space <- dials::grid_latin_hypercube(
    dials::trees(range = c(1000L, 3000)),
    dials::tree_depth(range = c(3L, 30L)),
    dials::min_n(range = c(15L, 100L)),
    dials::mtry(range = c(10L, 35L)),
    dials::learn_rate(range = c(-3, -1), trans = scales::log10_trans()),
    size = step
  )

  eval_metric <- yardstick::metric_set(yardstick::rmse)

  ctrl <- tune::control_grid(
    #save_pred = T,
    verbose = T,
    allow_par = T
  )

  print("Starting tuning")
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

  save_object(best_params_final, model_name, prefix = glue::glue("best_params_{sub_model_name}"))

  return(best_params_final)
}

