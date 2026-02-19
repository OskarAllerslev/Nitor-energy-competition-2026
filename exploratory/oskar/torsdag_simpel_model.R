
# opsætning af data ----
training_data <- load_full_dataset()

# rsample::rolling_origin

set.seed(1)
splits <- rsample::initial_time_split(
  data = training_data,
  prop = 0.85
)


train_df <- rsample::training(splits)
#dplyr::glimpse(train_df)
test_df <- rsample::testing(splits)


# folds ----
# TODO: Lav dem kumulative
folds <- rsample::sliding_period(
  data = train_df,
  index = delivery_start,
  period = "day",
  lookback = 28,
  assess_stop = 1,
  step = 1
)

# tjekker lige hvad der er i folds
#fold1 <- rsample::get_rsplit(folds, 1)
#fold1_train <- rsample::analysis(fold1)
# så dette er 28 dage man har i et fold
#fold1_test <- rsample::assessment(fold1)
# dette er så faktisk den følgende dag
# dette giver så vist nok ret god mening?

# feature engineering ----
# dokumenter godt hvad der sker

#dplyr::glimpse(train_df)
# lav nok en residual load på load_forcast, load - (wind + solar)?
# wind direction skal lave til cos / sin
# wind speed ?


train_df_engineered <-
  train_df |>
  dplyr::mutate(
    #market = factor(market),
    wind_speed_80m = wind_speed_80m^3#,
    #delivery_start = lubridate::ymd_hms(delivery_start)#,
    # hour_of_day_start = lubridate::hour(delivery_start ),
    # hour_sin_start = sin(2 * pi * hour_of_day_start  / 24),
    # hour_cos_start = cos(2 * pi * hour_of_day_start  / 24),
    # delivery_end = lubridate::ymd_hms(delivery_end),
    # hour_of_day_end = lubridate::hour(delivery_end),
    # hour_sin_end = sin(2 * pi * hour_of_day_end / 24),
    # hour_cos_end = cos(2 * pi * hour_of_day_end / 24)
    )


# step harmonic sin(2pi * frek * x / cycle_size) samme med cos
# TODO: tilføj residual load

initial_recipe <- recipes::recipe(
  target ~ .,
  data = train_df_engineered
)  |>
recipes::update_role(id, new_role = "ID") |>
recipes::step_dummy(market, one_hot = TRUE) |>
recipes::step_date(
  delivery_start,
  features = c("dow", "month", "doy", "year"),
  label = FALSE,
  keep_original_cols = TRUE
)  |>
recipes::step_time(
  delivery_start,
  features = c("hour"),
  keep_original_cols = TRUE
)  |>
recipes::step_harmonic( #harmonic på ugedag
  delivery_start_dow,
  frequency = 1,
  cycle_size = 7
)  |>
recipes::step_harmonic( #harmonic på hour
  delivery_start_hour,
  frequency = 1,
  cycle_size =24
)  |>
recipes::step_harmonic( #harmonic på year
  delivery_start_doy,
  frequency = 1,
  cycle_size = 365.25
)  |> # normalize
  recipes::step_mutate(
    target = asinh((target - median(target, na.rm = T)) / stats::mad(target, na.rm = T) )
  ) |>
  recipes::step_rm(delivery_start) |>
  recipes::step_rm(delivery_end)

## visualiser vores recipes ----

prepped_recipe <- recipes::prep(
  initial_recipe,
  training = train_df_engineered
)

transformed_train_data <- recipes::bake(
  prepped_recipe,
  new_data = NULL
)

# temp plots

# ggplot2::ggplot(
#   data = transformed_train_data ,
#   mapping = ggplot2::aes(
#     x = id,
#     # y = target_alt
#     y = target
#   )
# ) + ggplot2::geom_line()

# ggplot2::ggplot(
#   data = transformed_train_data  |>  dplyr::filter(id < 120 ),
#   mapping = ggplot2::aes(
#     x = id,
#   )
# ) + ggplot2::geom_line(mapping = ggplot2::aes(y  = delivery_start_hour_cos_1)) +
# ggplot2::geom_line(mapping = ggplot2::aes(y  = delivery_start_hour_sin_1))


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
    "xgboost"
    # tree_method = "hist",
    # device = "cuda"
 )  |>
  parsnip::set_mode("regression")


# workflow ----
xgb_wf <- workflows::workflow()  |>
  workflows::add_recipe(initial_recipe)  |>
  workflows::add_model(xgb_spec)

# parameterrum ----

xgb_hyper_space <- dials::grid_latin_hypercube(
  dials::trees(range = c(500L, 2000L)),
  dials::tree_depth(range = c(3L, 9L)),
  dials::min_n(range = c(15L, 100L)),
  dials::mtry(range = c(10L, 35L)),
  dials::learn_rate(range = c(-3, -1), trans = scales::log10_trans()),
  size = 30
)


# fit model ----


eval_metric <- yardstick::metric_set(yardstick::rmse)


ctrl <- tune::control_grid(
  save_pred = T,
  verbose = T,
  allow_par = T
)

set.seed(1)
cores <- parallel::detectCores() - 1
cl <- parallel::makePSOCKcluster(cores)
doParallel::registerDoParallel(cl)
xgb_tune_res <- tune::tune_grid(
  object = xgb_wf,
  resamples = folds,
  grid = xgb_hyper_space,
  metrics = eval_metric,
  control = ctrl
)





# se på de bedste hyperparametre ----


best_params <- tune::show_best(xgb_tune_res, metric = "rmse")

best_xgb_params <- tune::select_best(xgb_tune_res, metric = "rmse")


