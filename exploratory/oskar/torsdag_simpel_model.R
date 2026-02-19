
# opsætning af data ----
training_data <- load_full_dataset()

## tilføj kalman filter ----


t_median <- median(training_data$target)
t_mad <- stats::mad(training_data$target)

library(KFAS)
training_data_initial_transform <- training_data  |> 
  dplyr::mutate(
    target = asinh((target - t_median) / t_mad)#,
    # kalman = {
    #   y_vec <- as.numeric(target)
    #   mod <- KFAS::SSModel(y_vec ~ SSMtrend(1, Q = list(matrix(NA))), H = matrix(NA))
    #   fit <- KFAS::fitSSM(mod, inits = c(0, 0), method = "BFGS")$model
    #   kfs_out <- KFAS::KFS(fit)
    #   as.numeric(kfs_out$a[1:length(y_vec)])
    # },
    # garch_state = {
    #   y_vec <- as.numeric(target) 
    #   spec <- rugarch::ugarchspec(
    #     variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
    #     mean.model = list(armaOrder = c(0,0))
    #   )
    #   garch_fit <- rugarch::ugarchfit(spec = spec, data = y_vec)
    #   as.numeric(rugarch::sigma(garch_fit))
    # }
  )



## rsample::rolling_origin ----

set.seed(1)
splits <- rsample::initial_time_split(
  data = training_data_initial_transform,
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
  step = 30, #sæt til 1
  skip = 27
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


# library(KFAS)
# train_df_engineered <- train_df |>
#   dplyr::mutate(
#     target = asinh((target - median(target, na.rm = TRUE)) / stats::mad(target, na.rm = TRUE)),
#   ) 


# step harmonic sin(2pi * frek * x / cycle_size) samme med cos
# TODO: tilføj residual load

initial_recipe <- recipes::recipe(
  target ~ .,
  # data = train_df_engineered
  data = train_df 
)  |>
recipes::update_role(id, new_role = "ID") |>
recipes::step_mutate(
    wind_speed_80m = wind_speed_80m^3,
    wind_dir_sin = sin(wind_direction_80m * (pi /180)),
    wind_dir_cos = cos(wind_direction_80m * (pi /180)),
    residual_load = load_forecast - solar_forecast - wind_forecast,
    temp_index = pmax(0, wet_bulb_temperature_2m - 22) + pmax(0, 18 - air_temperature_2m),
    convective_threat = convective_available_potential_energy * (1/ (convective_inhibition + 0.01)) * cloud_cover_high, 
    icing_risk = dplyr::if_else(freezing_level_height < 150 & relative_humidity_2m > 90, 1,0)
)  |>
  # rolling stats
recipes::step_window(
  residual_load, 
  size = 7, 
  role = "predictor", 
  statistic = "mean", 
  names = "residual_load_ma_6h"
)  |> 
  recipes::step_lag(load_forecast, lag = 24)  |> 
  recipes::step_mutate(
    load_momentum = load_forecast - dplyr::lag(load_forecast, 3)
  )  |> 
  recipes::step_rm(wind_direction_80m )  |>
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
)  |> 
  recipes::step_rm(delivery_start) |>
  recipes::step_naomit(recipes::all_predictors())  |> 
  recipes::step_rm(delivery_end)  

## visualiser vores recipes ----

prepped_recipe <- recipes::prep(
  initial_recipe,
  training = train_df 
)

transformed_train_data <- recipes::bake(
  prepped_recipe,
  new_data = NULL
)

# temp plots

# ggplot2::ggplot(
#   data = transformed_train_data  |>  dplyr::filter(id < 2000), 
#   mapping = ggplot2::aes(
#     x = id ,
#   )
# ) + ggplot2::geom_line(mapping = ggplot2::aes(y = garch_state), color = "red", size = 2) + 
#    ggplot2::geom_line(mapping = ggplot2::aes(y  = target), color = "black", alpha = 0.5)

# ggplot2::ggplot(
#   data = transformed_train_data  |>  dplyr::filter(id < 500), 
#   mapping = ggplot2::aes(
#     x = id ,
#   )
# ) + ggplot2::geom_line(mapping = ggplot2::aes(y = kalman), color = "red", size = 2) + 
#    ggplot2::geom_line(mapping = ggplot2::aes(y  = target), color = "black", alpha = 0.5)

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
    "xgboost"#,
   #nthread =15
    #tree_method = "gpu_hist",
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
  size = 5
)


# fit model ----


eval_metric <- yardstick::metric_set(yardstick::rmse)


ctrl <- tune::control_grid(
  save_pred = T,
  verbose = T,
  allow_par = T
)

set.seed(1)
# options(future.globals.maxSize = Inf)
# future::plan(future::sequential)
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


parallel::stopCluster(cl)


# se på de bedste hyperparametre ----


best_params <- tune::show_best(xgb_tune_res, metric = "rmse")

best_xgb_params <- tune::select_best(xgb_tune_res, metric = "rmse")

saveRDS(best_params, file = "./inst/torsdag_aften/model/best_params.rds")
saveRDS(best_xgb_params, file = "./inst/torsdag_aften/model/best_xgb_params.rds")

df <- readRDS("./inst/torsdag_aften/model/best_params.rds")

# vi skal fitte model med disse hyperparams 
best_params_final <- head(df, n = 1)

final_xgb_wf <- xgb_wf  |> 
  tune::finalize_workflow(best_params_final)


# test og trainingdaten
# test_data_f <- load_nitor_test_data()  |> 
#   dplyr::mutate(

#   )
training_data_f <- load_full_dataset()
training_data_f_transformed <- training_data_f    |> 
  dplyr::mutate(
    target = asinh((target - t_median) / t_mad)#,
    # kalman = {
    #   y_vec <- as.numeric(target)
    #   mod <- KFAS::SSModel(y_vec ~ SSMtrend(1, Q = list(matrix(NA))), H = matrix(NA))
    #   fit <- KFAS::fitSSM(mod, inits = c(0, 0), method = "BFGS")$model
    #   kfs_out <- KFAS::KFS(fit)
    #   as.numeric(kfs_out$a[1:length(y_vec)])
    # },
    # garch_state = {
    #   y_vec <- as.numeric(target) 
    #   spec <- rugarch::ugarchspec(
    #     variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
    #     mean.model = list(armaOrder = c(0,0))
    #   )
    #   garch_fit <- rugarch::ugarchfit(spec = spec, data = y_vec)
    #   as.numeric(rugarch::sigma(garch_fit))
    # }
  )
# fit model på træningssættet
final_xgb_fit <- final_xgb_wf  |> 
  parsnip::fit(data = training_data_f_transformed)

# alternativ på vores egen test data, så kan vi se hvad vi fanger
# final_xgb_fit <- final_xgb_wf  |> 
#   parsnip::fit(data = train_df)


#importance 
final_xgb_fit  |> 
  workflows::extract_fit_parsnip()  |> 
  vip::vi(method = "model")  |> 
  print(n = 50)


t_median <- median(training_data_f$target)
t_mad <- stats::mad(training_data_f$target)


predictions_asinh <- predict(
  final_xgb_fit, 
  new_data = test_data_f
)

# alternativ på vores egen test data, så kan vi se hvad vi fanger
# predictions_asinh <- predict(
#   final_xgb_fit, 
#   new_data =test_df 
# )
# vi skal have dem til normale priser
# tilføj kalman som kovariat
final_submission <- test_data_f  |> 
  dplyr::select(id)  |> 
  dplyr::bind_cols(predictions_asinh)  |> 
  dplyr::mutate(
    target = sinh(.pred) * t_mad + t_median
  )  |> 
  dplyr::select(id, target) 
  # save_results(model_name = "xgb_19-02-2013")

# ggplot2::ggplot(
#   data = final_submission, 
#   mapping = ggplot2::aes(
#     x = id, 
#     y = target
#   )
# ) + ggplot2::geom_line()

# tjek qqplot ----
library(ggplot2)

# Vi samler data i en lille tibble til plottet
qq_data <- data.frame(
  actual = test_df$target,
  predicted = final_submission$target
)

ggplot(qq_data, aes(sample = predicted)) +
  stat_qq(distribution = qt, dparams = list(df = 5)) + # Valgfrit: sammenlign med t-fordeling
  stat_qq_line() +
  labs(title = "QQ-plot: Model Predictions",
       subtitle = "Sammenligning af prædikteret fordeling mod teoretisk fordeling") +
  theme_minimal()


