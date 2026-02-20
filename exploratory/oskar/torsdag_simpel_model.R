
# opsætning af data ----
training_data <- load_full_dataset()

## tilføj kalman filter ----


global_stats <- data.frame(
  median <- median(training_data$target),
  mad <- stats::mad(training_data$target)
)


# training_data_initial_transform <- data_transform(training_data)

## rsample::rolling_origin ----

set.seed(1)
splits <- rsample::initial_time_split(
  data = training_data,
  prop = 0.85
)
train_df <- rsample::training(splits)
test_df <- rsample::testing(splits)

train_df_stats <- data.frame(
  median <- median(train_df$target),
  mad <- stats::mad(train_df$target)
)
train_df_test <- data.frame(
  median <- median(test_df$target),
  mad <- stats::mad(test_df$target)
)
train_df <- train_df  |>
  data_transform()
test_df <- test_df  |>
  data_transform()



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
# fold1 <- rsample::get_rsplit(folds, 1)
# fold1_train <- rsample::analysis(fold1)
# så dette er 28 dage man har i et fold
# fold1_test <- rsample::assessment(fold1)
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
) |>
  recipes::update_role(id, new_role = "ID") |>
  recipes::step_rm(wind_direction_80m) |>
  recipes::step_dummy(market, one_hot = TRUE) |>
  # recipes::step_mutate(
  #   days_since_start = base::as.numeric(
  #     base::difftime(
  #       delivery_start,
  #       min(training_data$delivery_start),
  #       units = "days"
  #     )
  #   ),
  #   recency_weight = hardhat::importance_weights(exp(days_since_start / 180))
  # ) |>
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

## visualiser vores recipes ----

# prepped_recipe <- recipes::prep(
#   initial_recipe,
#   training = train_df
# )
#
# transformed_train_data <- recipes::bake(
#   prepped_recipe,
#   new_data = NULL
# )

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

save_object(best_params, "fredagsize50", prefix = "best_params")
save_object(best_xgb_params, "fredagsize50", prefix = "best_xgb_params")

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


inv_prediction_trans <- inv_asinh_trans(train_df_stats$mad....stats..mad.train_df.target., train_df_stats$median....median.train_df.target.)



## predicitons ----
fit_on_all <- fit_final_model_on_all_data(model = final_xgb_fit, inverse_prediction_transformation = inv_prediction_trans, data_transformation_function = function(x) {data_transform(x, T)})



fit_on_all |> dplyr::filter(id %in% (test_df |> dplyr::select(id))$id) |> yardstick::rmse(target, pred)
test_df |> dplyr::inner_join(fit_on_all, by="id") |> yardstick::rmse(target.x, target.y)
extract_data_for_prediction(fit_on_all) |> save_results("fredagsize")





predictions_asinh <- predict(
  final_xgb_fit,
  new_data = test_df
)

# til intern data

# final_pred_intern <- predictions_asinh  |>
#   dplyr::mutate(
#     .pred = sinh(.pred) * train_df_stats$mad....stats..mad.train_df.target. + train_df_stats$median....median.train_df.target.
#   )
#
# test_df |> dplyr::mutate(pred = final_pred_intern$.pred) |> yardstick::rmse(target, pred)



full_data_frame <- rbind(train_df |> remove_target(), test_df |> remove_target(), load_nitor_test_data() |> data_transform(TRUE))

predictions_final <- full_data_frame |>
  dplyr::mutate(target = predict(
    final_xgb_fit,
    new_data = full_data_frame
  )$.pred) |>
  dplyr::mutate(target = sinh(target) * train_df_stats$mad....stats..mad.train_df.target. + train_df_stats$median....median.train_df.target.)


final_results <- predictions_final |> dplyr::filter(id >= 133627) |> dplyr::select(id, target) |> dplyr::arrange(id)
save_results(final_results, "fredag")

# alternativ på vores egen test data, så kan vi se hvad vi fanger
# predictions_asinh <- predict(
#   final_xgb_fit,
#   new_data =test_df
# )
# vi skal have dem til normale priser
# tilføj kalman som kovariat

final_submission <- test_data.f  |>
  dplyr::select(id)  |>
  dplyr::bind_cols(predictions_asinh)  |>
  dplyr::mutate(
    target = sinh(.pred) * t_mad + t_median
  )  |>
  dplyr::select(id, target)  # |>
  # save_results(model_name = "xgb_20-02-1348")

# ggplot2::ggplot(
#   data = final_submission,
#   mapping = ggplot2::aes(
#     x = id,
#     y = target
#   )
# ) + ggplot2::geom_line()

# tjek qqplot ----

# Vi samler data i en lille tibble til plottet
qq_data <- data.frame(
  actual = (rsample::testing(splits) |> dplyr::filter(id < 133627))$target,
  predicted = (predictions_final |> dplyr::filter(id < 133627, id >= 112727))$target
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
