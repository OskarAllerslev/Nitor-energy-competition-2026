
# training_data <- load_full_dataset()

# # split data ----
# set.seed(1)
# splits <- rsample::initial_time_split(
#   data = training_data,
#   prop = 0.85
# )
# train_df <- rsample::training(splits)
# test_df <- rsample::testing(splits)

# # definer tærskel ----
# u <- 1000 

# # forbered data ----
# # Vi bruger "spike" og "normal" som faktorniveauer for at sikre stabil forudsigelse
# data_prepared <- train_df |> 
#   dplyr::mutate(
#     y_binary = as.factor(dplyr::if_else(target > u, "spike", "normal")) 
#   )

# # opsæt krydsvalidering ----
# set.seed(1)
# cv_folds <- rsample::vfold_cv(data_prepared, v = 5)

# # opskrift frekvens ----
# freq_recipe <- recipes::recipe(y_binary ~ ., data = data_prepared) |> 
#   recipes::step_rm(target) |> 
#   recipes::step_mutate(
#     delivery_start_num = as.numeric(delivery_start),
#     delivery_end_num = as.numeric(delivery_end)
#   ) |> 
#   recipes::step_rm(delivery_start, delivery_end) |> 
#   recipes::step_string2factor(recipes::all_nominal_predictors()) |> 
#   recipes::step_dummy(recipes::all_nominal_predictors())

# # modelspecifikation frekvens ----
# freq_spec <- parsnip::boost_tree(
#   trees = 100, 
#   tree_depth = 4, 
#   learn_rate = 0.1
# ) |> 
#   parsnip::set_engine("xgboost") |> 
#   parsnip::set_mode("classification")

# # workflow frekvens ----
# freq_wf <- workflows::workflow() |> 
#   workflows::add_recipe(freq_recipe) |> 
#   workflows::add_model(freq_spec)

# # fit frekvens oof ----
# freq_res <- tune::fit_resamples(
#   object = freq_wf,
#   resamples = cv_folds,
#   control = tune::control_resamples(save_pred = TRUE)
# )

# # udtræk frekvens oof ----
# oof_freq_preds <- tune::collect_predictions(freq_res) |> 
#   dplyr::arrange(.row) |> 
#   dplyr::pull(.pred_spike)

# # klargør skadesstørrelse data ----
# data_tail <- data_prepared |> 
#   dplyr::filter(target > u)

# # opskrift skadesstørrelse ----
# tail_recipe <- recipes::recipe(target ~ ., data = data_tail) |> 
#   recipes::step_rm(y_binary) |> 
#   recipes::step_mutate(
#     delivery_start_num = as.numeric(delivery_start),
#     delivery_end_num = as.numeric(delivery_end)
#   ) |> 
#   recipes::step_rm(delivery_start, delivery_end) |> 
#   recipes::step_string2factor(recipes::all_nominal_predictors()) |> 
#   recipes::step_dummy(recipes::all_nominal_predictors())

# # modelspecifikation skadesstørrelse ----
# tail_spec <- parsnip::boost_tree(
#   trees = 50,
#   tree_depth = 3
# ) |> 
#   parsnip::set_engine("xgboost") |> 
#   parsnip::set_mode("regression")

# # workflow skadesstørrelse ----
# tail_wf <- workflows::workflow() |> 
#   workflows::add_recipe(tail_recipe) |> 
#   workflows::add_model(tail_spec)

# # fit skadesstørrelse ----
# tail_fit <- parsnip::fit(
#   object = tail_wf, 
#   data = data_tail
# )

# # forudsig skadesstørrelse ----
# severity_preds <- stats::predict(
#   object = tail_fit, 
#   new_data = data_prepared
# ) |> 
#   dplyr::pull(.pred)

# # saml meta features ----
# final_train_df <- data_prepared |> 
#   dplyr::mutate(
#     feature_prob_spike = oof_freq_preds,
#     feature_expected_severity = severity_preds
#   ) |> 
#   dplyr::select(-y_binary)


# # ggplot2::ggplot(
# #   data = final_train_df, 
# #   mapping = ggplot2::aes(
# #     x = id
# #   )
# # ) + 
# #   ggplot2::geom_line(ggplot2::aes(y = target)) + 
# #   ggplot2::geom_line(ggplot2::aes(y = feature_prob_spike, colour = "red"))
# #   ggplot2::geom_line(ggplot2::aes(y = feature_expected_severity, colour = "red"))




# freq_fit_full <- parsnip::fit(
#   object = freq_wf, 
#   data = data_prepared
# )

# freq_bundle <- bundle::bundle(freq_fit_full)
# tail_bundle <- bundle::bundle(tail_fit)

# readr::write_rds(freq_bundle, file = "exploratory/oskar/freq_model.rds")
# readr::write_rds(tail_bundle, file = "exploratory/oskar/tail_model.rds")



# anvend funktionen på test data der ikke har en target ----
# test_df_enriched <- add_tail_covariates(new_data = test_df)
