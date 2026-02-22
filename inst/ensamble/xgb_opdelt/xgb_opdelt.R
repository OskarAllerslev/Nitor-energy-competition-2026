
training_data <- load_full_dataset()

set.seed(1)
splits <- rsample::initial_split(
  data = training_data,
  prop = 0.95
)
training_split <- rsample::training(splits)
testing_split <- rsample::testing(splits)


# fit modeller ----
wrks = 1

xgb_best_params_A <- xgb_opdelt_market(
  MARKED = "Market A", 
  sub_model_name = "xbg_opdelt", 
  training_split = trainig_split, 
  wrks =wrks 
)
xgb_best_params_B <- xgb_opdelt_market(
  MARKED = "Market B", 
  sub_model_name = "xbg_opdelt", 
  training_split = trainig_split, 
  wrks =wrks 
)
xgb_best_params_C <- xgb_opdelt_market(
  MARKED = "Market C",
  sub_model_name = "xbg_opdelt", 
  training_split = trainig_split, 
  wrks =wrks 
)
xgb_best_params_D <- xgb_opdelt_market(
  MARKED = "Market D", 
  sub_model_name = "xbg_opdelt", 
  training_split = trainig_split, 
  wrks =wrks 
)
xgb_best_params_E <- xgb_opdelt_market(
  MARKED = "Market E", 
  sub_model_name = "xbg_opdelt", 
  training_split = trainig_split, 
  wrks =wrks 
)
xgb_best_params_F <- xgb_opdelt_market(
  MARKED = "Market F", 
  sub_model_name = "xbg_opdelt", 
  training_split = trainig_split, 
  wrks =wrks 
)


# så skal de fittes ----
