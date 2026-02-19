
# opsætning af data ----
training_data <- load_full_dataset()

# rsample::rolling_origin 

set.seed(1)
splits <- rsample::initial_time_split(
  data = training_data,
  prop = 0.85
)


train_df <- rsample::training(splits)
dplyr::glimpse(train_df)
test_df <- rsample::testing(splits)


# folds ----
folds <- rsample::sliding_period(
  data = train_df, 
  index = delivery_start, 
  period = "day", 
  lookback = 28, 
  assess_stop = 1, 
  step = 1 
)

# tjekker lige hvad der er i folds
fold1 <- rsample::get_rsplit(folds, 1)
fold1_train <- rsample::analysis(fold1)
# så dette er 28 dage man har i et fold 
fold1_test <- rsample::assessment(fold1)
# dette er så faktisk den følgende dag
# dette giver så vist nok ret god mening?

# feature engineering ----
# dokumenter godt hvad der sker 










# opsætning af model xgb ----
# workflow ----
# parameterrum ----
# brug dials

# fit model ----


# se på de bedste hyperparametre ----


