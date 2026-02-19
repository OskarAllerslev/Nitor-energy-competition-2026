
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

dplyr::glimpse(train_df)
# lav nok en residual load på load_forcast, load - (wind + solar)?
# wind direction skal lave til cos / sin
# wind speed ?


train_df_engineered <-
  train_df |>
  dplyr::mutate(
    market = factor(market),
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


# opsætning af model xgb ----
# workflow ----
# parameterrum ----
# brug dials

# fit model ----


# se på de bedste hyperparametre ----


