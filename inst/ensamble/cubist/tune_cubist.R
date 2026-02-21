# preopsætning----
model_name <- "cubist"
sub_model_name <- "lørdag_eftermiddag"



# opsætning af data ----
training_data <- load_full_dataset()

set.seed(1)
splits <- rsample::initial_time_split(
  data = training_data,
  prop = 0.95
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
  data_transformation_to_use()
test_df <- testing_split  |>
  data_transformation_to_use()

# recipie ----

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


# model tuning ----
# Sørg for at 'rules' er loaded, da den indeholder cubist_rules() og dens hyperparametre
library(rules) 
library(tidymodels)

set.seed(1) 
cv_folds <- rsample::vfold_cv(
  train_df, 
  v = 10 
)

# 1. Definer Cubist modellen
# Vi tuner 'committees' og 'neighbors'
library(rules)
cubist_spec <- cubist_rules(
  committees = tune::tune(), 
  neighbors  = tune::tune()
) |> 
  parsnip::set_engine("Cubist") |> 
  parsnip::set_mode("regression")

# 2. Saml i et workflow
cubist_wf <- workflows::workflow() |> 
  workflows::add_recipe(initial_recipe) |> 
  workflows::add_model(cubist_spec)

# 3. Lav grid
# dials finder automatisk de rigtige ranges for committees (ofte 1-100) og neighbors (0-9)
cubist_grid <- dials::grid_latin_hypercube(
  rules::committees(), 
  dials::neighbors(), 
  size = 25
)

# 4. Kør tuning
# Bemærk: Cubist kan være lidt tungere at tune end glmnet, så dette kan tage et par minutter
tune_res_cubist <- tune::tune_grid(
  cubist_wf, 
  resamples = cv_folds, 
  grid      = cubist_grid 
)

# 5. Find bedste parametre og gem
best_params_cubist <- tune::select_best(tune_res_cubist, metric = "rmse")

# Genbruger din gemme-funktion
save_object(best_params_cubist, "cubist", prefix = glue::glue("{sub_model_name}"))