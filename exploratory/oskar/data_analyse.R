
training_data <- load_full_dataset()

set.seed(1)
splits <- rsample::initial_time_split(
  data = training_data,
  prop = 0.85
)
train_df <- rsample::training(splits)
test_df <- rsample::testing(splits)

# se på evt ----

u <- 30

data_to_fit_with <- train_df  |> 
  dplyr::mutate(
    y_binary = dplyr::if_else(target > u, 1 , 0)
  )

tmp <- data_to_fit_with  |> dplyr::filter(y_binary > 0)
hist(tmp$target, breaks = 100)
