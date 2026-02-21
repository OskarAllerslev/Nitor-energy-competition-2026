load_full_dataset <- function () {
  readr::read_csv("data/train.csv")
}

load_nitor_test_data <- function() {
  readr::read_csv("data/test_for_participants.csv")
}

validate_results <- function(results) {
  if (!identical(colnames(results), c("id", "target"))) {
    stop("Wrong column names!")
  } else if (nrow(results) != 13098) {
    stop(sprintf("Wrong row count: %d", nrow(results)))
  } else if (min(results$id) != 133627) {
    stop("IDs must start at 133627")
  } else if (max(results$id) != 146778) {
    stop("IDs must end at 146778")
  } else if (any(is.na(results$target))) {
    stop("No NA values allowed!")
  } else if (!all(is.finite(results$target))) {
    stop("No infinite values allowed!")
  }
}

save_results <- function(data, model_name, prefix = "", postfix = "") {
  validate_results(data)
  timestamp <- format(Sys.time(), "%d-%m-%Y %H-%M-%S")
  dirpath <- glue::glue("./inst/{model_name}/results/")
  path <- glue::glue("{dirpath}{prefix}{model_name}results{timestamp}{postfix}.csv")
  dir.create(dirpath, showWarnings = TRUE, recursive = TRUE)
  write.csv(data, path, row.names = FALSE)
}

save_object <- function(objectToSave, model_name, prefix = "", postfix = "") {
  timestamp <- format(Sys.time(), "%d-%m-%Y %H-%M-%S")
  dirpath <- glue::glue("./inst/{model_name}/model/")
  path <- glue::glue("{dirpath}{prefix}{model_name}{timestamp}{postfix}.rds")
  dir.create(dirpath, showWarnings = TRUE, recursive = TRUE)
  saveRDS(objectToSave, file = path)
}

remove_target <- function(df) {
  df |> dplyr::select(-target)
}

fit_final_model_on_all_data <- function(model, inverse_prediction_transformation, data_transformation_function) {
  inverse_vectorized <- Vectorize(inverse_prediction_transformation)
  training_data <- load_full_dataset() |> data_transformation_function() |> remove_target()
  prediction_data <- load_nitor_test_data() |> data_transformation_function()
  full_df <- rbind(training_data, prediction_data)
  full_df |>
    dplyr::mutate(target = predict(model, new_data=full_df)$.pred) |>
    dplyr::mutate(target = inverse_vectorized(target))
}


extract_data_for_prediction <- function(fit_on_all_data) {
  fit_on_all_data |> dplyr::filter(id >= 133627) |> dplyr::select(id, target) |> dplyr::arrange(id)
}

extract_fit_subset <- function(fit_on_all_data, subset) {
  subset |> dplyr::inner_join(fit_on_all_data, by="id")
}




