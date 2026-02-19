load_full_dataset <- function () {
  readr::read_csv("data/train.csv")
}

load_nitor_test_data <- function() {
  readr::read_csv("data/test_for_participants.csv")
}

save_results <- function(data, model_name, prefix = "", postfix = "") {
  timestamp <- format(Sys.time(), "%d-%m-%Y %H-%M-%S")
  dirpath <- glue::glue("./inst/{model_name}/results/")
  path <- glue::glue("{dirpath}{prefix}{model_name}results{timestamp}{postfix}.csv")
  dir.create(dirpath, showWarnings = TRUE, recursive = TRUE)
  write.csv(data, path, row.names = FALSE)
}
