# source("./utils/dataloadandsave.R")
training_data <- load_full_dataset()
View(training_data)



mean_guess <- mean(training_data$target)









test_data <- load_nitor_test_data()
View(test_data)

guess <- data.frame(id = test_data$id, target = mean_guess)
View(guess)

write.csv(guess, "./inst/eksempel_model/resultater/gennemsnit.csv", row.names = FALSE)



save_results(guess, "gennemsnit", "mitprefix", "mitpostfix")

View(training_data)


