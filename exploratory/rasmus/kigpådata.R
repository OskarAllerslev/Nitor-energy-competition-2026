
training_data <- readr::read_csv("data/train.csv")
View(training_data)



mean_guess <- mean(training_data$target)







test_data <- readr::read_csv("data/test_for_participants.csv")
View(test_data)

guess <- data.frame(id = test_data$id, target = mean_guess)
View(guess)

write.csv(guess, "./inst/eksempel_model/resultater/gennemsnit.csv", row.names = FALSE)





View(training_data)


