rm(list = ls())
data("training_data")
data("testing_swiss_data")


variables <- grep("^dpin", x = names(training_data))
variance <- 0.90

# trace(GH_filter, edit = T)
row_train <- utilitR::GH_filter(
  X = training_data,
  variables = variables,
  variance = variance
)
# untrace(GH_filter)

training_data <- training_data[row_train, ]

row_test <- utilitR::GH_filter(
  X = training_data,
  Y = testing_swiss_data,
  variables = variables,
  variance = variance
)

testing_swiss_data <- testing_swiss_data[row_test, ]

save(training_data, file = "data/training_data.rda")
save(testing_swiss_data, file = "data/testing_swiss_data.rda")





