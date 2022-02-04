data("training_data")
data("testing_swiss_data")
data("stratified_kfolds")

for(i in 1:length(stratified_kfolds)){
  training_data[stratified_kfolds[[i]], "fold"] <- i
}
training_data$fold

feather::write_feather(training_data,
                       path = "data/training_data.feather")
feather::write_feather(testing_swiss_data,
                       path = "data/testing_swiss_data.feather")

