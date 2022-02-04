library(utilitR)
library(tibble)
rm(list = ls())
data("training_data")

# trace(create_kfold, edit = T)
stratified_kfolds <- create_kfold(training_data,
                                  k = 10,
                                  strata = "strata_uid",
                                  seed = 42)
#untrace(create_kfold)

save(stratified_kfolds, file = "data/stratified_kfolds.rda", compress = "xz")

sapply(stratified_kfolds, FUN = function(x){
   table(training_data[x, "strata_uid"])
})

sapply(stratified_kfolds, length)
