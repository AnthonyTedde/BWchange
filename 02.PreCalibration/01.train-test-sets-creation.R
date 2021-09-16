################################################################################
#
################################################################################

rm(list = ls())
library(magrittr)
data("dataset_cleaned")

Holstein_data <-  dataset_cleaned %>%
  dplyr::filter(breed == "Holstein")

table(Holstein_data$provider)

train_data <- Holstein_data %>%
  dplyr::filter(provider %in% c("gpe", "hso", "seenovia", "ualberta", "utrobe"))
test_data <- Holstein_data %>%
  dplyr::filter(provider %in% c("swiss"))
out_of_sample <- Holstein_data %>%
  dplyr::filter(provider == "seenorest")


save(train_data, file = "data/train_data.rda")
save(test_data, file = "data/test_data.rda")
save(out_of_sample, file = "data/out_of_sample.rda")
