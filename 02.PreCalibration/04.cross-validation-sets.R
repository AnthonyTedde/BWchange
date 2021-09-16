rm(list = ls())
library(magrittr)
data("train_data")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Create cross-validation sets ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

##
# Create cross-validation animal independent
##
table(train_data$parity_fct)
table(train_data$test_MIR_season)

with(train_data, table(parity_fct, test_MIR_season))

set.seed(42)
train_cv_partition <- train_data %>%
  # Stratification by parity and season (merge both column)
  dplyr::mutate(parity_season = paste(parity_fct, test_MIR_season,
                                      sep = "_")) %>%
  rsample::vfold_cv(strata = "parity_season")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Save ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
save(train_cv_partition, file = "data/train_cv_partition.rda")
