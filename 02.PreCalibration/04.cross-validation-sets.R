rm(list = ls())
library(magrittr)
data("training_data")

#-------------------------------------------------------------------------------
# Create cross-validation sets ####
#-------------------------------------------------------------------------------

set.seed(1010)
# Outside cross-validation: animal independent
# outside_cv <- training_data %>%
train_cv_partition <- training_data %>%
  rsample::group_vfold_cv(group = strata_uid, v = 10)
  # rsample::group_vfold_cv(group = an_uid, v = 5)

# train_cv_partition <- training_data %>%
#   rsample::nested_cv(
#     outside = outside_cv,
#     inside = rsample::group_vfold_cv(group = strata_uid, v = 5)
#   )
#
# lobstr::obj_size(train_cv_partition)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Save ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
save(train_cv_partition, file = "data/train_cv_partition.rda",
     compress = "xz")
