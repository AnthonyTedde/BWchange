################################################################################
# Remove Brune and Normande
################################################################################
rm(list = ls())
library(magrittr)

data("dataset_original")
dim(dataset_original)



dataset_original$breed %>%
  table()

##
# Bodyweight distribution ALL country, no deletion
#

dataset_original %<>%
  dplyr::filter(! breed %in% c("Normande", "Brune"))
dim(dataset_original)


################################################################################
# Save
################################################################################

save(dataset_original, file = "data/dataset_original.rda")
