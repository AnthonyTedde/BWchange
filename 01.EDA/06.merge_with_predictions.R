library(magrittr)
library(bodyweight.data)
rm(list = ls())

data("predictions_BW")
data("dataset_cleaned_phs1")

names(predictions_BW)

# ------------------------------------------------------------------------------
# Merge both dataset. ####
# ------------------------------------------------------------------------------

dataset_cleaned_phs1 <- predictions_BW %>%
  dplyr::mutate(parity_full = parity) %>%
  dplyr::select(-c(id, milk_yield, parity)) %>%
  dplyr::inner_join(dataset_cleaned_phs1)


# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------

save(dataset_cleaned_phs1, file = "data/dataset_cleaned_phs1.rda")
