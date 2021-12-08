rm(list = ls())
library(magrittr)

data("dataset_original")

################################################################################
# There is NAs in spectral data ?
# Look by dataset
################################################################################

dataset_original %>%
  dplyr::select(an_id, provider, dplyr::contains("dpin")) %>%
  naniar::gg_miss_fct(fct = provider)

# Remove the rows with missing spectra

dataset_original %<>%
  tidyr::drop_na(dplyr::contains("dpin"))
dim(dataset_original)

# Is there some NA amongs spectra ?
dataset_original %>%
  dplyr::select(dplyr::starts_with("pin")) %>%
  naniar::pct_complete()
# No more.

################################################################################
# Other than spectra
################################################################################

# Percentage of column with at least one NA in it.
dataset_original %>%
  naniar::pct_miss_var()
#~ > 0.52%


############################################
# Plot and table miss var
############################################

dataset_original %>%
  dplyr::select(-dplyr::contains("pin")) %>%
  naniar::gg_miss_var(facet = country,
                      show_pct = T)

##
# Insert original dataset if expl_id is missing
##
dataset_original %<>%
  dplyr::mutate(
    expl_id = ifelse(is.na(expl_id), provider, expl_id)
  )

dataset_original %>%
  naniar::gg_miss_upset()


dataset_original %>%
  dplyr::select(-dplyr::contains("pin")) %>%
  naniar::gg_miss_fct(fct = breed)

dataset_original %>%
  dplyr::select(-dplyr::contains("pin")) %>%
  naniar::gg_miss_fct(fct = country)

##
# Remove record without BW of MY
##
dataset_original %<>%
  dplyr::filter(dplyr::across(-c("project", "lactose_rate",
                                 "protein_rate", "fat_rate"),
                              .fns = ~ ! is.na(.)))
dim(dataset_original)

#####################
# Save
#####################
save(dataset_original, file = "data/dataset_original.rda")
