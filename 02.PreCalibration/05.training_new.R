rm(list = ls())
library(magrittr)
library(bodyweight.data)
data("dataset_cleaned")
data("BW_ch1")
source("globals/globals-models.R")


# ------------------------------------------------------------------------------
# Training_data
# ------------------------------------------------------------------------------

holstein_data <-  dataset_cleaned %>%
  dplyr::filter(breed == "Holstein") %>%
  dplyr::mutate(
    lactation_stage = dplyr::case_when(
      dim < 100 ~ "early",
      dim < 200 ~ "mid",
      T ~ "late",
    ) %>% factor(levels = c("early", "mid", "late"))
  ) %>%
  tibble::rowid_to_column(var = "uid") %>%
  dplyr::group_by(an_id) %>%
  dplyr::mutate(an_uid = dplyr::cur_group_id()) %>%
  dplyr::group_by(parity_fct, lactation_stage, provider) %>%
  dplyr::mutate(strata_uid = dplyr::cur_group_id()) %>%
  dplyr::ungroup()

training_data <- holstein_data %>%
  dplyr::filter(
    provider %in% c("hso", "gpe", "seenovia", "utrobe", "ualberta")
  ) %>%
  dplyr::select(
    uid, an_uid, strata_uid,
    bodyweight, dim, parity_ord, milk_yield,
    paste0("d", pin212_name)
  )

# Objects sizes
lobstr::obj_size(training_data)


# ------------------------------------------------------------------------------
# Testing_data
# ------------------------------------------------------------------------------

# (!) an_id and provider is needed for merging purpose on validation stage
cols <- c("an_id", "provider", "parity_full", names(training_data))

testing_swiss_data <-   holstein_data %>%
  dplyr::filter(provider %in% c("swiss")) %>%
  dplyr::select(dplyr::all_of(cols)) %>%
  dplyr::mutate(
    an_id = sub(pattern = "[[:alpha:]]+", replacement = "", an_id) %>%
      stringr::str_pad(width = 12, pad = "0") %>%
      paste0("CHE", .)
  )


# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------

save(training_data, file = "data/training_data.rda")
save(holstein_data, file = "data/holstein_data.rda")
save(testing_swiss_data, file = "data/testing_swiss_data.rda")
