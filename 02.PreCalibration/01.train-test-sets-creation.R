rm(list = ls())
library(magrittr)
library(bodyweight.data)
data("dataset_cleaned")
data("BW_ch1")

holstein_data <-  dataset_cleaned %>%
  dplyr::filter(breed == "Holstein") %>%
  dplyr::mutate(bodyweight = as.vector(bodyweight)) %>%
  dplyr::mutate(
    lactation_stage = dplyr::case_when(
      dim < 100 ~ "early",
      dim < 200 ~ "mid",
      T ~ "late",
    ) %>% factor(levels = c("early", "mid", "late"))
  ) %>%
  dplyr::group_by(parity_fct, lactation_stage, provider) %>%
  dplyr::mutate(strata_uid = dplyr::cur_group_id()) %>%
  dplyr::ungroup()

# Check the strata_uid:
holstein_data %>%
  dplyr::select(parity_fct, lactation_stage, provider, strata_uid, dim)

out_of_sample_bw <- BW_ch1 %>%
  dplyr::mutate(
    parity_full = parity,
    parity = ifelse(parity_full < 3, parity_full, 3),
    parity_4 = ifelse(parity_full < 4, parity_full, 4)
  )

# ------------------------------------------------------------------------------
# Training_data
# ------------------------------------------------------------------------------

training_data <- holstein_data %>%
  dplyr::filter(
    provider %in% c("hso", "gpe", "seenovia", "utrobe", "ualberta")
  ) %>%
  dplyr::select(
    uid, an_uid, strata_uid,
    bodyweight, dim, parity_fct, milk_yield,
    dplyr::starts_with("dpin")
  )

# Objects sizes
lobstr::obj_size(training_data)
length(training_data) * lobstr::obj_size(training_data$bodyweight)


# ------------------------------------------------------------------------------
# Testing_data
# ------------------------------------------------------------------------------
# (!) an_id and provider is needed for merging purpose on validation stage
cols <- c("an_id", "provider", "parity_full", names(training_data))

swiss_data <-   holstein_data %>%
  dplyr::filter(provider %in% c("swiss")) %>%
  dplyr::select(dplyr::all_of(cols)) %>%
  dplyr::mutate(
    an_id = sub(pattern = "[[:alpha:]]+", replacement = "", an_id) %>%
      stringr::str_pad(width = 12, pad = "0") %>%
      paste0("CHE", .)
  )

testing_data <- holstein_data %>%
  dplyr::filter(provider %in% c("seenorest")) %>%
  dplyr::bind_rows(swiss_data) %>%
  dplyr::mutate(bodyweight = as.vector(bodyweight)) %>%
  dplyr::select(dplyr::all_of(cols))
lobstr::obj_size(testing_data)

holstein_data %<>%
  dplyr::select(cols)


# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------

save(training_data, file = "data/training_data.rda")
save(holstein_data, file = "data/holstein_data.rda")
save(testing_data, file = "data/testing_data.rda")
save(out_of_sample_bw, file = "data/out_of_sample_bw.rda")


