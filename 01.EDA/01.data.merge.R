rm(list = ls())
library(bodyweight.data)
library(magrittr)


################################################################################
# Get the data
################################################################################

dataset_nm <- c("BW_GpE", "BW_HM_ch1", "BW_HM_fr1", "BW_HM_seenorest_2021",
                "BW_HSO_2019", "BW_Laura_2020", "BW_Ualberta_2020",
                "BW_Utrobe_2020")

data(list = dataset_nm)

source(here::here("globals/globals-models.R"))


################################################################################
# Merge
################################################################################

column_in_common <- mget(dataset_nm) %>%
  purrr::map(names) %>%
  purrr::reduce(intersect) %>%
  # Remove the spectral variable. Should be selected using the pin212_name
  # variable from the global file "globals/globals-models.R".
  grep(pattern = "^(?!pin|dpin)", value = T, perl = T)
length(column_in_common)

spectral_variable <- c(pin212_name, paste0("d", pin212_name))

mapout <- c("BW_GpE", "BW_HM_ch1", "BW_HM_fr1", "BW_HM_seenorest_2021",
           "BW_HSO_2019", "BW_Laura_2020", "BW_Ualberta_2020", "BW_Utrobe_2020")
mapin <- c("gpe", "swiss", "seenovia", "seenorest", "hso", "laura",
           "ualberta", "utrobe")
season <- c("spring", "summer", "autumn", "winter")

dataset_original <- mget(dataset_nm) %>%
  ## Add data provider
  purrr::imap(~tibble::add_column(.x, initial_dataset = .y)) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  ## Create more readable names
  dplyr::mutate(
    provider = plyr::mapvalues(initial_dataset, from = mapout, to = mapin),
    parity_full = parity,
    parity = ifelse(parity < 3, parity, 3),
    parity_fct = parity %>% factor(levels = 1:3,
                                   labels = c("first", "second", "third+")),
    wol_ord = cut(dim, breaks = seq(0,
                                    max(dim) + 7 - (max(dim) %% 7),
                                    by = 7), ordered_result = T)
  ) %>%
  ## Add hemisphere location
  dplyr::mutate(
    hemisphere = ifelse(provider == "utrobe", "southern", "northern"),
    # Start with spring
    test_MIR_season = dplyr::case_when(
      hemisphere == "southern" ~ lubridate::quarter(test_MIR_dt, fiscal_start = 10L),
      hemisphere == "northern" ~ lubridate::quarter(test_MIR_dt, fiscal_start = 4L)
    ) %>%
      plyr::mapvalues(from = 1:4, to = season) %>%
      factor(levels = season)
  ) %>%
  dplyr::select(provider, parity_full, parity, parity_fct, wol_ord,
                initial_dataset, hemisphere, test_MIR_season,
                dplyr::all_of(column_in_common),
                dplyr::all_of(spectral_variable)) %>%
  # id0: Initial row identifier.
  tibble::rowid_to_column(var = "id0")

# To presentation
with(
  dataset_original,
  table(provider, abbreviate(breed))
)


################################################################################
# Save
################################################################################

save(dataset_original, file = "data/dataset_original.rda")
