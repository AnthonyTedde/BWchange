rm(list = ls())
library(bodyweight.data)
library(magrittr)


# ------------------------------------------------------------------------------
# Get the data ####
# ------------------------------------------------------------------------------

dataset_nm <- c("BW_GpE", "BW_HM_ch1", "BW_HM_fr1", "BW_HM_seenorest_2021",
                "BW_HSO_2019", "BW_Laura_2020", "BW_Ualberta_2020",
                "BW_Utrobe_2020")

data(list = dataset_nm)

source(here::here("globals/globals-models.R"))


# ------------------------------------------------------------------------------
# Merge
# ------------------------------------------------------------------------------

column_in_common <- mget(dataset_nm) %>%
  purrr::map(names) %>%
  purrr::reduce(intersect) %>%
  # Remove the spectral variable. Should be selected using the pin212_name
  # variable from the global file "globals/globals-models.R".
  grep(pattern = "^(?!pin|dpin)", value = T, perl = T)
length(column_in_common)

# spectral_variable <- c(paste0("d", pin212_name))
# spectral_variable <- c(paste0("d", pin212_name))
# spectral_variable <- c(pin277_name, paste0("d", pin277_name))

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
    # For backward compatibility
    parity_4 = ifelse(parity_full < 4, parity_full, 4),
    parity_fct = parity %>% factor(levels = 1:3,
                                   labels = c("first", "second", "third+")),
    parity_ord = ordered(parity_fct),
    wol_ord = cut(dim, breaks = seq(0,
                                    max(dim) + 7 - (max(dim) %% 7),
                                    by = 7), ordered_result = T)
  ) %>%
  ## Add yield_tot if does not exist
  tibble::add_column(., !!!setdiff(c("yield_tot"), names(.))) %>%
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
  tibble::rowid_to_column(var = "id0")

dataset_spectra <- dataset_original %>%
  dplyr::select(id0, an_id, expl_id, provider, test_MIR_dt,
                parity_full, dim, milk_yield,
                dplyr::all_of(
                  paste0("pin", stringr::str_pad(1:1060, width = 4, pad = "0"))
                ))

dataset_original %<>%
  dplyr::select(id0, provider,
                parity_full, parity, parity_4, parity_fct, parity_ord,
                wol_ord, initial_dataset, hemisphere, test_MIR_season,
                yield_tot,
                dplyr::all_of(column_in_common),
                dplyr::starts_with("pin"),
                dplyr::starts_with("dpin"))
                # dplyr::all_of(spectral_variable))

# ------------------------------------------------------------------------------
# Some Graph
# ------------------------------------------------------------------------------

# source(file = here::here("helper", "plot_desc.R"))
# source(file = here::here("helper", "plot_desc2.R"))
# plot_desc(dataset_original)
# plot_desc2(dataset_original %>% dplyr::filter(breed == "Holstein"))

with(dataset_original,
     table(provider, breed))

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------

dataset_spectra %<>%
  dplyr::mutate(
    expl_id = dplyr::if_else(is.na(expl_id), provider, expl_id)
  ) %>%
  tidyr::drop_na()

dim(dataset_original)

write.csv(dataset_spectra,
          file = "extdata/dataset_spectra.csv", row.names = F)

save(dataset_original, file = "data/dataset_original.rda")
save(dataset_spectra, file = "data/dataset_spectra.rda")
