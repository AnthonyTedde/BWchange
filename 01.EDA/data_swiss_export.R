library(magrittr)
library(bodyweight.data)
rm(list = ls())
data("dataset_original")
data("BW_ch1")

dim(dataset_original)


a <- dataset_original %>%
  dplyr::filter(provider == "swiss", dim < 365) %>%
  dplyr::group_by(an_id, parity_full) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_step_d = (max(dim) - min(dim))/n
  ) %>%
  dplyr::mutate(an_id = gsub("[[:alpha:]]", "", an_id)) %>%
  dplyr::mutate(an_id = stringr::str_pad(an_id, width = 12, pad = "0")) %>%
  dplyr::filter(n > 2, mean_step_d <= 20) %>%
  dplyr::arrange(mean_step_d)


b <- BW_ch1 %>%
  dplyr::mutate(
    parity_full = parity
  ) %>%
  dplyr::count(an_id, parity_full) %>%
  dplyr::mutate(an_id = gsub("[[:alpha:]]", "", an_id))

c <- dplyr::inner_join(a, b, by = c("an_id", "parity_full"))

a %>%
  dplyr::select(an_id, parity_full) %>%
  write.csv(file = "extdata/suisse_julie_export.csv", row.names = F)


a %>%
  dplyr::select(an_id, parity_full) %>%
  dplyr::mutate(merged = glue::glue(
    "(an_id='{an_id}' AND parity='{parity_full}')"
  )) %>%
  dplyr::pull(merged) %>%
  paste(collapse = " OR ")
