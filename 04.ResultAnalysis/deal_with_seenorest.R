rm(list = ls())
library(magrittr)
data("pls_fit_bst")
data("dataset_cleaned")
library(bodyweight.data)
data("BW_HM_seenorest_2021")

seenorest_new <- BW_HM_seenorest_2021 %>%
  dplyr::mutate(
    parity_full = parity,
    parity = ifelse(parity < 3, parity, 3)
  )

model <- pls_fit_bst

broom::augment(model, new_data = seenorest_new) %>%
  yardstick::rmse(truth = bodyweight, estimate = .pred)


with(dataset_cleaned, table(provider))


broom::augment(model, new_data = dataset_cleaned) %>%
  dplyr::group_by(provider) %>%
  dplyr::group_map(.f = function(d, k){
    d %>%
      yardstick::rmse(truth = bodyweight, estimate = .pred) %>%
      tibble::add_column(provider = k, .before = 1)
  }, .keep = T) %>%
  purrr::reduce(dplyr::bind_rows)

augmented_dataset <- broom::augment(model, new_data = dataset_cleaned)


# Plot 1
augmented_dataset %>%
  dplyr::filter(provider %in% c("swiss", "seenorest")) %>%
  ggplot2::ggplot(ggplot2::aes(y = .pred, x = bodyweight, color = provider)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(slope = 1, intercept = 0)

# plot2 By year
augmented_dataset %>%
  dplyr::filter(provider %in% c("seenorest")) %>%
  dplyr::mutate(test_MIR_year = factor(lubridate::year(test_MIR_dt))) %>%
  ggplot2::ggplot(ggplot2::aes(y = .pred, x = bodyweight,
                               color = test_MIR_year)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(slope = 1, intercept = 0)

augmented_dataset %>%
  dplyr::filter(provider %in% c("seenorest")) %>%
  dplyr::mutate(test_MIR_year = factor(lubridate::year(test_MIR_dt))) %>%
  dplyr::group_by(test_MIR_year) %>%
  dplyr::group_map(.f = function(d, k){
    d %>%
      yardstick::rmse(truth = bodyweight, estimate = .pred) %>%
      tibble::add_column(provider = k, .before = 1)
  }, .keep = T) %>%
  purrr::reduce(dplyr::bind_rows)


# plot2 By parity
augmented_dataset %>%
  dplyr::filter(provider %in% c("seenorest")) %>%
  ggplot2::ggplot(ggplot2::aes(y = .pred, x = bodyweight,
                               color = parity_fct)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(slope = 1, intercept = 0)



# plot2 By dim
augmented_dataset %>%
  dplyr::filter(provider %in% c("seenorest")) %>%
  dplyr::mutate(
    dim_50 = cut(dim, breaks = seq(0, 400, by = 100))
  ) %>%
  ggplot2::ggplot(ggplot2::aes(y = .pred, x = bodyweight,
                               color = dim_50)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(slope = 1, intercept = 0)


augmented_dataset %>%
  dplyr::filter(provider %in% c("seenorest")) %>%
  dplyr::mutate(
    dim_50 = cut(dim, breaks = seq(0, 400, by = 100))
  ) %>%
  dplyr::group_by(dim_50) %>%
  dplyr::group_map(.f = function(d, k){
    d %>%
      yardstick::rmse(truth = bodyweight, estimate = .pred) %>%
      tibble::add_column(provider = k, .before = 1)
  }, .keep = T) %>%
  purrr::reduce(dplyr::bind_rows)



# plot2 milk
augmented_dataset %>%
  dplyr::filter(provider %in% c("seenorest")) %>%
  dplyr::select(dim, milk_yield, bodyweight, .pred) %>%
  tidyr::pivot_longer(cols = c(bodyweight, .pred), values_to = "bodyweight") %>%
  ggplot2::ggplot(ggplot2::aes(x = milk_yield, y = bodyweight)) +
  ggplot2::geom_point()+
  ggplot2::facet_wrap(~name)


augmented_dataset %>%
  dplyr::filter(provider %in% c("swiss")) %>%
  dplyr::select(dim, milk_yield, bodyweight, .pred) %>%
  tidyr::pivot_longer(cols = c(bodyweight, .pred), values_to = "bodyweight") %>%
  ggplot2::ggplot(ggplot2::aes(x = milk_yield, y = bodyweight)) +
  ggplot2::geom_point()+
  facet_wrap(~name)


# plot2 dim
augmented_dataset %>%
  dplyr::filter(provider %in% c("seenorest")) %>%
  dplyr::select(parity_fct, dim, milk_yield, bodyweight, .pred) %>%
  tidyr::pivot_longer(cols = c(bodyweight, .pred), values_to = "bodyweight") %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight)) +
  ggplot2::coord_cartesian(ylim = c(400, 960)) +
  ggplot2::geom_point()+
  ggplot2::facet_grid(parity_fct~name)


augmented_dataset %>%
  dplyr::filter(provider %in% c("swiss")) %>%
  dplyr::select(parity_fct, dim, milk_yield, bodyweight, .pred) %>%
  tidyr::pivot_longer(cols = c(bodyweight, .pred), values_to = "bodyweight") %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight)) +
  ggplot2::coord_cartesian(ylim = c(400, 960)) +
  ggplot2::geom_point()+
  ggplot2::facet_grid(parity_fct~name)
