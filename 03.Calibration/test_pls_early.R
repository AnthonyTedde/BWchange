library(magrittr)
rm(list = ls())
data("test_data")
data("train_data")
# data("train_data_noseenorest")
data("train_data_noseenorest")
data("dataset_cleaned")
source("globals/globals-models.R")
library(splines)

train_data_seenorest <- train_data %>%
  dplyr::filter(provider == "seenorest")
train_data_noseenorest %>% dim
train_data_noseenorest$wol_ord %>% hist
train_data_noseenorest$dim %>% hist

dataset_cleaned %>%
  dplyr::filter(dim <= 100) %>%
  with(table(breed, provider))

training_data <- train_data_noseenorest %>%
  dplyr::filter(dim <= 100)
test_data <- test_data %>%
  dplyr::filter(dim <= 100)
test_data_seenorest <- train_data %>%
  dplyr::filter(provider == "seenorest", dim <= 100)


# ------------------------------------------------------------------------------
# pls
# ------------------------------------------------------------------------------
rmse <- function(d, m){
  tibble::tibble(
    prd = predict(m, newdata = d, ncomp = 15) %>% drop,
    tth = d$bodyweight
  )  %>%
    yardstick::rmse(truth = tth, estimate = prd)
}

rmse_byprovider <- function(d, m, by = "provider"){
  d %>%
    dplyr::group_by(provider) %>%
    dplyr::group_map(.f = function(d, k){
      tibble::tibble(
        prd = predict(m, newdata = d, ncomp = 15) %>% drop,
        tth = d$bodyweight
      ) %>%
        yardstick::rmse(truth = tth, estimate = prd) %>%
        tibble::add_column(provider = k, .before = 1)
    }) %>%
    purrr::reduce(dplyr::bind_rows)
}

form <- paste0("d", pin212_name) %>%
  paste(collapse = " + ") %>%
  paste("ns(milk_yield, df=3)", "ns(dim, df=3)", "parity",
        "parity:milk_yield", "dim:milk_yield",
        ., sep = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>% formula()

pls_early <- pls::mvr(form, ncomp = 15,
                     data = training_data,
                     method = "oscorespls",
                     scale = T, center = T)


rmse(training_data, pls_early)
rmse_byprovider(training_data, pls_early)
rmse(test_data, pls_early)
rmse(test_data_seenorest, pls_early)



save(pls_early, file = "data/pls_early.rda")
