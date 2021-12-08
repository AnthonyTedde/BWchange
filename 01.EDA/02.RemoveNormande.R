library(magrittr)


# ------------------------------------------------------------------------------
# Remove Brune and Normande
# ------------------------------------------------------------------------------

rm(list = ls())
library(magrittr)

data("dataset_original")
dim(dataset_original)

dataset_original <- dataset_original[dataset_original$breed == "Holstein", ]
dataset_original$breed |> unique()

with(dataset_original,{
  table(breed, provider)
})

# dataset_original$provider %>%
#   table()
#
# dataset_original$breed %>%
#   table()
#
# ##
# # Bodyweight distribution ALL country, no deletion
# #
#
# dataset_original %<>%
#   dplyr::filter(! breed %in% c("Normande", "Brune"))
# dim(dataset_original)


# ------------------------------------------------------------------------------
# Remove inapropriate yield_tot
# ------------------------------------------------------------------------------
library(splines)
library(pls)
data("pls_test")
mdl <- pls_test

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


table(dataset_original$provider)
rmse(d = dataset_original %>%
       dplyr::filter(provider == "seenorest"),
     m = mdl)
rmse_byprovider(d = dataset_original, m = mdl)

with(dataset_original,{
  all(is.na(yield_tot[provider == "utrobe"]))
})
with(dataset_original,{
  all(is.na(yield_tot[provider == "utrobe"]))
})


dataset_original %<>%
  dplyr::mutate(
    yield_tot = ifelse(is.na(yield_tot), milk_yield, yield_tot),
    yield_ratio = (yield_tot /  milk_yield) %>%
      ifelse(. > 1, .^(-1), .)
  ) %>%
  dplyr::filter(
    yield_ratio > .8
  )

with(dataset_original,{
  table(breed, provider)
})
rmse_byprovider(dataset_original, mdl)

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------

save(dataset_original, file = "data/dataset_original.rda")
