rm(list = ls())
library(magrittr)
library(bodyweight.data)
data("BW_fr1")
data("BW_seenorest")
data("dataset_original")

dataset_original$provider %>%
  table()

augment_df <- function(datain){

  datain %>%
    dplyr::group_by(expl_id, an_id, parity_full) %>%
    dplyr::group_map(.f = function(dat, key){

      reg <- mgcv::gam(bodyweight ~ s(dim, k = nrow(dat)/5),
                       data=dat,
                       family = Gamma(link = "log"),
                       method = "REML")

      # reg <- mgcv::gam(bodyweight ~ s(dim, bs="cr", k = 10), data=dat,
      #                  family = Gamma(link = "log"), method = "REML")
      # Output dataframe
      key %>%
        dplyr::mutate(
          mdl_prd = tibble::lst(lm = reg),
          dataset = tibble::lst(dat),
          max_dim = max(dat$dim),
          min_dim = min(dat$dim)
        )
    }, .keep = T) %>%
    purrr::reduce(dplyr::bind_rows)

}



BW_fr1 %<>%
  dplyr::ungroup() %>%
  dplyr::mutate(parity_full = parity)
BW_seenorest %<>%
  dplyr::ungroup() %>%
  dplyr::mutate(parity_full = parity)


# ------------------------------------------------------------------------------
# Seenovia ####
# ------------------------------------------------------------------------------

rowtokeep <- BW_fr1 %>%
  dplyr::group_by(an_id, expl_id, parity_full) %>%
  dplyr::summarise(
    n = dplyr::n(),
    dim_mean_step = (max(dim) - min(dim)) / n
  ) %>%
  # dplyr::filter(parity_full < 5) %>%
  dplyr::filter(dim_mean_step < 6 & n > 20) %>%
  dplyr::select(expl_id, an_id, parity_full)

sub <- BW_fr1 %>%
  dplyr::inner_join(rowtokeep) %>%
  augment_df() %>%
  dplyr::select(expl_id, an_id, parity_full, max_dim, min_dim, mdl_prd) %>%
  dplyr::mutate(an_id = paste0("FR", an_id))

seenoviatokeep <- dataset_original %>%
  dplyr::filter(provider == "seenovia") %>%
  dplyr::inner_join(sub) %>%
  dplyr::filter(dim > min_dim & dim < max_dim) %>%
  dplyr::rowwise(dplyr::everything()) %>%
  #ICI bodyweight <-> bodyweight2 if donot want to erase former bw value.
  dplyr::mutate(bodyweight = predict(
    mdl_prd,
    tibble::tibble(dim),
    type = "response")
  )

#ICI
# seenoviatokeep %>%
#   dplyr::ungroup() %>%
#   dplyr::select(bodyweight, bodyweight2) %>%
#   ggplot2::ggplot(ggplot2::aes(x = bodyweight, y = bodyweight2)) +
#   ggplot2::geom_point()

# ------------------------------------------------------------------------------
# Seenorest ####
# ------------------------------------------------------------------------------

rowtokeep <- BW_seenorest %>%
  dplyr::group_by(an_id, expl_id, parity_full) %>%
  dplyr::summarise(
    n = dplyr::n(),
    dim_mean_step = (max(dim) - min(dim)) / n
  ) %>%
  # ici
  # dplyr::filter(parity_full < 5) %>%
  dplyr::filter(dim_mean_step < 6 & n > 20) %>%
  dplyr::select(expl_id, an_id, parity_full)

sub <- BW_seenorest %>%
  dplyr::inner_join(rowtokeep) %>%
  augment_df() %>%
  dplyr::select(expl_id, an_id, parity_full, max_dim, min_dim, mdl_prd)

# TODO: Compare with a bodyweight2

seenorest <- dataset_original %>%
  dplyr::filter(provider == "seenorest") %>%
  dplyr::inner_join(sub) %>%
  dplyr::filter(dim > min_dim & dim < max_dim) %>%
  dplyr::rowwise(dplyr::everything()) %>%
  #ICI bodyweight <-> bodyweight2 if donot want to erase former bw value.
  dplyr::mutate(bodyweight = predict(
    mdl_prd,
    tibble::tibble(dim),
    type = "response")
  )


# ------------------------------------------------------------------------------
# Transform dataset ####
# ------------------------------------------------------------------------------

dataset_original %<>%
  dplyr::filter(provider != "seenovia") %>%
  dplyr::bind_rows(seenoviatokeep)
dataset_original %<>%
  dplyr::filter(provider != "seenorest") %>%
  dplyr::bind_rows(seenorest)

# Limit the parity
dataset_original %<>%
  # dplyr::filter(parity_full < 5) %>%
  dplyr::select(-c("min_dim", "max_dim")) %>%
  dplyr::arrange(id0)

# test the values
data("pls_test")
mdl <- pls_test
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


with(dataset_original,{
  table(breed, provider)
})

rmse_byprovider(d = dataset_original, m = mdl)

source(file = here::here("helper", "plot_desc2.R"))
plot_desc2(dataset_original %>% dplyr::filter(breed == "Holstein"))

save(dataset_original, file = "data/dataset_original.rda")
