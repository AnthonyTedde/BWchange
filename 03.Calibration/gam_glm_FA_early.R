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

training_data$dim %>% hist
test_data$dim %>% hist
test_data_seenorest$dim %>% hist


# train_data_noseenorest <- train_data

predictor_v <- names(training_data)[
  stringr::str_detect(names(training_data),
                      pattern = "^dpin(?=[:digit:]{4})|^pred(?=[:digit:]{3})",
                      negate = T)
]

FAs <- predictor_v[stringr::str_detect(
  predictor_v,
  pattern = "^C(?=[:digit:])"
)]

sub_FAinfat <- stringr::str_detect(FAs, pattern = "\\_in\\_fat$")
FAs_in_fat <- FAs[sub_FAinfat]
FAs_rate <- FAs[!sub_FAinfat]

# ------------------------------------------------------------------------------
# Relationship ####
# ------------------------------------------------------------------------------

# Fatty acids
FA_df <- training_data %>%
  dplyr::select(bodyweight, dplyr::all_of(FAs_in_fat))

form <- paste("bodyweight" ,
              paste(FAs_in_fat, collapse = "+"),
              sep = "~") %>% formula()
pairs(form, FA_df)

old_par <- par()
cols <- 4
rows <- ceiling((ncol(FA_df) - 1)/cols) # remove BW
par(mfrow = c(rows, cols), oma = c(0, 0, 0, 0), xpd = NA)
plot(form, data = training_data, col = as.factor(training_data$provider))
legend(1, y = -5, ncol = 1,
       legend = levels(as.factor(training_data$provider)),
       col = as.factor(training_data$provider),
       pch = 1)
par(old_par)

plot_relationship <- function(){
  # arguments
  x <- FAs_rate
  y <- "bodyweight"
  dat <- dataset_cleaned

  #code
  dat %>%
    dplyr::select(dplyr::all_of(c(x, y, "provider"))) %>%
    tidyr::pivot_longer(cols = x) %>%
    ggplot2::ggplot(ggplot2::aes(x = value, y = bodyweight, color = provider)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~name, ncol = 4, scales = "free_x")
}


# ------------------------------------------------------------------------------
# Generalized linear model ####
# ------------------------------------------------------------------------------

ncmp <- 5

formulae_fa <- paste(FAs_in_fat, collapse = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>% formula()

(mvr_FAs <- pls::mvr(formulae_fa, data = training_data, ncomp = ncmp,
                    method = "oscorespls",
                    scale = T, center = T)) |> system.time()


vip_score <- (chillR::VIP(mvr_FAs) %>%
  colSums() / max(ncmp)) %>%
  sort(decreasing = T)

FAs_vip <- which(vip_score > .9) %>% names

# Update formulae_fa
formulae_fa <- paste(FAs_vip, collapse = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>% formula()


# ------------------------------------------------------------------------------
# gam1 for prediction
# TODO: long-chain, med-chain, short-chain, mono/poly-insaturated, saturated
# TODO: DMI
# ------------------------------------------------------------------------------

# FA
FAs_4_my_interaction <- stringr::str_detect(
  FAs_vip,
  pattern = "(^C10|^C14|^C16|^C18)\\_"
)


formulae_gam <- glue::glue("s({FAs_vip}, bs = 'cr')") %>%
  paste(collapse = "+") %>%
  paste("s(dim, bs = 'cr')",
        "parity",
        "s(milk_yield, bs = 'cr')" , ., sep = "+") %>%
  paste("ti(dim, milk_yield)", sep = "+") %>%
  paste("s(fat_rate_2_prd, bs = 'cr')",
        "s(protein_rate_1_prd, bs = 'cr')",
        "s(lactose_rate_1_prd, bs = 'cr')", sep = "+") %>%
  paste("ti(fat_rate_2_prd, milk_yield)",
        "ti(protein_rate_1_prd, milk_yield)",
        "ti(lactose_rate_1_prd, milk_yield)", sep = "+") %>%
  paste(glue::glue("ti({FAs_vip[FAs_4_my_interaction]}, milk_yield)") |>
          paste(collapse = "+"),
        sep = "+") %>%
  paste("bodyweight", ., sep = "~") %>% formula()

system.time(
  gam_FA_early <- mgcv::gam(
    formulae_gam,
    family = Gamma(link = "log"),
    data = training_data,
    method = "REML"
  )
)

mgcv::gam.check(gam_FA_early)

rmse <- function(d, m){
  tibble::tibble(
    prd = predict(m, newdata = d, type="response", ncomp = 15) %>% drop,
    tth = d$bodyweight
  )  %>%
    yardstick::rmse(truth = tth, estimate = prd)
}

rmse_byprovider <- function(d, m, by = "provider"){
  d %>%
    dplyr::group_by(provider) %>%
    dplyr::group_map(.f = function(d, k){
      tibble::tibble(
        prd = predict(m, newdata = d, type="response", ncomp = 15) %>% drop,
        tth = d$bodyweight
      ) %>%
        yardstick::rmse(truth = tth, estimate = prd) %>%
        tibble::add_column(provider = k, .before = 1)
    }) %>%
    purrr::reduce(dplyr::bind_rows)
}

rmse(training_data, gam_FA_early)
rmse_byprovider(training_data, gam_FA_early)
rmse(test_data, gam_FA_early)
rmse(test_data_seenorest, gam_FA_early)


# ------------------------------------------------------------------------------
# Save. ####
# ------------------------------------------------------------------------------

# save(train_data, file = "data/train_data.rda")
# save(test_data, file = "data/test_data.rda")
# save(train_data_noseenorest, file = "data/train_data_noseenorest.rda")

save(gam_FA_early, file = "data/gam_FA_early.rda")
