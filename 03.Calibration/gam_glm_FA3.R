library(magrittr)
# ------------------------------------------------------------------------------
# Data. Run rm before the following chunck
# ------------------------------------------------------------------------------
rm(list = ls())
data("train_data")
data("train_data_noseenorest")
data("test_data")
# train_data_noseenorest <- train_data
predictor_v <- names(train_data)[
  stringr::str_detect(names(train_data),
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
# Generalized linear model ####
# ------------------------------------------------------------------------------
ncmp <- 5
formulae_fa <- paste(FAs_in_fat, collapse = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>% formula()
(mvr_FAs <- pls::mvr(formulae_fa, data = train_data_noseenorest, ncomp = ncmp,
                    method = "oscorespls",
                    scale = T, center = T)) |> system.time()
vip_score <- (chillR::VIP(mvr_FAs) %>%
  colSums() / max(ncmp)) %>%
  sort(decreasing = T)
FAs_vip <- which(vip_score > 1) %>% names
# Update formulae_fa
formulae_fa <- paste(FAs_vip, collapse = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>% formula()
# ------------------------------------------------------------------------------
# mvr for prediction
# ------------------------------------------------------------------------------
formulae_pls <- paste(FAs_vip, collapse = "+") %>%
  paste("dim", "parity", "milk_yield" , ., sep = "+") %>%
  paste("dim:milk_yield", sep = "+") %>%
  paste("fat_rate_2_prd", "protein_rate_1_prd", "lactose_rate_1_prd", sep = "+") %>%
  paste("bodyweight", ., sep = "~") %>% formula()
(mvr_FA_pred <- pls::mvr(formulae_pls, ncomp = 10, data = train_data_noseenorest,
                        method = "oscorespls",
                        scale = T, center = T)) |> system.time()
tibble::tibble(
  pred = predict(mvr_FA_pred, newdata = train_data_noseenorest, comps = 10) |> drop(),
  obs = train_data_noseenorest$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = pred)
tibble::tibble(
  pred = predict(mvr_FA_pred,
                 newdata = test_data, comps = 10,
                 type = "response") |> drop(),
  obs = test_data$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = pred)
# ------------------------------------------------------------------------------
# some plot
# ------------------------------------------------------------------------------
with(train_data,{
  a <- fat_rate_2_prd / milk_yield
  plot(bodyweight ~ a)
})
with(train_data_noseenorest,{
  plot(bodyweight ~ solid_recovery_prd)
})
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
formulae_gam <- glue::glue("s({FAs_vip}, bs = 'tp')") %>%
  paste(collapse = "+") %>%
  paste("s(dim, bs = 'tp')",
        "parity",
        "s(milk_yield, bs = 'tp')" , ., sep = "+") %>%
  paste("ti(dim, milk_yield)", sep = "+") %>%
  paste("s(fat_rate_2_prd, bs = 'tp')",
        "s(protein_rate_1_prd, bs = 'tp')",
        "s(lactose_rate_1_prd, bs = 'tp')", sep = "+") %>%
  paste("ti(fat_rate_2_prd, milk_yield)",
        "ti(protein_rate_1_prd, milk_yield)",
        "ti(lactose_rate_1_prd, milk_yield)", sep = "+") %>%
  paste(glue::glue("ti({FAs_vip[FAs_4_my_interaction]}, milk_yield)") |>
          paste(collapse = "+"),
        sep = "+") %>%
  paste("bodyweight", ., sep = "~") %>% formula()

system.time(
  gam_FA3 <- mgcv::gam(
    formulae_gam,
    family = Gamma(link = "log"),
    # family = inverse.gaussian,
    # family = Gamma,
    data = train_data_noseenorest,
    method = "REML"
  )
)

mgcv::gam.check(gam_FA3)

tibble::tibble(
  pred = predict(gam_FA3,
                 newdata = train_data_noseenorest,
                 type = "response") |> drop(),
  obs = train_data_noseenorest$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = pred)

tibble::tibble(
  pred = predict(gam_FA3,
                 newdata = test_data,
                 type = "response") |> drop(),
  obs = test_data$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = pred)

# ------------------------------------------------------------------------------
# Save. ####
# ------------------------------------------------------------------------------
# save(train_data, file = "data/train_data.rda")
# save(test_data, file = "data/test_data.rda")
# save(train_data_noseenorest, file = "data/train_data_noseenorest.rda")
save(gam_FA3, file = "data/gam_FA3.rda")
