library(magrittr)
rm(list = ls())
data("test_data")
data("train_data")
source("globals/globals-models.R")
library(splines)

train_data %<>%
  dplyr::filter(provider != "seenorest")



# ------------------------------------------------------------------------------
# Global function
# ------------------------------------------------------------------------------

rmse <- function(mod, dat){
  tibble::tibble(
    prd = predict(mod, dat, type = "response") %>%
      drop,
    obs = dat$bodyweight
  ) %>%
    yardstick::rmse(truth = obs, estimate = prd)
}


# ------------------------------------------------------------------------------
# PLS
# ------------------------------------------------------------------------------

form <- paste0("d", pin212_name) %>%
  paste(collapse = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>% formula()

ncomp <- 6

pls_spct <- pls::mvr(
  form, ncomp = ncomp,
  data = train_data,
  method = "oscorespls",
  scale = T, center = T
)

# ncomp <- (cumsum(pls_spct$Xvar /  pls_spct$Xtotvar) < .9) %>%
#   which %>%
#   seq_along()

train_data %<>%
  tibble::add_column(
    predict(pls_spct, newdata = train_data, ncomp = ncomp, type = "scores") %>%
      tibble::as_tibble() %>%
      setNames(nm = paste0("Comp", stringr::str_pad(ncomp, width = 2, pad = "0")))
  )
test_data %<>%
  tibble::add_column(
    predict(pls_spct, newdata = test_data, ncomp = ncomp, type = "scores") %>%
      tibble::as_tibble() %>%
      setNames(nm = paste0("Comp", stringr::str_pad(ncomp, width = 2, pad = "0")))
  )

# ------------------------------------------------------------------------------
# Extract the most important pin variable
# ------------------------------------------------------------------------------
vip_score <- (chillR::VIP(pls_spct) %>%
  colSums() / max(ncomp)) %>%
  sort(decreasing = T)

dpins_vip <- which(vip_score > 1) %>% names

# ------------------------------------------------------------------------------
# glmnet: elasticnet + glm
# ------------------------------------------------------------------------------

form_pls <- names(train_data) %>%
  grep(pattern = "^Comp", value = T) %>%
  paste(collapse = " + ") %>%
  paste("milk_yield", "dim", "parity",
        "dim:milk_yield", "parity:milk_yield",
        ., sep = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>%
  formula()

form_vip <- paste(dpins_vip, collapse = " + ") %>%
  paste("milk_yield", "dim", "parity",
        "dim:milk_yield", "parity:milk_yield",
        ., sep = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>%
  formula()


glm_pls <- glm(form_pls, family = Gamma, data = train_data)
glm_vip <- glm(form_vip, family = Gamma, data = train_data)

rmse(glm_pls, dat = train_data)
rmse(glm_pls, dat = test_data_swiss)
rmse(glm_vip, dat = train_data)
rmse(glm_vip, dat = test_data_swiss)


# ------------------------------------------------------------------------------
# GAM
# ------------------------------------------------------------------------------

form_gam_pls <- names(train_data) %>%
  grep(pattern = "^Comp", value = T) %>%
  paste(collapse = " + ") %>%
  paste("s(milk_yield)", "s(dim)", "parity",
        "ti(dim, milk_yield, k=6)", "parity:milk_yield",
        ., sep = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>%
  formula()

form_gam_vip <- paste(dpins_vip, collapse = " + ") %>%
  paste("s(milk_yield)", "s(dim)", "parity",
        "ti(dim, milk_yield, k=6)", "parity:milk_yield",
        ., sep = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>%
  formula()

train_spectra_cor <- train_data %>%
  dplyr::select(dplyr::all_of(dpins_vip)) %>%
  cor
train_spectra_cor %>%
  corrplot::corrplot(method = "square", order = "hclust")
train_spectra_dist <- dist(train_spectra_cor)
train_spectra_hclust <- hclust(train_spectra_dist)
plot(train_spectra_hclust, hang = -3)
memb <- cutree(train_spectra_hclust, k = 10)

dpin1 <- names(memb[which(memb == 1)])
dpin2 <- names(memb[which(memb == 2)])
dpin3 <- names(memb[which(memb == 3)])
dpin4 <- names(memb[which(memb == 4)])
dpin5 <- names(memb[which(memb == 5)])
dpin6 <- names(memb[which(memb == 6)])
dpin7 <- names(memb[which(memb == 7)])
dpin8 <- names(memb[which(memb == 8)])
dpin9 <- names(memb[which(memb == 9)])
dpin10 <- names(memb[which(memb == 10)])
# dpin11 <- names(memb[which(memb == 11)])
# dpin11 <- names(memb[which(memb == 11)])

train_spectra_cor <- train_data %>%
  dplyr::select(dplyr::all_of(dpin1)) %>%
  cor


form_gam_vip <- bodyweight ~ s(milk_yield) + s(dim) + parity +
  ti(dim, milk_yield, k = 6) + parity:milk_yield +
  dpin0045 + dpin0046 + dpin0044 + dpin0051 +
  dpin0506 + dpin0049 + dpin0048 + dpin0050 + dpin0212 + dpin0047 + dpin0145 +
  dpin0036 + dpin0016 + dpin0037 + dpin0211 + dpin0015 + dpin0029 + dpin0030 +
  dpin0517 + dpin0060 + dpin0123 + dpin0124 + dpin0106 + dpin0107 + dpin0059 +
  dpin0038 + dpin0105 + dpin0498 + dpin0125 + dpin0210 + dpin0497 + dpin0516 +
  dpin0126 + dpin0058 + dpin0127 + dpin0108 + dpin0499 + dpin0507 + dpin0515 +
  dpin0057 + dpin0496 + dpin0130 + dpin0128 + dpin0056 + dpin0129 + dpin0131 +
  dpin0033 + dpin0032 + dpin0019 + dpin0034 + dpin0018 + dpin0012 + dpin0111 +
  dpin0074 + dpin0020 + dpin0112 + dpin0026 + dpin0027 + dpin0110 + dpin0155 +
  dpin0013 + dpin0154 + dpin0025 + dpin0115 + dpin0156 + dpin0021 +
  dpin0073 + dpin0230 + dpin0225 + dpin0229 + dpin0224 + dpin0226 + dpin0118 +
  dpin0227 + dpin0119 + dpin0223 + dpin0500 + dpin0501 + dpin0502 + dpin0117 +
  dpin0228 + dpin0072 + dpin0116 + dpin0102 + dpin0520 + dpin0503 + dpin0521 +
  dpin0103 + dpin0216 + dpin0217 + dpin0095 + dpin0529 + dpin0120 + dpin0215 +
  dpin0170 + dpin0522 + dpin0064 + dpin0519 + dpin0530 + dpin0063 + dpin0096 +
  dpin0065 + dpin0101 + dpin0528 + dpin0218 + dpin0169 + dpin0214 + dpin0094 +
  dpin0161 +
  dpin0017 + dpin0023 + dpin0014 + dpin0028 + dpin0024 + dpin0022 +
  dpin0035 + dpin0031 + dpin0113 + dpin0075 + dpin0078 +
  dpin0137 + dpin0061 +
  dpin0052 + dpin0135 + dpin0043 + dpin0109 + dpin0104 + dpin0040 + dpin0039 +
  dpin0144 + dpin0122 + dpin0160 + dpin0505 + dpin0098 + dpin0099 + dpin0525 +
  dpin0092 + dpin0159 + dpin0518 + dpin0091 +
  ti(dpin0045, dpin0046, dpin0044, dpin0051)
  # ti(dpin0506, dpin0049, dpin0048, dpin0050, dpin0212, dpin0047, dpin0145) +
  # ti(dpin0036, dpin0016, dpin0037, dpin0211, dpin0015, dpin0029, dpin0030,
  #    dpin0517, dpin0060, dpin0123, dpin0124, dpin0106, dpin0107, dpin0059,
  #    dpin0038, dpin0105, dpin0498, dpin0125, dpin0210, dpin0497, dpin0516,
  #    dpin0126, dpin0058, dpin0127, dpin0108, dpin0499, dpin0507, dpin0515,
  #    dpin0057, dpin0496, dpin0130, dpin0128, dpin0056, dpin0129, dpin0131) +
  # ti(dpin0033, dpin0032, dpin0019, dpin0034, dpin0018, dpin0012, dpin0111,
  #    dpin0074, dpin0020, dpin0112, dpin0026, dpin0027, dpin0110, dpin0155,
  #    dpin0013, dpin0154, dpin0025, dpin0115, dpin0156, dpin0021) +
  # ti(dpin0073, dpin0230, dpin0225, dpin0229, dpin0224, dpin0226, dpin0118,
  #    dpin0227, dpin0119, dpin0223, dpin0500, dpin0501, dpin0502, dpin0117,
  #    dpin0228, dpin0072, dpin0116, dpin0102, dpin0520, dpin0503, dpin0521,
  #    dpin0103, dpin0216, dpin0217, dpin0095, dpin0529, dpin0120, dpin0215,
  #    dpin0170, dpin0522, dpin0064, dpin0519, dpin0530, dpin0063, dpin0096,
  #    dpin0065, dpin0101, dpin0528, dpin0218, dpin0169, dpin0214, dpin0094,
  #    dpin0161) +
  # ti(dpin0017, dpin0023, dpin0014, dpin0028, dpin0024, dpin0022) +
  # ti(dpin0035, dpin0031, dpin0113, dpin0075, dpin0078) +
  # ti(dpin0137, dpin0061) +
  # ti(dpin0052, dpin0135, dpin0043, dpin0109, dpin0104, dpin0040, dpin0039) +
  # ti(dpin0144, dpin0122, dpin0160, dpin0505, dpin0098, dpin0099, dpin0525,
  #    dpin0092, dpin0159, dpin0518, dpin0091)


form <- sort(dpins_vip) %>%
  paste(collapse = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>% formula

# form <- paste0("d", pin212_name) %>%
#   paste(collapse = " + ") %>%
#   paste("bodyweight", ., sep = " ~ ") %>% formula()

cmp <- 10
pls_spct <- pls::mvr(form, data = train_data, ncomp = cmp,
                     method = "oscorespls",
                     scale = T, center = T)

train_data %<>%
  dplyr::select(-dplyr::starts_with("Comp")) %>%
  tibble::add_column(
    predict(pls_spct, newdata = train_data, type = "scores") %>%
      tibble::as_tibble() %>%
      setNames(
        nm = paste0("Comp", stringr::str_pad(1:cmp, width = 2, pad = "0"))
      )
  )
test_data %<>%
  dplyr::select(-dplyr::starts_with("Comp")) %>%
  tibble::add_column(
    predict(pls_spct, newdata = test_data, type = "scores") %>%
      tibble::as_tibble() %>%
      setNames(
        nm = paste0("Comp", stringr::str_pad(1:cmp, width = 2, pad = "0"))
      )
  )

train_data %>%
  # dplyr::select(dim, milk_yield, bodyweight,
  #               Comp01, Comp02, Comp03, Comp04, Comp05, Comp06) %>%
  dplyr::select(dim, milk_yield, bodyweight, parity,
                dplyr::starts_with("Comp")) %>%
  cor() %>%
  corrplot::corrplot(method = "square", order = "hclust")

with(train_data,{
  plot(milk_yield ~ dim)
})

form_gam_vip <- names(train_data) %>%
  grep(pattern = "^Comp", value = T) %>%
  paste(collapse = " + ") %>%
  # paste0("s(", ., ")") %>%
  paste("te(dim, milk_yield)", ., sep = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>% formula

form_gam_vip <- bodyweight ~  te(dim, milk_yield, bs = "cr")
  # s(Comp01, Comp02, Comp03, Comp04, Comp05, Comp06)

form_gam_vip <- bodyweight ~ s(dim, bs = "cr") + s(milk_yield, bs = "cr") +
  te(dim, milk_yield, bs = "cr") +
  s(Comp01, Comp02, Comp03, Comp04, Comp05, Comp06)
  # s(milk_yield) + parity +
  # te(dim, milk_yield) + parity:milk_yield +
  # s(Comp01, Comp02, Comp03, Comp04, Comp05, Comp06)

#   Comp01 +  Comp02 +  Comp03 +  Comp04 +  Comp05 +  Comp06 +
#   Comp01:milk_yield + Comp05:milk_yield +
#   Comp03:dim + Comp04:dim + Comp05:dim + Comp06:dim
#
#
#   s(Comp01) + te(dim, Comp01) + te(milk_yield, Comp01)
#
# + s(Comp02) +
#   s(Comp03) + s(Comp04) + s(Comp05) + s(Comp06) + s(Comp07) +
#   s(Comp08) + s(Comp09) + s(Comp10)
  # te(Comp03, dim) + Comp03:parity + te(Comp06, dim) + te(Comp08, dim)
  # s(Comp01) + s(Comp02) + s(Comp03) + s(Comp04) + s(Comp05) + s(Comp06) +
  # ti(dim, Comp06) + ti(dim, Comp03) + ti(milk_yield, Comp01)



tictoc::tic()
gam_vip2 <- mgcv::gam(
  form_gam_vip,
  family = Gamma(link = "log"),
  # family = Gamma,
  data = train_data,
  method = "REML"
)
tictoc::toc()


mgcv::gam.check(gam_vip2)

plot(train_data$bodyweight ~ fitted(gam_vip2))

# rmse(gam_pls, dat = train_data)
# rmse(gam_pls, dat = test_data_swiss)
# rmse(gam_vip, dat = train_data)
# rmse(gam_vip, dat = test_data_swiss)
rmse(gam_vip2, dat = train_data)
rmse(gam_vip2, dat = test_data)

save(gam_pls, file = here::here("data", "gam_pls.rda"))
save(gam_vip, file = here::here("data", "gam_vip.rda"))
save(gam_vip2, file = here::here("data", "gam_vip2.rda"))
save(pls_spct, file = here::here("data", "pls_spct.rda"))
