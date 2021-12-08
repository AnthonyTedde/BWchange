library(magrittr)
rm(list = ls())
data("test_data")
data("train_data")
# data("train_data_noseenorest")
data("train_data_noseenorest")
source("globals/globals-models.R")
library(splines)

train_data_seenorest <- train_data %>%
  dplyr::filter(provider == "seenorest")
train_data_noseenorest %>% dim
train_data_noseenorest$wol_ord %>% hist
train_data_noseenorest$dim %>% hist

len <- 20
set.seed(1010)
dim(train_data_noseenorest)

# train_data_noseenorest %<>%
#   dplyr::filter(provider != "utrobe")

#-------------------------------------------------------------------------------
# lm with spline
#-------------------------------------------------------------------------------
# mod <- lm(bodyweight ~ ns(dim, df = 3), data = train_data_noseenorest)
# par(mfrow = c(2, 2))
# plot(mod)


# ------------------------------------------------------------------------------
# pls
# ------------------------------------------------------------------------------

# train_data_noseenorest %<>%
#   dplyr::mutate(
#     bodyweight = bodyweight - bodyweight %% 10
#   )

# train_data_noseenorest <- train_data %>%
#   dplyr::filter(provider != "utrobe")

# V%<>%
#   dplyr::filter(!(provider %in% c("seenorest", "utrobe")))

form <- paste0("d", pin212_name) %>%
  paste(collapse = " + ") %>%
  paste("ns(milk_yield, df=9)", "ns(dim, df=9)", "parity",
        "parity:milk_yield", "dim:milk_yield",
        ., sep = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>% formula()

pls_test <- pls::mvr(form, ncomp = 15,
                     data = train_data_noseenorest,
                     method = "oscorespls",
                     scale = T, center = T)
# pls_test <- pls::cppls(form, ncomp = 15,
#                      data = train_data_noseenorest,
#                      scale = T, center = T)
# coefficients(pls_test)

ncomp <- (cumsum(pls_test$Xvar /  pls_test$Xtotvar) < .99) %>%
  which %>%
  seq_along()

t_seenorest <- train_data %>% dplyr::filter(provider == "seenorest")
p <- predict(pls_test, t_seenorest,
        ncomp = 15) %>% drop


sqrt((p - t_seenorest$bodyweight)^2)
sqrt(mean((p - t_seenorest$bodyweight)^2))

t_seenorest %<>%
  dplyr::mutate(
    rse = sqrt((p - t_seenorest$bodyweight)^2)
    # rse_q <- quantile(rse)
  )



tibble::tibble(
  prd = predict(pls_test, train_data_noseenorest, type = "response", ncomp = 15) %>%
    drop,

  type = "response", ncomp = 15) %>%
  drop,
obs = train_data_seenorest$bodyweight
) %>%
  # yardstick::rmse(truth = obs, estimate = prd)
  yardstick::rsq(truth = obs, estimate = prd)





# ------------------------------------------------------------------------------
# pls No parity
# ------------------------------------------------------------------------------

# train_data_noseenorest %<>%
#   dplyr::mutate(
#     bodyweight = bodyweight - bodyweight %% 10
#   )

# train_data_noseenorest <- train_data %>%
#   dplyr::filter(provider != "utrobe")

# V%<>%
#   dplyr::filter(!(provider %in% c("seenorest", "utrobe")))

form <- paste0("d", pin212_name) %>%
  paste(collapse = " + ") %>%
  # paste("ns(milk_yield, df=9)", "ns(dim, df=9)", "dim:milk_yield",
  #       ., sep = " + ") %>%
  paste("ns(milk_yield, df=9)", ., sep = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>% formula()

pls_test_noparity <- pls::mvr(form, ncomp = 15,
                     data = train_data_noseenorest,
                     method = "oscorespls",
                     scale = T, center = T)
# coefficients(pls_test)

ncomp <- (cumsum(pls_test_noparity$Xvar /  pls_test_noparity$Xtotvar) < .99) %>%
  which %>%
  seq_along()

tibble::tibble(
  prd = predict(pls_test_noparity, train_data_noseenorest, type = "response", ncomp = 15) %>%
    drop,
  obs = train_data_noseenorest$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = prd)
  # yardstick::rsq(truth = obs, estimate = prd)

tibble::tibble(
  prd = predict(pls_test_noparity, test_data, type = "response", ncomp = 15) %>%
    drop,
  obs = test_data$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = prd)
  # yardstick::rsq(truth = obs, estimate = prd)

tibble::tibble(
  prd = predict(pls_test_noparity, train_data_seenorest,
                type = "response", ncomp = 15) %>%
    drop,
  obs = train_data_seenorest$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = prd)
  # yardstick::rsq(truth = obs, estimate = prd)












# ------------------------------------------------------------------------------
# glmnet: elasticnet + glm
# ------------------------------------------------------------------------------
form <- paste0("d", pin212_name) %>%
  paste(collapse = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>% formula()

pls_spct <- pls::mvr(form, ncomp = 25,
                     data = train_data_noseenorest,
                     scale = T, center = T)
# coefficients(pls_test)

ncomp <- (cumsum(pls_spct$Xvar /  pls_spct$Xtotvar) < .99) %>%
  which %>%
  seq_along()

train_data_noseenorest %<>%
  tibble::add_column(
    predict(pls_spct, newdata = train_data_noseenorest, ncomp = ncomp, type = "scores") %>%
      tibble::as_tibble() %>%
      setNames(nm = paste0("Comp", stringr::str_pad(ncomp, width = 2, pad = "0")))
  )
test_data_swiss %<>%
  tibble::add_column(
    predict(pls_spct, newdata = test_data_swiss, ncomp = ncomp, type = "scores") %>%
      tibble::as_tibble() %>%
      setNames(nm = paste0("Comp", stringr::str_pad(ncomp, width = 2, pad = "0")))
  )

library(splines)
form <- names(train_data_noseenorest) %>%
  grep(pattern = "^Comp", value = T) %>%
  paste(collapse = " + ") %>%
  paste("milk_yield", "dim", "parity",
        "dim:milk_yield", "parity:milk_yield",
        # "dim:Comp01", "dim:Comp02",
        # "milk_yield:Comp01", "milk_yield:Comp02",
        # "ns(dim, df=5)", "ns(milk_yield, df=5)",
        ., sep = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>%
  formula()

form_gam <- names(train_data_noseenorest) %>%
  grep(pattern = "^Comp", value = T) %>%
  paste(collapse = " + ") %>%
  paste("s(milk_yield)", "s(dim)", "parity",
        "te(dim, milk_yield, k=6)", "parity:milk_yield",
        # "dim:Comp01", "dim:Comp02",
        # "milk_yield:Comp01", "milk_yield:Comp02",
        # "ns(dim, df=5)", "ns(milk_yield, df=5)",
        ., sep = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>%
  formula()


glm_test <- glm(form, family = Gamma, data = train_data_noseenorest)
gam_test <- mgcv::gam(form_gam,
                      family = Gamma,
                      data = train_data_noseenorest,
                      method = "REML")

tibble::tibble(
  prd = predict(glm_test, train_data_noseenorest, type = "response", ncomp = 10) %>%
    drop,
  obs = train_data_noseenorest$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = prd)

tibble::tibble(
  prd = predict(glm_test, test_data_swiss, type = "response", ncomp = 10) %>%
    drop,
  obs = test_data_swiss$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = prd)

tibble::tibble(
  prd = predict(gam_test, train_data_noseenorest, type = "response") %>%
    drop,
  obs = train_data_noseenorest$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = prd)

tibble::tibble(
  prd = predict(gam_test, test_data_swiss, type = "response") %>%
    drop,
  obs = test_data_swiss$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = prd)

# ------------------------------------------------------------------------------
# glmnet: elasticnet + glm
# ------------------------------------------------------------------------------

form <- paste0("d", pin212_name) %>%
  paste(collapse = " + ") %>%
  paste("milk_yield", "dim", "parity",
        ., sep = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>% formula()
  # paste0(" ~ ", .) %>% formula()

glm_all <- glmnetUtils::glmnet(form, data = train_data_noseenorest, family = Gamma())

tibble::tibble(
  prd = predict(glm_all, train_data_noseenorest, type = "response", s = .1) %>%
    drop,
  obs = train_data_noseenorest$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = prd)

tibble::tibble(
  prd = predict(glm_all, test_data_swiss, type = "response", s = 5) %>%
    drop,
  obs = test_data_swiss$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = prd)

tibble::tibble(
  prd = predict(glm_all, test_data_seenorest, type = "response", s = 15) %>%
    drop,
  obs = test_data_seenorest$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = prd)







tibble::tibble(
  prd = predict(pls_test, test_data, type = "response", ncomp = 15) %>%
    drop,
  obs = test_data$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = prd)

tibble::tibble(
  prd = predict(pls_test, train_data_noseenorest, type = "response", ncomp = 15) %>%
    drop,
  obs = train_data_noseenorest$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = prd)

tibble::tibble(
  prd = predict(pls_test, test_data, type = "response", ncomp = 10) %>%
    drop,
  obs = test_data$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = prd)

tibble::tibble(
  prd = predict(pls_test, train_data_seenorest, type = "response", ncomp = 10) %>%
    drop,
  obs = train_data_seenorest$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = prd)

save(pls_test, file = "data/pls_test.rda")
save(pls_test_noparity, file = "data/pls_test_noparity.rda")
save(glm_test, file = "data/glm_test.rda")
save(gam_test, file = "data/gam_test.rda")
save(pls_spct, file = "data/pls_spct.rda")

