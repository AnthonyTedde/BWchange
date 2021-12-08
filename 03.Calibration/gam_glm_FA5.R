library(magrittr)

# ------------------------------------------------------------------------------
# Data. Run rm before the following chunck
# ------------------------------------------------------------------------------

rm(list = ls())
data("train_data")
data("train_data_noseenorest")
data("test_data")

# train_data_noseenorest <- train_data
train_data_seenorest <- train_data %>%
  dplyr::filter(provider == "seenorest")

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

ncmp <- 3

formulae_fa <- paste(FAs_rate, collapse = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>% formula()

(mvr_FAs <- pls::mvr(formulae_fa,
                     data = train_data_noseenorest,
                     ncomp = ncmp,
                     method = "oscorespls",
                     scale = T, center = T)) |> system.time()


vip_score <- (chillR::VIP(mvr_FAs) %>%
                colSums() / max(ncmp)) %>%
  sort(decreasing = T)

FAs_vip <- which(vip_score > .9) %>% names

FA_names <- names(vip_score) %>% sub(pattern = "\\_prd", replacement = "") %>%
  sub(pattern = "\\.", replacement = ":") %>%
  sub(pattern = "\\_cis9\\_cis12\\_cis15", replacement = "(9,12,15)") %>%
  sub(pattern = "\\_cis9\\_cis12", replacement = "(9,12)") %>%
  sub(pattern = "\\_cis9", replacement = "(9)") %>%
  sub(pattern = "\\_trans\\_tot", replacement = " (trans) tot") %>%
  sub(pattern = "\\_tot", replacement = " tot") %>%
  sub(pattern = "\\_trans11", replacement = "") %>%
  sub(pattern = "\\_cis", replacement = "(cis)") %>%
  sub(pattern = "\\_trans", replacement = "")


tibble::tibble(
  FA = factor(FA_names, levels = FA_names),
  vip_score = vip_score
) %>%
  ggplot2::ggplot(ggplot2::aes(x = FA, y = vip_score)) +
  ggplot2::geom_col() +
  ggplot2::geom_hline(yintercept = 0.9, color = "darkred")

# Update formulae_fa
formulae_fa <- paste(FAs_vip, collapse = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>% formula()

# ------------------------------------------------------------------------------
# gam1 for prediction
# ------------------------------------------------------------------------------

FAs_4_my_interaction <- stringr::str_detect(
  FAs_vip,
  pattern = "(^C10|^C14|^C16|^C18)\\_"
)

formulae_gam <- glue::glue("s({FAs_vip}, k=100, bs = 'tp')") %>%
  paste(collapse = "+") %>%
  paste("s(milk_yield, k=100, bs = 'tp')" , ., sep = "+") %>%
  # paste("ti(dim, milk_yield)", sep = "+") %>%
  paste("s(fat_rate_2_prd, k=100, bs = 'tp')",
        "s(protein_rate_1_prd, k=100, bs = 'tp')",
        "s(lactose_rate_1_prd, k=100, bs = 'tp')",
         sep = "+") %>%
  paste("bodyweight", ., sep = "~") %>% formula()


system.time(
  gam_FA1 <- mgcv::gam(
    formulae_gam,
    family = Gamma(link = "log"),
    # family = Gamma,
    data = train_data_noseenorest,
    method = "REML"
  )
)

mgcv::gam.check(gam_FA1)

tibble::tibble(
  pred = predict(gam_FA1,
                 newdata = train_data_noseenorest,
                 type = "response") |> drop(),
  obs = train_data_noseenorest$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = pred)
  # yardstick::rsq(truth = obs, estimate = pred)

tibble::tibble(
  pred = predict(gam_FA1,
                 newdata = test_data,
                 type = "response") |> drop(),
  obs = test_data$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = pred)
  # yardstick::rsq(truth = obs, estimate = pred)


tibble::tibble(
  pred = predict(gam_FA1,
                 newdata = train_data_seenorest,
                 type = "response") |> drop(),
  obs = train_data_seenorest$bodyweight
) %>%
  yardstick::rmse(truth = obs, estimate = pred)
  # yardstick::rsq(truth = obs, estimate = pred)

# gam_FA1_noparity <- gam_FA1
# ------------------------------------------------------------------------------
# Save. ####
# ------------------------------------------------------------------------------

# save(train_data, file = "data/train_data.rda")
# save(test_data, file = "data/test_data.rda")
# save(train_data_noseenorest, file = "data/train_data_noseenorest.rda")

save(gam_FA1, file = "data/gam_FA1.rda")
save(gam_FA1_noparity, file = "data/gam_FA1_noparity.rda")
