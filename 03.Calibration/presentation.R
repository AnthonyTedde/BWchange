library(bodyweight.data)
library(magrittr)

# Data ####

data("out_of_sample")
data("train_data")
data("test_data")
data("out_of_sample_cleaned")
data("BW_seenorest")

# MOdels ####

pls_lm_mod_butch <- readRDS(file = here::here("models", "pls_lm_mod_butch.rds"))
pls_glm_gamma_mod_butch <- readRDS(file = here::here("models", "pls_glm_gamma_mod_butch.rds"))

BW_dp <- BW_seenorest %>%
  dplyr::filter(dim < 365) %>%
  dplyr::count(parity, expl_id, an_id, parity, name = "#BW")

MIR_dp <- out_of_sample %>%
  dplyr::filter(dim < 365) %>%
  dplyr::count(parity, expl_id, an_id, parity, name = "#MIR")

merged_dp <- dplyr::inner_join(BW_dp, MIR_dp)

keep_dp <- merged_dp %>%
  dplyr::arrange(parity, dplyr::desc(`#MIR`)) %>%
  dplyr::filter(parity < 4) %>%
  dplyr::group_by(parity) %>%
  dplyr::slice_head(n = 4)

################################################################################
# Plot
################################################################################
full_MIR_dt <- out_of_sample %>%
  dplyr::inner_join(
    keep_dp, by = c("expl_id", "an_id", "parity")
  ) %>%
  dplyr::filter(dim < 365)

full_BW_dt <- BW_seenorest %>%
  dplyr::inner_join(
    keep_dp, by = c("expl_id", "an_id", "parity")
  ) %>%
  dplyr::filter(dim < 365)

##
# Plot
##

full_BW_dt %>%
ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(parity ~ an_id)

## Remove bad datapoints

full_BW_noolr <- dt <- full_BW_dt %>%
  dplyr::arrange(an_id, expl_id, parity, dim) %>%
  dplyr::group_by(an_id, expl_id, parity) %>%
  dplyr::mutate(
    lag = dplyr::lag(bodyweight),
    lead = dplyr::lead(bodyweight)
  ) %>%
  tidyr::fill(lag, lead, .direction = "downup") %>%
  dplyr::mutate(
    bwchange = (abs(lag - bodyweight) + abs(lead - bodyweight)) + 2
  ) %>%
  dplyr::filter(bwchange < 50)


full_BW_noolr %>%
ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(parity ~ an_id)

#######3
# Average plot
####
full_BW_noolr %>%
ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "gam") +
  ggplot2::facet_wrap(parity ~ an_id)




an <- "5502489963"
ex <- "55331006"


test_p1 <- BW_seenorest %>%
  dplyr::filter(parity == 1) %>%
  dplyr::filter(dim < 365) %>%
  dplyr::filter(expl_id == ex, an_id == an) %>%
  dplyr::ungroup()


test_p1_mir <- out_of_sample %>%
  dplyr::filter(parity == 1) %>%
  dplyr::filter(dim < 365) %>%
  dplyr::mutate(parity_fct = ifelse(parity < 3, parity, 3) %>%
                  factor(levels = 1:3,
                         labels = c("first", "second", "third+"))) %>%
  dplyr::filter(expl_id == ex, an_id == an)

pred_bw_lm <- test_p1_mir %>%
  dplyr::select(parity, dim) %>%
  tibble::add_column(bodyweight = predict(pls_glm_gamma_mod_butch,
                                          new_data = test_p1_mir) %>%
                       dplyr::pull())


test_p1 %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight)) +
  ggplot2::geom_point() +
  ggplot2::geom_point(data = pred_bw_lm, color = "blue")



###############################################################################
# Table of bodyweigh change
###############################################################################


out_of_sample

BW_seenorest

spectra_pls_formula <- grep("^dpin", names(train_data), value = T) %>%
  paste(collapse = " + ") %>%
  paste("milk_yield", sep = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>%
  formula()
spectra_pls_meta_formula <- grep("^dpin", names(train_data), value = T) %>%
  paste(collapse = " + ") %>%
  paste("milk_yield", sep = " + ") %>%
  paste("metabw", ., sep = " ~ ") %>%
  formula()

train_data %<>%
  dplyr::mutate(
    metabw = 0.75 * bodyweight
  )
mod <- pls::mvr(spectra_pls_formula, ncomp = 15, data = train_data)
meta_mod <- pls::mvr(spectra_pls_meta_formula, ncomp = 15, data = train_data)
# mod <- pls_lm_mod_butch
# outof <- out_of_sample_cleaned
# outof <- out_of_sample
# mod <- pls_lm_mod2_butch
# mod <- pls_glm_gamma_mod_butch

# bodyweight_pred <- predict(mod, outof)[,,15] %>% dplyr::pull()
bodyweight_pred <- predict(mod, outof)[,,15]
bodyweight_meta_pred <- predict(meta_mod, outof)[,,15]
# bodyweight_pred2 <- predict(pls_lm_mod_butch, new_data = outof) %>% dplyr::pull()
# cor(bodyweight_pred, bodyweight_pred2)



outof %<>%
  dplyr::mutate(bodyweight_pred = bodyweight_pred) %>%
  dplyr::mutate(metabw_pred = bodyweight_meta_pred)


bwchangedf <- outof %>%
  dplyr::mutate(
    lactation_period = dplyr::case_when(
      dim <= 75 ~ "early",
      dim > 75 & dim <= 150 ~ "mid",
      dim > 150 ~ "late"
    ),
    metabw = 0.75 * bodyweight
  ) %>%
  dplyr::select(an_id, parity, dim, bodyweight, bodyweight_pred, metabw,
                metabw_pred, lactation_period) %>%
  dplyr::arrange(an_id, parity, dim) %>%
  dplyr::group_by(an_id, parity) %>%
  dplyr::mutate(
    bodyweight_lag = dplyr::lag(bodyweight),
    bodyweight_pred_lag = dplyr::lag(bodyweight_pred),
    metabw_lag = dplyr::lag(metabw),
    metabw_pred_lag = dplyr::lag(metabw_pred),
    dim_lag = dplyr::lag(dim)
  ) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    # bwchange = (bodyweight - bodyweight_lag) / bodyweight_lag,
    # bwchange_pred = (bodyweight_pred - bodyweight_pred_lag) / bodyweight_pred_lag,
    bwchange = (bodyweight - bodyweight_lag),
    bwchange_pred = (bodyweight_pred - bodyweight_pred_lag),
    bwchange_meta = (metabw - metabw_lag),
    bwchange_meta_pred = (metabw_pred -metabw_pred_lag),
    dimdiff = dim - dim_lag
  ) %>%
  dplyr::select(an_id, lactation_period, parity, dimdiff, bwchange_meta,
                bwchange_meta_pred, bwchange, bwchange_pred)

bwchangedf %>%
  dplyr::group_by(lactation_period) %>%
  dplyr::summarise(
    correlation = cor(bwchange, bwchange_pred)
  )

bwchangedf %>%
  dplyr::group_by(lactation_period) %>%
  dplyr::summarise(
    correlation = cor(bwchange_meta, bwchange_meta_pred)
  )

ggplot2::ggplot(data = bwchangedf, ggplot2::aes(x = bwchange,
                                                y = bwchange_pred,
                                                color = dimdiff)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(slope = 1, intercept = 0) +
  ggplot2::facet_grid(lactation_period ~ .)

ggplot2::ggplot(data = bwchangedf, ggplot2::aes(x = bwchange_meta,
                                                y = bwchange_meta_pred,
                                                color = dimdiff)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(slope = 1, intercept = 0) +
  ggplot2::facet_grid(lactation_period ~ .)


################################################################################
## SWISS data

test_data

BW_seenorest


bodyweight_pred <- predict(mod, test_data)[,,15]
bodyweight_meta_pred <- predict(meta_mod, test_data)[,,15]
bodyweight_pred <- predict(mod, test_data) %>% dplyr::pull()

test_data %<>%
  dplyr::mutate(bodyweight_pred = bodyweight_pred) %>%
  dplyr::mutate(metabw_pred = bodyweight_meta_pred)

bwchangedf <- test_data %>%
  dplyr::mutate(
    lactation_period = dplyr::case_when(
      dim <= 75 ~ "early",
      dim > 75 & dim <= 150 ~ "mid",
      dim > 150 ~ "late"
    ),
    metabw = 0.75 * bodyweight
  ) %>%
  dplyr::select(an_id, parity, dim, bodyweight, bodyweight_pred, metabw,
                metabw_pred, lactation_period) %>%
  dplyr::arrange(an_id, parity, dim) %>%
  dplyr::group_by(an_id, parity) %>%
  dplyr::mutate(
    bodyweight_lag = dplyr::lag(bodyweight),
    bodyweight_pred_lag = dplyr::lag(bodyweight_pred),
    metabw_lag = dplyr::lag(metabw),
    metabw_pred_lag = dplyr::lag(metabw_pred),
    dim_lag = dplyr::lag(dim)
  ) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    # bwchange = (bodyweight - bodyweight_lag) / bodyweight_lag,
    # bwchange_pred = (bodyweight_pred - bodyweight_pred_lag) / bodyweight_pred_lag,
    bwchange = (bodyweight - bodyweight_lag),
    bwchange_pred = (bodyweight_pred - bodyweight_pred_lag),
    bwchange_meta = (metabw - metabw_lag),
    bwchange_meta_pred = (metabw_pred -metabw_pred_lag),
    dimdiff = dim - dim_lag
  ) %>%
  dplyr::select(an_id, lactation_period, parity, dimdiff, bwchange_meta,
                bwchange_meta_pred, bwchange, bwchange_pred)

bwchangedf %>%
  dplyr::group_by(lactation_period) %>%
  dplyr::summarise(
    correlation = cor(bwchange, bwchange_pred)
  )

bwchangedf %>%
  dplyr::group_by(lactation_period) %>%
  dplyr::summarise(
    correlation = cor(bwchange_meta, bwchange_meta_pred)
  )


ggplot2::ggplot(data = bwchangedf, ggplot2::aes(x = bwchange,
                                                y = bwchange_pred,
                                                color = dimdiff)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(slope = 1, intercept = 0) +
  ggplot2::facet_grid(lactation_period ~ .)

ggplot2::ggplot(data = bwchangedf, ggplot2::aes(x = bwchange_meta,
                                                y = bwchange_meta_pred,
                                                color = dimdiff)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(slope = 1, intercept = 0) +
  ggplot2::facet_grid(lactation_period ~ .)

###############
# Restrict dimdiff < 50

bwchangedf50 <- bwchangedf %>%
  dplyr::filter(abs(bwchange) > 10)

bwchangedf50 %>%
  dplyr::group_by(lactation_period) %>%
  dplyr::summarise(
    correlation = cor(bwchange, bwchange_pred)
  )

ggplot2::ggplot(data = bwchangedf50, ggplot2::aes(x = bwchange,
                                                y = bwchange_pred,
                                                color = dimdiff)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(slope = 1, intercept = 0) +
  ggplot2::facet_grid(lactation_period ~.)






###############3
# Group by lactation period also



# bodyweight_pred <- predict(mod, test_data)[,,15]
bodyweight_pred <- predict(mod, test_data) %>% dplyr::pull()

test_data %<>%
  dplyr::mutate(bodyweight_pred = bodyweight_pred)

bwchangedf <- test_data %>%
  dplyr::mutate(
    lactation_period = dplyr::case_when(
      dim <= 75 ~ "early",
      dim > 75 & dim <= 150 ~ "mid",
      dim > 150 ~ "late"
    )
  ) %>%
  dplyr::select(an_id, parity, dim, bodyweight, bodyweight_pred,
                lactation_period) %>%
  dplyr::arrange(an_id, parity, dim) %>%
  dplyr::group_by(an_id, parity, lactation_period) %>%
  dplyr::mutate(
    bodyweight_lag = dplyr::lag(bodyweight),
    bodyweight_pred_lag = dplyr::lag(bodyweight_pred),
    dim_lag = dplyr::lag(dim)
  ) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    # bwchange = (bodyweight - bodyweight_lag) / bodyweight_lag,
    # bwchange_pred = (bodyweight_pred - bodyweight_pred_lag) / bodyweight_pred_lag,
    bwchange = (bodyweight - bodyweight_lag),
    bwchange_pred = (bodyweight_pred - bodyweight_pred_lag),
    dimdiff = dim - dim_lag
  ) %>%
  dplyr::select(an_id, lactation_period, parity, dimdiff,
                bwchange, bwchange_pred)

bwchangedf %>%
  dplyr::group_by(lactation_period) %>%
  dplyr::summarise(
    correlation = cor(bwchange, bwchange_pred)
  )


ggplot2::ggplot(data = bwchangedf, ggplot2::aes(x = bwchange,
                                                y = bwchange_pred,
                                                color = dimdiff)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(slope = 1, intercept = 0) +
  ggplot2::geom_smooth(method = "lm", se = F) +
  ggplot2::facet_grid(lactation_period ~ .)

