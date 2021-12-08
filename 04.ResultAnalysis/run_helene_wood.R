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

out_of_sample %<>%
  dplyr::mutate(
    log_dim = log(dim, exp(1)),
    log_bw = log(bodyweight, exp(1))
  )

BW_seenorest %<>%
  dplyr::mutate(
    log_dim = log(dim, exp(1)),
    log_bw = log(bodyweight, exp(1))
  )

BW_dp <- BW_seenorest %>%
  dplyr::filter(dim < 365) %>%
  dplyr::count(parity, expl_id, an_id, parity, name = "#BW")

MIR_dp <- out_of_sample %>%
  dplyr::filter(dim < 365) %>%
  dplyr::count(parity, expl_id, an_id, parity, name = "#MIR")

merged_dp <- dplyr::inner_join(BW_dp, MIR_dp) %>%
  dplyr::ungroup()

###########################
## Print for presentation
keep_dp <- merged_dp %>%
  dplyr::arrange(dplyr::desc(`#MIR`)) %>%
  dplyr::filter(parity < 4) %>%
  dplyr::group_by(parity) %>%
  dplyr::slice_head(n = 4)

keep_dp$`#MIR` %>% sum

## Create dataset
seenorest_mir <- out_of_sample %>%
  dplyr::filter(dim < 365) %>%
  dplyr::inner_join(keep_dp %>% dplyr::select(expl_id, an_id, parity),
                    by = c("expl_id", "an_id", "parity"))

seenorest_bw <- BW_seenorest %>%
  dplyr::filter(dim < 365) %>%
  dplyr::inner_join(keep_dp %>% dplyr::select(expl_id, an_id, parity),
                    by = c("expl_id", "an_id", "parity"))

## Create the models
# For bwpred





seenorest_prd_mdl <- seenorest_mir %>%
  dplyr::mutate(
    bodyweight = predict(pls_lm_mod_butch, seenorest_mir) %>% dplyr::pull(),
    log_bw = log(bodyweight, exp(1)),
    log_dim = log(dim, exp(1)),
  ) %>%
  dplyr::group_by(an_id, expl_id, parity) %>%
  dplyr::group_map(.f = function(dat, key){


    ##################################################
    # Estimate k
    max_wil <- 40
    sortiewiltest3 <- array(0, dim = c(max_wil, 8))
    coefwil=0.1
    for (j in 1:max_wil){
      coefwil<-coefwil-0.005
      dat %<>%
        dplyr::mutate(
          wil = exp(as.integer(dim) * coefwil)
        )
      reg <- lm(bodyweight~ dim + wil, data=dat)
      summaryreg <- summary(reg)
      nobs=nrow(dat)
      RMSE=sqrt(mean(summaryreg$residuals^2))

      final<-data.frame(coefwil,
                        nobs,
                        summaryreg$r.squared,
                        summaryreg$adj.r.squared,
                        RMSE,
                        reg$coefficients[1],
                        reg$coefficients[2],
                        reg$coefficients[3])
      sortiewiltest3[j,]<-as.matrix(final)
    }
    # diffR<-array(0,dim=c(max_wil))
    # for (j in 2:max_wil){
    #   diffR[j]=sortiewiltest3[j,3]-sortiewiltest3[j-1,3]
    # }
    # j=2
    # while(diffR[j]>0.001 & j < max_wil){
    #   j=j+1
    # }
    j <- which.max(sortiewiltest3[, 3])
    corfinal <- sortiewiltest3[j, 3]
    coefwil2<-sortiewiltest3[j]
    coeffinal<-coefwil2[1]

    dat %<>%
      dplyr::mutate(
        wil = exp(dim*coeffinal)
      )
    reg <- lm(bodyweight ~ dim + wil, data=dat)
    # print(sortiewiltest3)
    ##################################################
    key %>%
      dplyr::mutate(
        mdl_prd = tibble::lst(lm = reg),
        k = coeffinal,
        r2 = corfinal,
        n = nrow(dat),
        sortiewil = tibble::lst(s = sortiewiltest3),
        dat_pred = tibble::lst(dat),
        max_dim_pred = max(dat$dim),
        min_dim_pred = min(dat$dim)
      )
  }, .keep = T) %>%
  purrr::reduce(dplyr::bind_rows)



# print some points
# an_id == ai & expl_id == ei & parity == 1
# ai <- "5502419996"
# # ai <- "5502256647"
# ei <- "55331006"
# pt <- 1
ai <- "5502345414"
ei <- "55331006"
pt <- 2

k <- seenorest_prd_mdl %>%
  dplyr::filter(an_id == ai & parity == pt) %>%
  dplyr::pull(k)

data_pred <- tibble::tibble(dim = 1:365, wil = exp(dim*k))
dat_prt <- seenorest_prd_mdl %>%
  dplyr::filter(an_id == ai & expl_id == ei & parity == pt)

dat_lm_prt <- data_pred %>%
  dplyr::mutate(
    bodyweight = predict(dat_prt$mdl_prd$lm, newdata = data_pred)
  )

dat_prt$dat_pred$dat %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight)) +
  ggplot2::geom_point() +
  ggplot2::geom_point(data = dat_lm_prt, color = "darkred")

dat_prt$sortiewil$s


# For bwobs
seenorest_obs_mdl <- seenorest_bw %>%
  dplyr::mutate(
    log_bw = log(bodyweight, exp(1)),
    log_dim = log(dim, exp(1)),
  ) %>%
  dplyr::group_by(an_id, expl_id, parity) %>%
  dplyr::group_map(.f = function(dat, key){

    ##################################################
    # Estimate k
    max_wil <- 40
    sortiewiltest3 <- array(0, dim = c(max_wil, 8))
    coefwil=0.1
    for (j in 1:max_wil){
      coefwil<-coefwil-0.005
      dat %<>%
        dplyr::mutate(
          wil = exp(as.integer(dim) * coefwil)
        )
      reg <- lm(bodyweight~ dim + wil, data=dat)
      summaryreg <- summary(reg)
      nobs=nrow(dat)
      RMSE=sqrt(mean(summaryreg$residuals^2))

      final<-data.frame(coefwil,
                        nobs,
                        summaryreg$r.squared,
                        summaryreg$adj.r.squared,
                        RMSE,
                        reg$coefficients[1],
                        reg$coefficients[2],
                        reg$coefficients[3])
      sortiewiltest3[j,]<-as.matrix(final)
    }
    # diffR<-array(0,dim=c(max_wil))
    # for (j in 2:max_wil){
    #   diffR[j]=sortiewiltest3[j,3]-sortiewiltest3[j-1,3]
    # }
    # j=2
    # while(diffR[j]>0.001 & j < max_wil){
    #   j=j+1
    # }
    j <- which.max(sortiewiltest3[, 3])
    corfinal <- sortiewiltest3[j, 3]
    coefwil2<-sortiewiltest3[j]
    coeffinal<-coefwil2[1]

    dat %<>%
      dplyr::mutate(
        wil = exp(dim*coeffinal)
      )
    reg <- lm(bodyweight ~ dim + wil, data=dat)
    # print(sortiewiltest3)
    ##################################################


    key %>%
      dplyr::mutate(
        mdl_obs = tibble::lst(lm = reg),
        k = coeffinal,
        r2 = corfinal,
        sortiewil = tibble::lst(s = sortiewiltest3),
        n = nrow(dat),
        dat_obs = tibble::lst(dat),
        max_dim_obs = max(dat$dim),
        min_dim_obs = min(dat$dim)
      )
  }, .keep = T) %>%
  purrr::reduce(dplyr::bind_rows)

# print some points
# an_id == "5502092446" & expl_id == ei & parity == 3

k <- seenorest_obs_mdl %>%
  dplyr::filter(an_id == ai & parity == pt) %>%
  dplyr::pull(k)
data_pred <- tibble::tibble(
  dim = 1:365,
  wil = exp(dim*k)
)
dat_prt <- seenorest_obs_mdl %>%
  dplyr::filter(an_id == ai & expl_id == ei & parity == pt)
dat_lm_prt <- data_pred %>%
  dplyr::mutate(
    bodyweight = predict(dat_prt$mdl_obs$lm, newdata = data_pred)
  )

dat_prt$dat_obs$dat %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight)) +
  ggplot2::geom_point() +
  ggplot2::geom_point(data = dat_lm_prt, color = "darkred")

dat_prt$sortiewil


##
# Merge

seenorest_all <- dplyr::inner_join(
  x = seenorest_obs_mdl,
  y = seenorest_prd_mdl,
  by = c("an_id", "expl_id", "parity")
)
# %>%
#   dplyr::mutate(
#     max_dim = min(max_dim_obs, max_dim_pred),
#     min_dim = max(min_dim_obs, min_dim_pred)
#   ) %>%
#   dplyr::select(an_id, expl_id, parity, max_dim, min_dim,
#                 mdl_obs, mdl_prd, dat_obs, dat_pred)



prt_mdl <- seenorest_all %>%
  dplyr::filter(an_id == ai & expl_id == ei & parity == pt) %>%
  dplyr::mutate(
    max_dim = min(max_dim_obs, max_dim_pred),
    min_dim = max(min_dim_obs, min_dim_pred)
  )

dat_obs_mdl <- tibble::tibble(
  dim = prt_mdl$min_dim:prt_mdl$max_dim,
  wil = exp(dim* prt_mdl$k.x),
  mdl = "obs"
)
dat_prd_mdl <- tibble::tibble(
  dim = prt_mdl$min_dim:prt_mdl$max_dim,
  wil = exp(dim* prt_mdl$k.y),
  mdl = "prd"
)


dat_obs_mdl <- dat_obs_mdl %>%
  dplyr::mutate(
    bodyweight = predict(prt_mdl$mdl_obs$lm, newdata = dat_obs_mdl),
    mdl = "obs"
  )
dat_pred_mdl <- dat_prd_mdl %>%
  dplyr::mutate(
    bodyweight = predict(prt_mdl$mdl_prd$lm, newdata = dat_prd_mdl),
    mdl = "prd"
  )

dplyr::bind_rows(dat_obs_mdl, dat_pred_mdl) %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight, color = mdl)) +
  ggplot2::geom_point() +
  ggplot2::geom_point(data = prt_mdl$dat_obs$dat, color = "darkred") +
  ggplot2::geom_point(data = prt_mdl$dat_pred$dat, color = "blue")

# Graph BW change
dplyr::bind_rows(dat_obs_mdl, dat_pred_mdl) %>%
  dplyr::group_by(mdl) %>%
  dplyr::arrange(mdl, dim) %>%
  dplyr::mutate(
    bodyweight_lag = dplyr::lag(bodyweight)
  ) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    bwchange = bodyweight - bodyweight_lag
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bwchange, color = mdl)) +
  ggplot2::geom_point()

  # ggplot2::geom_point(data = prt_mdl$dat_obs$dat, color = "darkred") +
  # ggplot2::geom_point(data = prt_mdl$dat_pred$dat, color = "blue")


dplyr::bind_rows(dat_obs_mdl, dat_pred_mdl) %>%
  dplyr::group_by(mdl) %>%
  dplyr::arrange(mdl, dim) %>%
  dplyr::mutate(
    bodyweight_lag = dplyr::lag(bodyweight)
  ) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    bwchange = bodyweight - bodyweight_lag
  ) %>%
  dplyr::select(dim, bwchange, mdl) %>%
  tidyr::pivot_wider(names_from = mdl, values_from = bwchange) %>%
  with(cor(obs, prd))


dplyr::bind_rows(dat_obs_mdl, dat_pred_mdl) %>%
  dplyr::group_by(mdl) %>%
  dplyr::arrange(mdl, dim) %>%
  dplyr::mutate(
    bodyweight_lag = dplyr::lag(bodyweight)
  ) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    bwchange = bodyweight - bodyweight_lag
  ) %>%
  dplyr::select(dim, bwchange, mdl) %>%
  tidyr::pivot_wider(names_from = mdl, values_from = bwchange) %>%
  ggplot2::ggplot(ggplot2::aes(x = obs, y = prd)) +
  ggplot2::geom_point()







