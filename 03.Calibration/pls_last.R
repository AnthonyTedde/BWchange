library(splines)
library(ggplot2)
library(magrittr)
library(pls)
library(foreach)

data("training_data")
data("testing_swiss_data")
data("stratified_kfolds")

training <- training_data

# ------------------------------------------------------------------------------
# Formula ####
# ------------------------------------------------------------------------------
pls_mir_var <- grep("^dpin", names(training), value = T) |>
  paste(collapse = " + ")
left_hand <- paste("ns(milk_yield, df = 4)",
                   "ns(dim, df = 4)",
                   "parity_ord", pls_mir_var,
                   sep = " + ")
form <- paste("bodyweight", left_hand, sep = " ~ ") |> formula()

# Preamble data
N <- 1000
# ICI
# N <- 5
# prop <- .20
# round(nrow(training) * prop)
n <- nrow(training)

doMC::registerDoMC(cores = 5)
RNGkind(kind = "L'Ecuyer-CMRG")


# MCCV_perf <- forearch::foreach(prop in c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %do%
#   {
  # print(prop)}
options_lst <- expand.grid(round = 1:N,
                           prop = c(.05, .1, .2, .3, .4, .5))
ncomp <- 100

set.seed(1010)
MCCV_perf <- foreach::foreach(i = iterators::iter(options_lst, by = 'row'),
                              .options.multicore = list(set.seed = T),
                              .combine = "rbind") %dopar%
  {
    # -- Stratified sampling -- #
    strata <- training$strata_uid
    idx <- lapply(table(strata), FUN = function(d){
      1:d %in% sample(d, size = round(i$prop*d))
    }) |> unlist(use.names = F)
    csset <- (1:nrow(training))[idx[sort(order(strata))]]
    # csset <<- sample(1:n, size = round(n * i$prop))
    vsset <- setdiff(1:nrow(training), csset)
    tr <- training[csset, ]
    vl <- training[vsset, ]
    # -- Stratified sampling -- #
    pls_mod <- pls::mvr(
      form, data = tr, method = pls::pls.options()$plsralg,
      ncomp = ncomp,
      center = T, scale = T,
      validation = "none"
    )
    p_v <- predict(pls_mod,
                    ncomp = 1:ncomp,
                    newdata = vl,
                    type = "response") |> drop()
    p_c <- fitted(pls_mod) |> drop()
    PRESS_v <- (p_v - vl[, "bodyweight", drop = T])^2
    PRESS_c <- (p_c - tr[, "bodyweight", drop = T])^2
    RMSE_v <- sqrt(colSums(PRESS_v) / length(vsset))
    RMSE_c <- sqrt(colSums(PRESS_c) / length(csset))
    rmse_names <- paste0("RMSE", stringr::str_pad(1:ncomp, width = 3, pad = 0))
    names(RMSE_c) <- names(RMSE_v) <- rmse_names
    rbind(
      data.frame(type = "calibration", prop = i$prop, round = i$round,
                 N = N, n_cal = length(csset), n_val = length(vsset),
                 rbind(RMSE_c), row.names = NULL),
      data.frame(type = "validation", prop = i$prop, round = i$round,
                 N = N, n_cal = length(csset), n_val = length(vsset),
                 rbind(RMSE_v), row.names = NULL)
    )
  }

MCCV_perf

save(MCCV_perf, file = "data/MCCV_perf.rda")

# data("MCCV_perf")
#
# perf_sum <- MCCV_perf %>%
#   dplyr::group_by(type, prop, N) %>%
#   dplyr::summarize(
#     n_cal = mean(n_cal), n_val = mean(n_val),
#     dplyr::across(dplyr::starts_with("PRESS"), sum)
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(
#     n = ifelse(type == "calibration", n_cal, n_val),
#     dplyr::across(
#       dplyr::starts_with("PRESS"),  ~sqrt(.x / n / N),
#       .names = "RMSE_{sub('PRESS', '', .col)}"
#     )
#   )
#
# perf_sum_long <- perf_sum %>%
#   dplyr::select(type, prop, dplyr::starts_with("RMSE")) %>%
#   tidyr::pivot_longer(cols = dplyr::starts_with("RMSE"),
#                       names_to = "components_names",
#                       values_to = "RMSE") %>%
#   dplyr::mutate(
#     components = as.integer(factor(components_names)) # Already order (see str_pad)
#   )
#
#
# validation <- perf_sum_long[perf_sum_long$type == "validation", ]
# bst_validation <- by(validation,
#    factor(validation$prop),
#    FUN = function(tab){
#      m <- which.min(tab$RMSE)
#      tab[m, ]
#    }) |> Reduce(f = rbind)
# # mean(bst_validation$component_num)
#
# perf_sum_long %>%
#   ggplot(aes(x = components,
#              y = RMSE, color = type)) +
#   geom_line() +
#   # scale_y_continuous(limits = c(, 75)) +
#   # coord_cartesian(ylim = c(0, 75)) +
#   facet_wrap(prop~., ncol = 2)
#
#
#
# ncomp_v <- which.min(perf_sum_lst$validation)
# ncomp_c <- which.min(perf_sum_lst$calibration)
# plot(perf_sum_lst$calibration[1:100], type = "l", col = "blue")
# lines(perf_sum_lst$validation[1:100], col = "red")
# abline(v = ncomp_v, lty = 2)
#
# training <- training[training$bodyweight < 630, ]
#
# pls_final <- pls::mvr(
#   form, data = training, method = pls::pls.options()$plsralg,
#   # ncomp = ncomp_v,
#   ncomp = 100,
#   center = T, scale = T,
#   validation = "none"
# )
#
# rmse(pls_final, training, ncomp = ncomp_v, type = "response")
# rmse(pls_final, testing_swiss_data, ncomp = ncomp_v, type = "response")
#
# summary(pls_final)
#
#
# RMSE_pls <- sqrt(pls_full$validation$PRESS / n) |> drop()
# plot(RMSE_pls, type = "l")
#
#
# rmsecv <- sqrt(pls_lst[[1]]$validation$PRESS / (n))
# sqrt(pls_lst[[1]]$validation$PRESS / n)
# # rmsecv <- sqrt(pls_full$validation$PRESS / n)
# which.min(rmsecv)
# # plot(x = 1:222, y = rmsecv)
#
#
