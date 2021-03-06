library(splines)
library(ggplot2)
library(magrittr)
library(pls)
library(foreach)
library(magrittr)

data("training_data")

training <- training_data

# ------------------------------------------------------------------------------
# Formula ####
# ------------------------------------------------------------------------------
pls_mir_var <- grep("^dpin", names(training), value = T) %>%
  paste(collapse = " + ")
left_hand <- paste("ns(milk_yield, df = 4)",
                   "ns(dim, df = 4)",
                   "parity_ord", pls_mir_var,
                   sep = " + ")
form <- paste("bodyweight", left_hand, sep = " ~ ") %>% formula()

# Preamble data
N <- 10000
n <- nrow(training)

doMC::registerDoMC(cores = 10)
RNGkind(kind = "L'Ecuyer-CMRG")


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
    }) %>% unlist(use.names = F)
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
                    type = "response") %>% drop()
    p_c <- fitted(pls_mod) %>% drop()
    PRESS_v <- colSums((p_v - vl[, "bodyweight", drop = T])^2)
    PRESS_c <- colSums((p_c - tr[, "bodyweight", drop = T])^2)
    # RMSE_v <- sqrt(PRESS_v / length(vsset))
    # RMSE_c <- sqrt(PRESS_c / length(csset))
    press_names <- paste0("PRESS", stringr::str_pad(1:ncomp, width = 3, pad = 0))
    names(PRESS_c) <- names(PRESS_v) <- press_names
    rbind(
      data.frame(type = "calibration", prop = i$prop, round = i$round,
                 N = N, n_cal = length(csset), n_val = length(vsset),
                 rbind(PRESS_c), row.names = NULL),
      data.frame(type = "validation", prop = i$prop, round = i$round,
                 N = N, n_cal = length(csset), n_val = length(vsset),
                 rbind(PRESS_v), row.names = NULL)
    )
  }

save(MCCV_perf, file = "data/MCCV_perf.rda", compress = "xz")

