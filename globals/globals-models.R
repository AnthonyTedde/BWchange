library(magrittr)

source("globals/version.R")

if(dev){
  nproc <- 5
}else{
  #CECI
  nproc <- 10
}

ntrees <- 500L
ncores <- 5L
max_pls <- max_mtry <- 15L
init_bayes <- 15L
tune_iter <- 50L
last_noimproved <- 10L
verbose_opt <- T

pinstep <- (5010.15 - 925.66) / (1060 - 1)

pin212_boundary <- list(
  c(min = 968.1, max = 1577.5),
  c(min = 1731.8, max = 1762.6),
  c(min = 1781.9, max = 1808.9),
  c(min = 2831.0, max = 2966.0)
)

pinall <- seq(925.66, 5010.15, by = pinstep) %>%
  round(digits = 1) %>%
  setNames(nm = paste0("pin", stringr::str_pad(1:1060, 4, pad = "0")))

pin212_wave <- pin212_boundary %>%
  purrr::map(~pinall >= .x["min"] & pinall <= .x["max"]) %>%
  purrr::reduce(`|`) %>%
  `[`(pinall, .)

pin212_name <- pin212_wave %>% names
pin277_name <- paste0("pin", stringr::str_pad(c(2:175, 199:230, 470:540),
                                              width = 4, pad = 0))

