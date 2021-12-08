rm(list = ls())
library(bodyweight.data)
library(magrittr)

## TODO: Add all variables to the initial dataset.
## DONE: Check the computation of the drift correction
## TODO: Unsmooth data from bodyweight.data
## TODO: Rerun all script and explicitely change the dpin ton consider.


# Data ####
# data("out_of_sample_mir")
data("out_of_sample_bw")
data("train_data")
data("test_data")
data("BW_seenorest")
#data("test_data_swiss")
#data("test_data_seenorest")
# data("BW_fr1")
# pls_lm_mod <- readRDS(file = "models/pls_lm_mod.rds")
# pls_linearsvm_mod <- readRDS(file = "models/pls_linearsvm_mod.rds")
# pls_polysvm_mod <- readRDS(file = "models/pls_polysvm_mod.rds")
# cor_linearsvm_mod <- readRDS(file = "models/cor_linearsvm_mod.rds")
# cor_polysvm_mod <- readRDS(file = "models/cor_polysvm_mod.rds")

# TODO WOOD -->
# out_of_sample_mir <- train_data %>%
#   dplyr::filter(provider == "seenovia")
# out_of_sample_bw <- BW_fr1 %>%
#   dplyr::mutate(parity_full = parity) %>%
#   dplyr::mutate(an_id = paste0("FR", an_id))
#------------------

out_of_sample_mir <- test_data
out_of_sample_bw %<>%
  dplyr::mutate(expl_id = "swiss")
#ICI
# out_of_sample_mir <- train_data %>%
#   dplyr::filter(provider == "seenorest")
# out_of_sample_bw <- BW_seenorest %>%
#   dplyr::mutate(
#     parity_full = parity,
#     parity = ifelse(parity_full < 4, parity_full, 3)
#   )

# out_of_sample_mir %<>%
#   dplyr::filter(dim < 305)
# out_of_sample_bw %<>%
#   dplyr::filter(dim < 305)

# out_of_sample_mir %<>%
#   dplyr::filter(dim > 7)

# mdl ####
data("pls_test")
data("pls_spct")
# data("glm_test")
data("pls_lst")
# data("gam_test")
# data("gam_pls")
# data("gam_vip")
data("gam_vip2")
data("gam_FA1")
data("gam_FA1_noparity")
data("gam_FA2")
data("gam_FA3")
data("pls_test_noparity")

# Add comp column ####
ncomp <- (cumsum(pls_spct$Xvar /  pls_spct$Xtotvar) < .99) %>%
  which %>%
  seq_along()
ncomp <- 1:10

train_data %<>%
  tibble::add_column(
    predict(pls_spct, newdata = train_data, ncomp = ncomp, type = "scores") %>%
      tibble::as_tibble() %>%
      setNames(nm = paste0("Comp", stringr::str_pad(ncomp, width = 2, pad = "0")))
  )

out_of_sample_mir %<>%
  tibble::add_column(
    predict(pls_spct, newdata = out_of_sample_mir, ncomp = ncomp, type = "scores") %>%
      tibble::as_tibble() %>%
      setNames(nm = paste0("Comp", stringr::str_pad(ncomp, width = 2, pad = "0")))
  )

# Global variable ####
model <- pls_test
# model <- pls_linearsvm_mod
# model <- cor_linearsvm_mod
# model <- pls_polysvm_mod
# model <- cor_polysvm_mod
# model <- pls_lm_mod
# model <- glm_test
# model <- gam_test
# model <- gam_vip
# model <- gam_pls
# model <- gam_vip2
# model <- pls_lst$HSO_all
model <- gam_FA1
# model <- pls_lst$HSO_vip
# model <- gam_FA2
# model <- gam_FA3
model <- pls_test_noparity
model <- gam_FA1_noparity

# model <- pls_lst$HSO_all
ncomp <- 10
# ncomp <- 6


# Test performance of the model ####
rmse <- function(dat){
  tibble::tibble(
    # ici
    pred = predict(model, newdata = dat, ncomp = ncomp, type = "response"),
    # pred = predict(model, new_data = dat, type = "raw") %>%
    #   drop %>% unlist %>% unname,
    obs = dat$bodyweight
  ) %>%
    yardstick::rmse(truth = obs, estimate = pred)
}

rmse(train_data)
rmse(out_of_sample_mir)
# rmse(test_data_seenorest)

# Create summary statistics datasets ####
out_of_sample_bw_count <- out_of_sample_bw %>%
  #ici
  # dplyr::filter(dim < 365 & parity_full < 4) %>%
  dplyr::filter(dim < 365) %>%
  dplyr::count(parity_full, expl_id, an_id,  name = "nBW") %>%
  dplyr::arrange(desc(nBW))

out_of_sample_mir_count <- out_of_sample_mir %>%
  dplyr::filter(dim < 365 & parity_full == 2) %>%
  # dplyr::filter(dim < 365) %>%
  dplyr::count(parity_full, expl_id, an_id, parity_full, name = "nMIR") %>%
  dplyr::arrange(desc(nMIR)) %>%
  dplyr::filter(nMIR > 6)
out_of_sample_mir_count <- out_of_sample_mir_count[1:6, ]

out_of_sample_merged_count <- dplyr::inner_join(
  out_of_sample_bw_count,
  out_of_sample_mir_count
)


# Function for ................ ####
# dataset <- out_of_sample_mir %>%
#   dplyr::inner_join(out_of_sample_merged_count)
# mdl <- model
library(splines)
augment_dataframe <- function(dataset, mdl){
  if(!missing(mdl)){
    if(any(class(mdl) == "train")){
       dataset %<>%
        dplyr::mutate(bodyweight = predict(mdl, dataset))
    }else{
      dataset %<>%
        #ici
        # dplyr::mutate(
        #   bodyweight = predict(mdl, new_data = dataset, type = "raw") %>%
        #     drop %>% unlist %>% unname
        # )
        dplyr::mutate(bodyweight = predict(mdl, dataset, ncomp = ncomp,
                                           type = "response"))
    }
  }
  dataset %>%
    # Computation of the regression line by animal and parity.
    dplyr::group_by(an_id, expl_id, parity_full) %>%
    dplyr::group_map(.f = function(dat, key){
      dat %<>%
        dplyr::filter(dim > 4)
      # ici
      # reg <- lm(log(bodyweight) ~ dim + log(dim) , data=dat)
      reg <- mgcv::gam(bodyweight ~ s(dim), data=dat,
                       family = Gamma(link = "log"), method = "REML")
      # Output dataframe
      key %>%
        dplyr::mutate(
          mdl_prd = tibble::lst(lm = reg),
          n = nrow(dat),
          dataset = tibble::lst(dat),
          max_dim = max(dat$dim),
          min_dim = min(dat$dim)
        )
    }, .keep = T) %>%
    purrr::reduce(dplyr::bind_rows)
}

# Compute the augmented dataframe ####
out_of_sample_mir_cleaned <- out_of_sample_mir %>%
  # ICI
  # dplyr::filter(dim < 200) %>%
  dplyr::inner_join(out_of_sample_merged_count)
out_of_sample_bw_cleaned <- out_of_sample_bw %>%
  # ICI
  # dplyr::filter(dim < 200) %>%
  dplyr::inner_join(out_of_sample_merged_count) %>%
  tidyr::drop_na() %>%
  dplyr::filter(dim > 0)

augmented_oos_mir <- augment_dataframe(out_of_sample_mir_cleaned, mdl = model)
augmented_oos_bw <- augment_dataframe(out_of_sample_bw_cleaned)


augmented_oos <- augmented_oos_bw %>%
  tibble::add_column(type = "obs") %>%
  dplyr::bind_rows(
    augmented_oos_mir %>%
      tibble::add_column(type = "prd")
  )



# Print some ####





# Smoothed datapoints ####
smooth_data <- function(dt, ai, pt){
  aoos <- dt %>%
    dplyr::group_by(an_id, parity_full) %>%
  # Set min/max dim
    dplyr::mutate(
      max_dim = min(max_dim),
      min_dim = max(min_dim)
    ) %>%
    dplyr::group_by(type, .add = T) %>%
    dplyr::group_map(.f = function(dat, key){
      sample <- tibble::tibble(
        dim = dat$min_dim:dat$max_dim
      )
      mod <- dat$mdl_prd$lm
      sample %<>%
        dplyr::mutate(
          # ici
          # bodyweight = predict(mod, sample, type = "response") %>% exp,
          bodyweight = predict(mod, sample, type = "response"),
          bodyweight_lag = dplyr::lag(bodyweight),
          bwchange = (bodyweight - bodyweight_lag) / bodyweight_lag
        )
      dat %>%
        dplyr::mutate(sampled = tibble::lst(sample = sample))
    }, .keep = T) %>%
    purrr::reduce(dplyr::bind_rows)
}


# Regression trend line

# Regression trend line
plot_bw <- function(dt, var = "bodyweight"){
  # construct legend
  # TODO Find a more trustworthy way to get the legend.
  legend <- cowplot::get_legend(
    ggplot2::ggplot(dt, ggplot2::aes(x = parity_full, y = parity_full, color = type,
                                     group = type)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::theme(legend.position = "bottom")
  )
  plt_lst <- dt %>%
    dplyr::group_by(an_id, parity_full) %>%
    dplyr::group_map(.f = function(dat, key){
      sampled <- dat %>%
        dplyr::select(sampled, type) %>%
        purrr::pmap(~dplyr::mutate(..1, type = ..2)) %>%
        purrr::reduce(dplyr::bind_rows) %>%
        dplyr::mutate(dataset = "sampled") %>%
        dplyr::select(dim, type, dataset, dplyr::one_of(var))
      if(var == "bodyweight"){
        former <- dat %>%
          dplyr::select(dataset, type) %>%
          purrr::pmap(~dplyr::mutate(..1, type = ..2)) %>%
          purrr::reduce(dplyr::bind_rows) %>%
          dplyr::mutate(dataset = "former") %>%
          dplyr::select(dim, type, dataset, dplyr::one_of(var))

        plot <- former %>%
          ggplot2::ggplot(ggplot2::aes_string(x = "dim",
                                              y = var,
                                              color = "type")) +
          ggplot2::geom_point() +
          ggplot2::geom_line(ggplot2::aes(group = type),
                             data = sampled, size = 1, color = "#666666") +
          ggplot2::theme(
            legend.position = "none"
          ) +
          ggplot2::coord_cartesian(ylim = c(500, 850),
                                   xlim = c(0, 175))
      }else{
        plot <- sampled %>%
          ggplot2::ggplot(ggplot2::aes_string(x = "dim",
                                              y = var,
                                              color = "type")) +
          ggplot2::geom_line(size = 1) +
          ggplot2::theme(
            legend.position = "none"
          )+
          ggplot2::coord_cartesian(ylim = c(-0.002, 0.002),
                                   xlim = c(0, 175))
      }

    }, .keep = T)
  return(tibble::lst(plt_lst, legend))
}


augmented_oos <- smooth_data(augmented_oos)


# plot
# Bodyweight
plt_lst <- plot_bw(augmented_oos)

data_plt <- cowplot::plot_grid(plotlist = plt_lst$plt_lst, nrow = 3)
cowplot::plot_grid(data_plt, plt_lst$legend,
                   nrow = 2,
                   rel_heights = c(15, 1))


plt_lst <- plot_bw(augmented_oos, "bwchange")

data_plt <- cowplot::plot_grid(plotlist = plt_lst$plt_lst, nrow = 4)
cowplot::plot_grid(data_plt, plt_lst$legend,
                   nrow = 2,
                   rel_heights = c(15, 1))


# Plot the change from the same initial basis.
# Args
# dt <- augmented_oos
# ai <- 5502204964
# pt <- 3
# dat <- dt %>% dplyr::filter(an_id == ai, parity_full == pt)
# var <- "bodyweight"
#body
plot_bw_samestart <- function(dt, var = "bodyweight"){
  legend <- cowplot::get_legend(
    ggplot2::ggplot(dt, ggplot2::aes(x = parity_full, y = parity_full,
                                     color = type)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::theme(legend.position = "bottom")
  )
  plt_lst <- dt %>%
    dplyr::group_by(an_id, parity_full) %>%
    dplyr::group_map(.f = function(dat, key){
      bw <- dat %>%
        dplyr::select(sampled, type) %>%
        purrr::pmap(~dplyr::mutate(..1, type = ..2)) %>%
        purrr::reduce(dplyr::bind_rows)
      bw %<>%
        dplyr::mutate(
          bodyweight = bw %>%
            dplyr::filter(dim == min(dim), type == "obs") %>%
            dplyr::pull(bodyweight),
          # bodyweight = dplyr::case_when(
          #   dim == min(dim) ~ initial_bw %>%
          #     dplyr::filter(dim == min(dim), type == "obs") %>%
          #     dplyr::pull(bodyweight),
          #   TRUE ~ NA_real_
          bwchange = tidyr::replace_na(bwchange, 0)
        ) %>%
        dplyr::group_by(type) %>%
        dplyr::mutate(
          cumbwchangerate = dplyr::order_by(dim, cumprod(1 + bwchange)),
          bodyweight = bodyweight * cumbwchangerate
        )

      plot <- bw %>%
        ggplot2::ggplot(ggplot2::aes_string(x = "dim",
                                            y = var,
                                            color = "type")) +
        ggplot2::geom_line() +
        ggplot2::theme(
          legend.position = "none"
        ) +
          ggplot2::coord_cartesian(ylim = c(500, 850),
                                   xlim = c(0, 175))
    })
  return(tibble::lst(plt_lst, legend))
}

plt_lst <- plot_bw_samestart(augmented_oos)

data_plt <- cowplot::plot_grid(plotlist = plt_lst$plt_lst, nrow = 3)
cowplot::plot_grid(data_plt, plt_lst$legend,
                   nrow = 2,
                   rel_heights = c(15, 1))


# ------------------------------------------------------------------------------
# milk yield
# ------------------------------------------------------------------------------
augmented_oos %>%
  dplyr::filter(type == "prd") %>%
  dplyr::pull(dataset) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = milk_yield)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(an_id ~., nrow = 4)


# Plot the change from the same initial basis.
# Args
dataset <- augmented_oos
ai <- 5502256647
pt <- 2
dat <- dataset %>% dplyr::filter(an_id == ai, parity_full == pt)
var <- "bodyweight"

#body
plot_student <- function(dataset, var = "bodyweight"){
  # legend <- cowplot::get_legend(
  #   ggplot2::ggplot(dt, ggplot2::aes(x = k, y = k, color = type)) +
  #     ggplot2::geom_point() +
  #     ggplot2::geom_line() +
  #     ggplot2::theme(legend.position = "bottom")
  # )
  plt_lst <- dataset %>%
    dplyr::group_by(an_id, parity_full) %>%
    dplyr::group_map(.f = function(dat, key){
      bwc <- dat %>%
        dplyr::select(sampled, type) %>%
        purrr::pmap(~dplyr::mutate(..1, type = ..2)) %>%
        purrr::reduce(dplyr::bind_rows) %>%
        tidyr::pivot_wider(id_cols = "dim",
                           names_from = "type",
                           values_from = "bwchange") %>%
        tidyr::drop_na() %>%
        dplyr::mutate(
          diff = obs - prd,
          std_diff = (diff - mean(diff))/ sd(diff)^2
          )
      degf <- nrow(bwc) - 1
      t <- mean(bwc$diff) / sd(bwc$diff)

      tibble::tibble(x = seq(-3, 3, by = .01)) %>%
        dplyr::mutate(
          s = dt(x, df = degf)
        ) %>%
      # plot <- bwc %>%
        # ggplot2::ggplot(ggplot2::aes(x = std_diff)) +
        ggplot2::ggplot(ggplot2::aes(x = x, y = s)) +
        ggplot2::geom_line() +
        # ggplot2::geom_histogram(data = bwc, ggplot2::aes(x = std_diff),
        #                       color = "steelblue") +
        # ggplot2::stat_function(fun = dt, args = list(x = seq(-3, 3, by = .01),
        #                                              df = degf),
        #                        color = "darkred") +
        ggplot2::geom_vline(xintercept = t, color = "darkred") +
        ggplot2::coord_cartesian(xlim = c(-3, 3))
    })
}

# plt_lst <- plot_student(augmented_oos)
#
# cowplot::plot_grid(plotlist = plt_lst, nrow = 5)



bwchange <- augmented_oos %>%
  dplyr::group_by(an_id, expl_id, parity_full, type) %>%
  dplyr::group_map(~dplyr::bind_cols(.x$sampled$sample, .y)) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  tidyr::drop_na() %>%
  dplyr::select(dim, bwchange, an_id, expl_id, parity_full, type) %>%
  tidyr::pivot_wider(id_cols = c("an_id", "expl_id", "parity_full", "dim"),
                     names_from = type, values_from = bwchange)

bw <- augmented_oos %>%
  dplyr::group_by(an_id, expl_id, parity_full, type) %>%
  dplyr::group_map(~dplyr::bind_cols(.x$sampled$sample, .y)) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  tidyr::drop_na() %>%
  dplyr::select(dim, bodyweight, an_id, expl_id, parity_full, type) %>%
  tidyr::pivot_wider(id_cols = c("an_id", "expl_id", "parity_full", "dim"),
                     names_from = type, values_from = bodyweight)
ai <- "5502256647"
pt <- 2

augmented_oos %>%
  dplyr::filter(an_id == ai, parity_full == pt) %>%
  plot_bw()
bw %>%
  dplyr::filter(an_id == ai, parity_full == pt) %>%
  # dplyr::mutate(
  #   obs = (obs - mean(obs)) / sd(obs),
  #   prd = (prd - mean(prd)) / sd(prd),
  # ) %>%
  ggplot2::ggplot(ggplot2::aes(y = obs, x = prd)) +
  ggplot2::geom_point()

augmented_oos %>%
  dplyr::filter(an_id == ai, parity_full == pt) %>%
  plot_bw()
augmented_oos %>%
  dplyr::filter(an_id == ai, parity_full == pt) %>%
  plot_bw("bwchange")
  # ggplot2::geom_abline(slope = 1, intercept = 0)
# bwchange %>%
#   dplyr::filter(an_id == ai, parity_full == pt) %>%
#   dplyr::mutate(pl = obs + prd) %>%
#   ggplot2::ggplot(ggplot2::aes(y = pl, x = dim)) +
#   ggplot2::geom_point()
bwchange %>%
  dplyr::filter(an_id == ai, parity_full == pt) %>%
  with(cor(obs, prd))
# error
bwchange %>%
  dplyr::mutate(
    obs = (obs - mean(obs)) / sd(obs),
    prd = (prd - mean(prd)) / sd(prd),
  ) %>%
  dplyr::mutate(error = obs - prd) %>%
  ggplot2::ggplot(ggplot2::aes(x = error)) +
  # ggplot2::stat_function(fun = dnorm) +
  ggplot2::geom_density()

bwchange %>%
  dplyr::group_by(an_id, expl_id, parity_full) %>%
  # dplyr::summarise(mobs = mean(obs),
  #                  mprd = mean(prd),
  #                  sobs = sd(obs),
  #                  sprd = sd(prd))
  # dplyr::mutate(
  #   obs = (obs - mean(obs)) / sd(obs),
  #   prd = (prd - mean(prd)) / sd(prd)
  # ) %>%
  ggplot2::ggplot(ggplot2::aes(y = obs, x = prd)) +
  ggplot2::geom_point()

with(bwchange, cor(obs, prd))

