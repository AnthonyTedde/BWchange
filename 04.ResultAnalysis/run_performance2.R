rm(list = ls())
library(bodyweight.data)
library(magrittr)

## TODO: Add all variables to the initial dataset.
## DONE: Check the computation of the drift correction
## TODO: Unsmooth data from bodyweight.data
## TODO: Rerun all script and explicitely change the dpin ton consider.


# Data ####
data("out_of_sample_mir")
data("out_of_sample_bw")
data("train_data")
data("BW_seenorest")
data("BW_fr1")

# TODO WOOD -->
# out_of_sample_mir <- train_data %>%
#   dplyr::filter(provider == "seenovia")
# out_of_sample_bw <- BW_fr1 %>%
#   dplyr::mutate(parity_full = parity) %>%
#   dplyr::mutate(an_id = paste0("FR", an_id))

# mdl ####
data("pls_test")
data("glm_test")
# data("pls_lst")

# Add comp column ####
ncomp <- (cumsum(pls_test$Xvar /  pls_test$Xtotvar) < .99) %>%
  which %>%
  seq_along()

out_of_sample_mir %<>%
  tibble::add_column(
    predict(pls_test, newdata = out_of_sample_mir, ncomp = ncomp, type = "scores") %>%
      tibble::as_tibble() %>%
      setNames(nm = paste0("Comp", stringr::str_pad(ncomp, width = 2, pad = "0")))
  )

# Global variable ####
# model <- pls_test
model <- glm_test

# model <- pls_lst$HSO_all
ncomp <- 9


# Test performance of the model ####
rmse <- function(dat){
  tibble::tibble(
    pred = predict(model, newdata = dat, ncomp = ncomp, type = "response"),
    obs = dat$bodyweight
  ) %>%
    yardstick::rmse(truth = obs, estimate = pred)
}

rmse(train_data)

# Create summary statistics datasets ####
out_of_sample_bw_count <- out_of_sample_bw %>%
  dplyr::filter(dim < 365 & parity_full < 4) %>%
  dplyr::count(parity_full, expl_id, an_id,  name = "nBW") %>%
  dplyr::arrange(desc(nBW))

out_of_sample_mir_count <- out_of_sample_mir %>%
  dplyr::filter(dim < 365 & parity_full < 4) %>%
  dplyr::count(parity_full, expl_id, an_id, parity_full, name = "nMIR") %>%
  dplyr::arrange(desc(nMIR)) %>%
  dplyr::filter(nMIR > 9)

out_of_sample_merged_count <- dplyr::inner_join(
  out_of_sample_bw_count,
  out_of_sample_mir_count
)


# Function for ................ ####
# dataset <- out_of_sample_mir %>%
#   dplyr::inner_join(out_of_sample_merged_count)
# mdl <- model

augment_dataframe <- function(dataset, mdl){
  if(!missing(mdl)){
    dataset %<>%
      dplyr::mutate(bodyweight = predict(mdl, dataset, ncomp = ncomp,
                                         type = "response"))
  }
  dataset %>%
    # Computation of the regression line by animal and parity.
    dplyr::group_by(an_id, expl_id, parity_full) %>%
    dplyr::group_map(.f = function(dat, key){
      # Function to optimize
      # ftoM <- function(d, par){
      #   d %<>%
      #     dplyr::mutate(wil = exp(dim * par)) %>%
      #     dplyr::mutate(w = 1/(dim+1))
      #   reg <- lm(bodyweight~ dim + wil, data=d, weights = w)
      #   stat <- summary(reg)
      #   stat$r.squared
      # }
      ftoM <- function(d, par){
        d %<>%
          dplyr::mutate(wil = exp(dim * par))
          # dplyr::mutate(w = 1/(dim+1))
        # reg <- lm(bodyweight~ dim + wil, data=d, weights = w)
        reg <- glm(bodyweight~ dim + wil, data=d,
                   family = Gamma, maxit = 1000)
        stat <- summary(reg)
        # stat$r.squared - par
        1 - stat$deviance/stat$null.deviance
      }
      # Compute optimization
      # o <- optim(par = rep(0, 4), ftoM, d = dat)
      o <- optim(par = 0, ftoM, d = dat,
                 method = "Brent", lower = -.9, upper = .9,
                 control = list(fnscale = -1))
      k <- o$par
      r2 <- o$value
      # Update the dataframe with the value of wil
      dat %<>%
        dplyr::mutate(
          wil = exp(dim*k)
        )
      # Create the final linear model
      # reg <- lm(bodyweight ~ dim + wil, data=dat)
      reg <- glm(bodyweight ~ dim + wil, data=dat, family = Gamma, maxit = 1000)
      # Output dataframe
      key %>%
        dplyr::mutate(
          mdl_prd = tibble::lst(lm = reg),
          k = k,
          r2 = r2,
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
  # dplyr::filter(dim < 200 & parity_full < 4) %>%
  dplyr::inner_join(out_of_sample_merged_count)
out_of_sample_bw_cleaned <- out_of_sample_bw %>%
  # ICI
  # dplyr::filter(dim < 200 & parity_full < 4) %>%
  dplyr::inner_join(out_of_sample_merged_count)

augmented_oos_mir <- augment_dataframe(out_of_sample_mir_cleaned, mdl = model)
augmented_oos_bw <- augment_dataframe(out_of_sample_bw_cleaned)


augmented_oos <- augmented_oos_bw %>%
  tibble::add_column(type = "obs") %>%
  dplyr::bind_rows(
    augmented_oos_mir %>%
      tibble::add_column(type = "prd")
  )

augmented_oos %>%
  dplyr::arrange(desc(r2))


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
        dim = dat$min_dim:dat$max_dim,
        wil = exp(dim * dat$k)
      )
      mod <- dat$mdl_prd$lm
      sample %<>%
        dplyr::mutate(
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
    ggplot2::ggplot(dt, ggplot2::aes(x = k, y = k, color = type,
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
          ggplot2::geom_point(alpha = .2) +
          ggplot2::geom_line(data = sampled, size = 1) +
          ggplot2::theme(
            legend.position = "none"
          ) +
          ggplot2::coord_cartesian(ylim = c(375, 800))
      }else{
        plot <- sampled %>%
          ggplot2::ggplot(ggplot2::aes_string(x = "dim",
                                              y = var,
                                              color = "type")) +
          ggplot2::geom_line(size = 1) +
          ggplot2::theme(
            legend.position = "none"
          )+
          ggplot2::coord_cartesian(ylim = c(-0.002, 0.002))
      }

    }, .keep = T)
  return(tibble::lst(plt_lst, legend))
}


augmented_oos <- smooth_data(augmented_oos)


# plot
# Bodyweight
plt_lst <- plot_bw(augmented_oos)

data_plt <- cowplot::plot_grid(plotlist = plt_lst$plt_lst, nrow = 5)
cowplot::plot_grid(data_plt, plt_lst$legend,
                   nrow = 2,
                   rel_heights = c(15, 1))


# plt_lst <- plot_bw(augmented_oos, "bwchange")
#
# data_plt <- cowplot::plot_grid(plotlist = plt_lst$plt_lst, nrow = 5)
# cowplot::plot_grid(data_plt, plt_lst$legend,
#                    nrow = 2,
#                    rel_heights = c(15, 1))


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
    ggplot2::ggplot(dt, ggplot2::aes(x = k, y = k, color = type)) +
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
        ggplot2::geom_point() +
        ggplot2::theme(
          legend.position = "none"
        )
    })
  return(tibble::lst(plt_lst, legend))
}

# plt_lst <- plot_bw_samestart(augmented_oos)
#
# data_plt <- cowplot::plot_grid(plotlist = plt_lst$plt_lst, nrow = 5)
# cowplot::plot_grid(data_plt, plt_lst$legend,
#                    nrow = 2,
#                    rel_heights = c(15, 1))



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
