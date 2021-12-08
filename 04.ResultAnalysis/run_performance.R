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
data("test_data")
data("BW_seenorest")

# mdl ####
data("pls_lst")

# Global variable ####
model <- pls_lst$HSO_all

# Test performance of the model ####
rmse <- function(dat){
  tibble::tibble(
    pred = predict(model, newdata = dat),
    obs = dat$bodyweight
  ) %>%
    yardstick::rmse(truth = obs, estimate = pred)
}

rmse(test_data)
rmse(train_data)
rmse(out_of_sample_mir)

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
      dplyr::mutate(bodyweight = predict(mdl, dataset))
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
        reg <- glm(bodyweight~ dim + wil, data=d, family = Gamma)
        stat <- summary(reg)
        # stat$r.squared - par
        1 - stat$deviance/stat$null.deviance
      }
      # Compute optimization
      # o <- optim(par = rep(0, 4), ftoM, d = dat)
      o <- optim(par = 0, ftoM, d = dat,
                 method = "Brent", lower = -.2, upper = .2,
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
      reg <- glm(bodyweight ~ dim + wil, data=dat, family = Gamma)
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
  dplyr::inner_join(out_of_sample_merged_count)
out_of_sample_bw_cleaned <- out_of_sample_bw %>%
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
          bwchange = bodyweight - bodyweight_lag
        )
      dat %>%
        dplyr::mutate(sampled = tibble::lst(sample = sample))
    }, .keep = T) %>%
    purrr::reduce(dplyr::bind_rows)
}


# Regression trend line
plot_bw <- function(dt, ai, pt){
  aoos_mir <- dt %>%
    dplyr::filter(an_id == ai, parity_full == pt) %>%
    dplyr::mutate(
      max_dim = min(max_dim),
      min_dim = max(min_dim)
    )
  # datapoint
  aoos_obs <- aoos_mir %>%
    dplyr::filter(type == "obs")
  aoos_prd <- aoos_mir %>%
    dplyr::filter(type == "prd")
  obs_dp <- aoos_obs$dataset$dat
  prd_dp <- aoos_prd$dataset$dat

  max_dim <- aoos_mir$max_dim %>% unique
  min_dim <- aoos_mir$min_dim %>% unique

  trend_df <- aoos_mir$type %>%
    purrr::map(.f = function(typ){
      tibble::tibble(
        dim = min_dim:max_dim,
        wil = exp(dim * aoos_mir %>%
                    dplyr::filter(type == typ) %>%
                    dplyr::pull(k)
        ),
        type = typ
      )
    }) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::group_by(type) %>%
    dplyr::group_map(.f = function(dat, key){
      mod <- aoos_mir %>%
        dplyr::filter(type == key$type) %>%
        dplyr::pull(mdl_prd) %>% `$`('lm')
      dat %>%
        dplyr::mutate(
          # bodyweight = predict(mod, dat)
          bodyweight = predict(mod, dat, type = "response")
        )
    },.keep = T) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::mutate(type = factor(type, levels = c("prd", "obs")))

  obs_dp %>%
    ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight)) +
    ggplot2::geom_point(color = "steelblue") +
    ggplot2::geom_point(data = prd_dp, color = "darkred") +
    ggplot2::geom_line(ggplot2::aes(color = type), data = trend_df) +
    ggplot2::theme(
      legend.position = "none"
    )

}


# Regression trend line
plot_bw <- function(dt, var = "bodyweight"){
  # construct legend
  # TODO Find a more trustworthy way to get the legend.
  legend <- cowplot::get_legend(
    ggplot2::ggplot(dt, ggplot2::aes(x = k, y = k, color = type)) +
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
          ggplot2::geom_line(data = sampled, size = 1) +
          ggplot2::theme(
            legend.position = "none"
          )
      }else{
        plot <- sampled %>%
          ggplot2::ggplot(ggplot2::aes_string(x = "dim",
                                              y = var,
                                              color = "type")) +
          ggplot2::geom_line(size = 1) +
          ggplot2::theme(
            legend.position = "none"
          )
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


plt_lst <- plot_bw(augmented_oos, "bwchange")

data_plt <- cowplot::plot_grid(plotlist = plt_lst$plt_lst, nrow = 5)
cowplot::plot_grid(data_plt, plt_lst$legend,
                   nrow = 2,
                   rel_heights = c(15, 1))
