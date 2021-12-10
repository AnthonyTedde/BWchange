rm(list = ls())
library(magrittr)

data("train_cv_partition")
data("training_data")
data("testing_data")
source("globals/globals-models.R")

dpins <- paste0("d", pin212_name)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Base recipe ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

base_recipe <- recipes::recipe(x = training_data) %>%
  recipes::update_role(bodyweight, new_role = "outcome") %>%
  recipes::update_role(dim, parity_fct) %>%
  # recipes::update_role(milk_yield, dim, parity_fct) %>%
  recipes::step_dummy(parity_fct) %>%
  recipes::step_ns(dim, deg_free = tune::tune(id = "deg_free_dim")) %>%
  # recipes::step_ns(milk_yield, deg_free = tune::tune(id = "deg_free_my")) %>%
  recipes::step_normalize(recipes::all_numeric_predictors())


lm_spec <- parsnip::linear_reg() %>%
  parsnip::set_engine("lm") %>%
  parsnip::set_mode("regression")

lm_wfl <- workflows::workflow() %>%
  workflows::add_recipe(base_recipe) %>%
  workflows::add_model(lm_spec)

lm_wfl %>% tune::tunable() %>%
  dplyr::pull(call_info)


# ------------------------------------------------------------------------------
# Range of value for lm
# ------------------------------------------------------------------------------
mdl_param_updtd <- lm_wfl %>%
  dials::parameters() %>%
  update(deg_free_dim = dials::deg_free(c(1, 15)))
  # update(deg_free_my = dials::deg_free(c(1, 15)))

mdl_param_updtd$object



set.seed(42)
search_res_lm <- tune::tune_bayes(
  lm_wfl,
  resamples = train_cv_partition,
  iter = 10,
  initial = 5,
  metrics = yardstick::metric_set(yardstick::rmse),
  param_info = mdl_param_updtd,
  control = tune::control_bayes(verbose = T)
  # control = tune::control_bayes(verbose = T, no_improve = 5)
)

tune::show_best(search_res_lm)
bst <- tune::select_best(search_res_lm)
oneSe <- tune::select_by_one_std_err(
  search_res_lm,
  metric = "rmse",
  deg_free_dim
)
search_res_lm %>%
  tune::collect_metrics()

lm_fit_onese <- lm_wfl %>%
  tune::finalize_workflow(oneSe) %>%
  # dials::finalize(oneSe) %>%
  parsnip::fit(training_data)

broom::augment(lm_fit_onese, new_data = training_data) %>%
  yardstick::rmse(truth = bodyweight, estimate = .pred)
broom::augment(lm_fit_onese, new_data = testing_data) %>%
  dplyr::group_by(provider) %>%
  dplyr::group_map(.f = function(d, k){
    d %>%
      yardstick::rmse(truth = bodyweight, estimate = .pred) %>%
      tibble::add_column(provider = k, .before = 1)
  }, .keep = T) %>%
  purrr::reduce(dplyr::bind_rows)

lm_fit_bst <- lm_wfl %>%
  tune::finalize_workflow(bst) %>%
  # dials::finalize(oneSe) %>%
  parsnip::fit(training_data)

broom::augment(lm_fit_bst, new_data = training_data) %>%
  yardstick::rmse(truth = bodyweight, estimate = .pred)
broom::augment(lm_fit_bst, new_data = testing_data) %>%
  dplyr::group_by(provider) %>%
  dplyr::group_map(.f = function(d, k){
    d %>%
      yardstick::rmse(truth = bodyweight, estimate = .pred) %>%
      tibble::add_column(provider = k, .before = 1)
  }, .keep = T) %>%
  purrr::reduce(dplyr::bind_rows)

broom::glance(lm_fit_bst)
broom::tidy(lm_fit_bst)

tidyr::expand_grid(
  dim = 1:365,
  parity_fct = unique(training_data$parity_fct)
) %>%
  dplyr::mutate(.,
    bodyweight = predict(lm_fit_onese, new_data = .) %>% pull
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight, color = parity_fct)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(data = training_data, alpha = .2)

library(splines)
dataset_cleaned %>%
  # dplyr::filter(!provider %in% c("seenorest", "swiss", "utrobe")) %>%
  dplyr::filter(!provider %in% c("seenorest", "swiss")) %>%
  ggplot2::ggplot(ggplot2::aes(x = milk_yield, y = bodyweight, color = provider)) +
  ggplot2::geom_point(alpha = .2) +
  ggplot2::geom_smooth(ggplot2::aes(color = NA), method = "lm",
                       formula = y ~ ns(x, 2), color = "darkred",
                       se = F) +
  ggplot2::geom_smooth(ggplot2::aes(color = NA), method = "lm",
                       formula = y ~ ns(x, 3), color = "steelblue",
                       se = F) +
  ggplot2::geom_smooth(ggplot2::aes(color = NA), method = "lm",
                       formula = y ~ ns(x, 8), color = "black",
                       se = F) +
  ggplot2::geom_smooth(ggplot2::aes(color = NA), method = "lm",
                       formula = y ~ ns(x, 10), color = "red",
                       se = F)
+
   ggplot2::facet_grid(parity_fct~.)



# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------

lobstr::obj_size(lm_fit_bst)
lobstr::obj_size(lm_fit_onese)
lobstr::obj_sizes(search_res_lm)

butcher::axe_call(lm_fit_bst, verbose = T)

save(search_res_lm, file = "data/search_res_lm.rda", compress = "xz")
save(lm_fit_bst, file = "data/lm_fit_bst.rda", compress = "xz")
save(lm_fit_onese, file = "data/lm_fit_onese.rda", compress = "xz")
