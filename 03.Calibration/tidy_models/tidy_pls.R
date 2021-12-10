rm(list = ls())
library(magrittr)

data("train_cv_partition")
data("training_data")
data("testing_data")
source("globals/globals-models.R")

dpins <- paste0("d", pin212_name)


#-------------------------------------------------------------------------------
# Base recipe ####
#-------------------------------------------------------------------------------

base_recipe <- recipes::recipe(x = training_data) %>%
  recipes::update_role(bodyweight, new_role = "outcome") %>%
  recipes::update_role(milk_yield, dim, parity_fct) %>%
  recipes::update_role(dplyr::all_of(dpins)) %>%
  recipes::step_dummy(parity_fct) %>%
  # recipes::step_interact(terms = ~dim:milk_yield) %>%
  # recipes::step_interact(terms = ~parity:milk_yield) %>%
  recipes::step_ns(dim, deg_free = tune::tune(id = "deg_free_dim")) %>%
  recipes::step_ns(milk_yield, deg_free = tune::tune(id = "deg_free_my")) %>%
  recipes::step_normalize(recipes::all_numeric_predictors())


#-------------------------------------------------------------------------------
# Model ####
#-------------------------------------------------------------------------------

pls_spec <- plsmod::pls(num_comp = tune::tune()) %>%
  parsnip::set_engine("mixOmics") %>%
  parsnip::set_mode("regression")


#-------------------------------------------------------------------------------
# Workflow ####
#-------------------------------------------------------------------------------

pls_wfl <- workflows::workflow() %>%
  workflows::add_recipe(base_recipe) %>%
  workflows::add_model(pls_spec)

pls_wfl %>% tune::tunable() %>%
  dplyr::pull(call_info)


# ------------------------------------------------------------------------------
# Range of value for Hyperparameters
# ------------------------------------------------------------------------------

mdl_param_updtd <- pls_wfl %>%
  dials::parameters() %>%
  update(num_comp = dials::num_comp(c(5, 50))) %>%
  update(deg_free_dim = dials::deg_free(c(1, 5))) %>%
  update(deg_free_my = dials::deg_free(c(1, 5)))

mdl_param_updtd$object

options(tidymodels.dark = TRUE)

set.seed(42)
search_res_pls <- tune::tune_bayes(
  pls_wfl,
  resamples = train_cv_partition,
  iter = 100,
  # initial = 30,
  initial = 5,
  metrics = yardstick::metric_set(yardstick::rmse),
  param_info = mdl_param_updtd,
  control = tune::control_bayes(verbose = T, no_improve = 10)
)

tune::show_best(search_res_pls)
bst <- tune::select_best(search_res_pls)
oneSe <- tune::select_by_one_std_err(
  search_res_pls,
  metric = "rmse",
  num_comp, deg_free_dim, deg_free_my
)
search_res_pls %>%
  tune::collect_metrics()

pls_fit_onese <- pls_wfl %>%
  tune::finalize_workflow(oneSe) %>%
  # dials::finalize(oneSe) %>%
  parsnip::fit(training_data)

broom::augment(pls_fit_onese, new_data = training_data) %>%
  yardstick::rmse(truth = bodyweight, estimate = .pred)
broom::augment(pls_fit_onese, new_data = testing_data) %>%
  dplyr::group_by(provider) %>%
  dplyr::group_map(.f = function(d, k){
    d %>%
      yardstick::rmse(truth = bodyweight, estimate = .pred) %>%
      tibble::add_column(provider = k, .before = 1)
  }, .keep = T) %>%
  purrr::reduce(dplyr::bind_rows)

pls_fit_bst <- pls_wfl %>%
  tune::finalize_workflow(bst) %>%
  # dials::finalize(oneSe) %>%
  parsnip::fit(training_data)

broom::augment(pls_fit_bst, new_data = training_data) %>%
  yardstick::rmse(truth = bodyweight, estimate = .pred)
broom::augment(pls_fit_bst, new_data = testing_data) %>%
  dplyr::group_by(provider) %>%
  dplyr::group_map(.f = function(d, k){
    d %>%
      yardstick::rmse(truth = bodyweight, estimate = .pred) %>%
      tibble::add_column(provider = k, .before = 1)
  }, .keep = T) %>%
  purrr::reduce(dplyr::bind_rows)

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------

lobstr::obj_size(pls_fit_bst)
lobstr::obj_size(pls_fit_onese)
lobstr::obj_sizes(search_res_pls)

butcher::axe_call(pls_fit_bst, verbose = T)

save(search_res_pls, file = "data/search_res_pls.rda", compress = "xz")
save(pls_fit_bst, file = "data/pls_fit_bst.rda", compress = "xz")
save(pls_fit_onese, file = "data/pls_fit_onese.rda", compress = "xz")

