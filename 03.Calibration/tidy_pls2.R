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
  recipes::update_role(dim, parity) %>%
  # recipes::update_role(milk_yield, dim, parity,
  #                      fat_rate_2_prd, protein_rate_1_prd,
  #                      C18.1_cis9_prd_in_fat, C18_prd_in_fat
  #                      ) %>%
  recipes::update_role(dplyr::all_of(dpins)) %>%
  # recipes::step_interact(terms = ~dim:milk_yield) %>%
  # recipes::step_interact(terms = ~parity:milk_yield) %>%
  # recipes::step_ns(dim, deg_free = tune::tune(id = "deg_free_dim")) %>%
  # recipes::step_ns(milk_yield, deg_free = tune::tune(id = "deg_free_my")) %>%
  # recipes::step_ns(fat_rate_2_prd, deg_free = tune::tune(id = "deg_free_fat")) %>%
  # recipes::step_ns(protein_rate_1_prd, deg_free = tune::tune(id = "deg_free_prot")) %>%
  # recipes::step_ns(C18.1_cis9_prd_in_fat, deg_free = tune::tune(id = "deg_free_c189")) %>%
  # recipes::step_ns(C18_prd_in_fat, deg_free = tune::tune(id = "deg_free_c18")) %>%
  recipes::step_normalize(recipes::all_numeric_predictors())

# pls_recipe <- base_recipe %>%
#   recipes::step_pls(recipes::all_numeric_predictors(),
#                     outcome = recipes::all_outcomes(),
#                     num_comp = tune::tune())

pls_spec <- plsmod::pls(num_comp = tune::tune()) %>%
  parsnip::set_engine("mixOmics") %>%
  parsnip::set_mode("regression")

pls_wfl <- workflows::workflow() %>%
  workflows::add_recipe(base_recipe) %>%
  workflows::add_model(pls_spec)

pls_wfl %>% tune::tunable() %>%
  dplyr::pull(call_info)


# ------------------------------------------------------------------------------
# Range of value for pls
# ------------------------------------------------------------------------------
mdl_param_updtd <- pls_wfl %>%
  dials::parameters() %>%
  update(num_comp = dials::num_comp(c(10, 100)))
  # update(num_comp = dials::num_comp(c(10, 60))) %>%
  # update(deg_free_dim = dials::deg_free(c(5, 40))) %>%
  # update(deg_free_my = dials::deg_free(c(5, 40))) %>%
  # update(deg_free_fat = dials::deg_free(c(5, 40))) %>%
  # update(deg_free_prot = dials::deg_free(c(5, 40))) %>%
  # update(deg_free_c189 = dials::deg_free(c(5, 40))) %>%
  # update(deg_free_c18 = dials::deg_free(c(5, 40)))



set.seed(42)
search_res_pls2 <- tune::tune_bayes(
  pls_wfl,
  resamples = train_cv_partition,
  iter = 100,
  initial = 40,
  # initial = 5,
  metrics = yardstick::metric_set(yardstick::rmse),
  param_info = mdl_param_updtd,
  control = tune::control_bayes(verbose = T, no_improve = 10)
)

tune::show_best(search_res_pls2)
bst <- tune::select_best(search_res_pls2)
oneSe <- tune::select_by_one_std_err(
  search_res_pls2,
  metric = "rmse",
  num_comp, deg_free_dim, deg_free_my
)
search_res_pls2 %>%
  tune::collect_metrics()

pls_fit_onese2 <- pls_wfl %>%
  tune::finalize_workflow(oneSe) %>%
  # dials::finalize(oneSe) %>%
  parsnip::fit(training_data)

broom::augment(pls_fit_onese2, new_data = training_data) %>%
  yardstick::rmse(truth = bodyweight, estimate = .pred)
broom::augment(pls_fit_onese2, new_data = testing_data) %>%
  dplyr::group_by(provider) %>%
  dplyr::group_map(.f = function(d, k){
    d %>%
      yardstick::rmse(truth = bodyweight, estimate = .pred) %>%
      tibble::add_column(provider = k, .before = 1)
  }, .keep = T) %>%
  purrr::reduce(dplyr::bind_rows)

pls_fit_bst2 <- pls_wfl %>%
  tune::finalize_workflow(bst) %>%
  # dials::finalize(oneSe) %>%
  parsnip::fit(training_data)

broom::augment(pls_fit_bst2, new_data = training_data) %>%
  yardstick::rmse(truth = bodyweight, estimate = .pred)
broom::augment(pls_fit_bst2, new_data = testing_data) %>%
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

lobstr::obj_size(pls_fit_bst2)
lobstr::obj_size(pls_fit_onese2)
lobstr::obj_sizes(search_res_pls2)

butcher::axe_call(pls_fit_bst2, verbose = T)

save(pls_fit_bst2, file = "data/pls_fit_bst2.rda", compress = "xz")
save(pls_fit_onese2, file = "data/pls_fit_onese2.rda", compress = "xz")
save(search_res_pls2, file = "data/search_res_pls2.rda", compress = "xz")
