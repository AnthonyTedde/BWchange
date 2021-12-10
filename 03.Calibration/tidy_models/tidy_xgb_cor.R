rm(list = ls())
library(magrittr)

data("train_cv_partition")
data("training_data")
data("testing_data")

# ------------------------------------------------------------------------------
# Globals variable
# ------------------------------------------------------------------------------
source("globals/globals-models.R")
max_mtry <- 50

dpins <- paste0("d", pin212_name)

## Global option
source(here::here("globals", "globals-models.R"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Base recipe ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

base_recipe <- recipes::recipe(x = training_data) %>%
  recipes::update_role(bodyweight, new_role = "outcome") %>%
  recipes::update_role(milk_yield, dim, parity_fct) %>%
  recipes::step_dummy(parity_fct) %>%
  recipes::update_role(dplyr::all_of(dpins)) %>%
  recipes::step_ns(dim, deg_free = tune::tune(id = "deg_free_dim")) %>%
  recipes::step_ns(milk_yield, deg_free = tune::tune(id = "deg_free_my")) %>%
  recipes::step_normalize(recipes::all_numeric_predictors()) %>%
  recipes::step_corr(tidyselect::starts_with("dpin"),
                     threshold = tune::tune())

tune::tunable(base_recipe)

# pls_recipe <- base_recipe %>%
#   recipes::step_pls(
#     tidyselect::starts_with("dpin"),
#     outcome = dplyr::vars(bodyweight),
#     num_comp = tune::tune()
#   )

xgb_spec <- parsnip::boost_tree(
  trees = tune::tune(),
  tree_depth = tune::tune(),
  min_n = tune::tune(),
  loss_reduction = tune::tune(),
  sample_size = tune::tune(),
  mtry = tune::tune(),
  learn_rate = tune::tune(),
) %>%
  parsnip::set_engine("xgboost") %>%
  parsnip::set_mode(mode = "regression")

xgb_wfl <- workflows::workflow() %>%
  workflows::add_recipe(base_recipe) %>%
  workflows::add_model(xgb_spec)

xgb_wfl %>% tune::tunable() %>%
  dplyr::pull(call_info)


# ------------------------------------------------------------------------------
# Range of value for pls
# ------------------------------------------------------------------------------


mdl_param_updtd <- xgb_wfl %>%
  dials::parameters() %>%
  update(
    mtry = dials::mtry(c(1, max_mtry)),
    # num_comp = dials::num_comp(c(2, 50)),
    deg_free_dim = dials::deg_free(c(1, 5)),
    deg_free_my = dials::deg_free(c(1, 5))
  )

mdl_param_updtd$object


options(tidymodels.dark = TRUE)

set.seed(42)
search_res_xgb <- tune::tune_bayes(
  xgb_wfl,
  resamples = train_cv_partition,
  iter = 100,
  initial = 30,
  metrics = yardstick::metric_set(yardstick::rmse),
  param_info = mdl_param_updtd,
  control = tune::control_bayes(verbose = T, no_improve = 15)
)


tune::show_best(search_res_xgb)
bst <- tune::select_best(search_res_xgb)
oneSe <- tune::select_by_one_std_err(
  search_res_xgb,
  metric = "rmse",
  learn_rate, threshold
)
search_res_xgb %>%
  tune::collect_metrics()

xgb_fit_onese <- xgb_wfl %>%
  tune::finalize_workflow(oneSe) %>%
  # dials::finalize(oneSe) %>%
  parsnip::fit(training_data)

broom::augment(xgb_fit_onese, new_data = training_data) %>%
  yardstick::rmse(truth = bodyweight, estimate = .pred)
broom::augment(xgb_fit_onese, new_data = testing_data) %>%
  dplyr::group_by(provider) %>%
  dplyr::group_map(.f = function(d, k){
    d %>%
      yardstick::rmse(truth = bodyweight, estimate = .pred) %>%
      tibble::add_column(provider = k, .before = 1)
  }, .keep = T) %>%
  purrr::reduce(dplyr::bind_rows)

xgb_fit_bst <- xgb_wfl %>%
  tune::finalize_workflow(bst) %>%
  # dials::finalize(oneSe) %>%
  parsnip::fit(training_data)

broom::augment(xgb_fit_bst, new_data = training_data) %>%
  yardstick::rmse(truth = bodyweight, estimate = .pred)
broom::augment(xgb_fit_bst, new_data = testing_data) %>%
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

lobstr::obj_size(xgb_fit_bst)
lobstr::obj_size(xgb_fit_onese)
lobstr::obj_sizes(search_res_xgb)

butcher::axe_call(pls_fit_bst, verbose = T)

search_res_xgb_cor <- search_res_xgb
xgb_cor_fit_bst <- xgb_fit_bst
xgb_cor_fit_onese <- xgb_fit_onese

system.time({
  save(search_res_xgb_cor,
       file = "data/search_res_xgb_cor.rda",
       compress = "xz")
  save(xgb_cor_fit_bst,
       file = "data/xgb_cor_fit_bst.rda",
       compress = "xz")
  save(xgb_cor_fit_onese,
       file = "data/xgb_cor_fit_onese.rda",
       compress = "xz")
})
