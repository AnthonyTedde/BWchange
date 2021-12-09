rm(list = ls())
library(magrittr)

data("train_cv_partition")
data("training_data")
data("testing_data")


# ------------------------------------------------------------------------------
# Globals variable ####
# ------------------------------------------------------------------------------

source("globals/globals-models.R")

dpins <- paste0("d", pin212_name)


# ------------------------------------------------------------------------------
# Set parallel ####
# ------------------------------------------------------------------------------

#
# nproc in globals/globals-models.R
# ICI
# cl <- parallel::makePSOCKcluster(nproc)
cl <- parallel::makePSOCKcluster(1)
doParallel::registerDoParallel(cl)

# ------------------------------------------------------------------------------
# Base recipe ####
# ------------------------------------------------------------------------------

base_recipe <- recipes::recipe(x = training_data) %>%
  recipes::update_role(bodyweight, new_role = "outcome") %>%
  recipes::update_role(milk_yield, dim, parity_fct) %>%
  recipes::step_dummy(parity_fct) %>%
  recipes::update_role(dplyr::all_of(dpins)) %>%
  recipes::step_normalize(recipes::all_numeric_predictors()) %>%
  recipes::step_corr(tidyselect::starts_with("dpin"),
                     threshold = tune::tune())

tune::tunable(base_recipe)

svm_poly_cor_spec <- parsnip::svm_poly(
  cost = tune::tune(),
  degree = tune::tune(),
  margin = tune::tune(),
  scale_factor = tune::tune()
) %>%
  parsnip::set_mode("regression") %>%
  parsnip::set_engine("kernlab")

svm_poly_cor_spec %>% parsnip::translate()

# Which hyper parameters could be tuned ?
tune::tunable(svm_poly_cor_spec)

svm_poly_cor_wfl <- workflows::workflow() %>%
  workflows::add_recipe(base_recipe) %>%
  workflows::add_model(svm_poly_cor_spec)

svm_poly_cor_wfl %>% tune::tunable() %>%
  dplyr::pull(call_info)


# ------------------------------------------------------------------------------
# Range of value for pls
# ------------------------------------------------------------------------------

mdl_param_updtd <- svm_poly_cor_wfl %>%
  dials::parameters() %>%
  update(degree = dials::prod_degree(c(1, 5)))

#mdl_param_updtd$object

#options(tidymodels.dark = TRUE)

set.seed(42)
search_res_svm_poly_cor <- tune::tune_bayes(
  svm_poly_cor_wfl,
  resamples = train_cv_partition,
  #ICI
  # iter = 200,
  iter = 1,
  # ICI
  # initial = 30,
  initial = 5,
  metrics = yardstick::metric_set(yardstick::rmse),
  param_info = mdl_param_updtd,
  control = tune::control_bayes(verbose = T, no_improve = 15)
)

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------


system.time({
  save(search_res_svm_poly_cor,
       file = "data/search_res_svm_poly_cor.rda",
       compress = "xz")
  save(svm_poly_cor_wfl,
       file = "data/svm_poly_cor_wfl.rda",
       compress = "xz")
})
