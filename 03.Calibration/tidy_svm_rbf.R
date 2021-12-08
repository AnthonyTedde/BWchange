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
# PCA and PLS ?  ####
# ------------------------------------------------------------------------------

form <- grep(pattern = "^dpin", names(training_data), value = T) %>%
  paste(collapse = "+") %>%
  paste("bodyweight", ., sep = "~") %>% formula
training_spectral_data <- training_data %>%
  dplyr::select(dplyr::starts_with("dpin"))

# 17 comp == 99% Variance
pls_mod <- pls::mvr(form, data = training_data, scale = T, center = T)
cumsum(pls_mod$Xvar / pls_mod$Xtotvar)
# 15 comp == 99% variance
pca_mod <- FactoMineR::PCA(training_spectral_data, ncp = 15, graph = F)
pca_mod$eig

PCA_coord <- predict(pca_mod, newdata = training_data)$coord %>%
  tibble::as_tibble() %>%
  setNames(paste0("PC", stringr::str_pad(1:15, width = 2, pad = "0")))

PLS_comp <- predict(pls_mod, newdata = training_data,
                    type = "scores", ncomp = 1:17) %>%
  tibble::as_tibble() %>%
  setNames(paste0("C", stringr::str_pad(1:17, width = 2, pad = "0")))

dplyr::bind_cols(PCA_coord, PLS_comp) %>%
  cor %>%
  corrplot::corrplot(order = "hclust")

dplyr::bind_cols(PCA_coord, PLS_comp) %>%
  ggplot2::ggplot(ggplot2::aes(x = PC09, y = C09)) +
  ggplot2::geom_point()


# ------------------------------------------------------------------------------
# Base recipe ####
# ------------------------------------------------------------------------------

base_recipe <- recipes::recipe(x = training_data) %>%
  recipes::update_role(bodyweight, new_role = "outcome") %>%
  recipes::update_role(milk_yield, dim, parity_fct) %>%
  recipes::step_dummy(parity_fct) %>%
  recipes::update_role(dplyr::all_of(dpins)) %>%
  # recipes::step_ns(dim, deg_free = tune::tune(id = "deg_free_dim")) %>%
  # recipes::step_ns(milk_yield, deg_free = tune::tune(id = "deg_free_my")) %>%
  recipes::step_normalize(recipes::all_numeric_predictors())
  # recipes::step_corr(tidyselect::starts_with("dpin"),
  #                    threshold = tune::tune())

tune::tunable(base_recipe)

pls_recipe <- base_recipe %>%
  recipes::step_pls(
    tidyselect::starts_with("dpin"),
    outcome = dplyr::vars(bodyweight),
    num_comp = tune::tune()
  )

tune::tunable(pls_recipe)

svm_rbf_spec <- parsnip::svm_rbf(
  cost = tune::tune(),
  margin = tune::tune(),
  rbf_sigma = tune::tune()
) %>%
  parsnip::set_mode("regression") %>%
  parsnip::set_engine("kernlab")

svm_rbf_spec %>% parsnip::translate()

# Which hyper parameters could be tuned ?
tune::tunable(svm_rbf_spec)

svm_rfb_wfl <- workflows::workflow() %>%
  workflows::add_recipe(pls_recipe) %>%
  workflows::add_model(svm_rbf_spec)

svm_rfb_wfl %>% tune::tunable() %>%
  dplyr::pull(call_info)


# ------------------------------------------------------------------------------
# Range of value for pls
# ------------------------------------------------------------------------------

mdl_param_updtd <- svm_rfb_wfl %>%
  dials::parameters() %>%
  update(
    # mtry = dials::mtry(c(1, max_mtry)),
    num_comp = dials::num_comp(c(2, 50))
    # deg_free_dim = dials::deg_free(c(1, 5)),
    # deg_free_my = dials::deg_free(c(1, 5))
  )

mdl_param_updtd$object


options(tidymodels.dark = TRUE)

set.seed(42)
search_res_svm_rfb <- tune::tune_bayes(
  svm_rfb_wfl,
  resamples = train_cv_partition,
  iter = 100,
  initial = 20,
  metrics = yardstick::metric_set(yardstick::rmse),
  param_info = mdl_param_updtd,
  control = tune::control_bayes(verbose = T, no_improve = 15)
)


tune::show_best(search_res_svm_rfb)
bst <- tune::select_best(search_res_svm_rfb)
oneSe <- tune::select_by_one_std_err(
  search_res_svm_rfb,
  metric = "rmse",
  cost
)
search_res_svm_rfb %>%
  tune::collect_metrics()

svm_rfb_fit_onese <- svm_rfb_wfl %>%
  tune::finalize_workflow(oneSe) %>%
  # dials::finalize(oneSe) %>%
  parsnip::fit(training_data)

broom::augment(svm_rfb_fit_onese, new_data = training_data) %>%
  yardstick::rmse(truth = bodyweight, estimate = .pred)
broom::augment(svm_rfb_fit_onese, new_data = testing_data) %>%
  dplyr::group_by(provider) %>%
  dplyr::group_map(.f = function(d, k){
    d %>%
      yardstick::rmse(truth = bodyweight, estimate = .pred) %>%
      tibble::add_column(provider = k, .before = 1)
  }, .keep = T) %>%
  purrr::reduce(dplyr::bind_rows)

svm_rfb_fit_bst <- svm_rfb_wfl %>%
  tune::finalize_workflow(bst) %>%
  # dials::finalize(oneSe) %>%
  parsnip::fit(training_data)

broom::augment(svm_rfb_fit_bst, new_data = training_data) %>%
  yardstick::rmse(truth = bodyweight, estimate = .pred)
broom::augment(svm_rfb_fit_bst, new_data = testing_data) %>%
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

lobstr::obj_size(svm_rfb_fit_bst)
lobstr::obj_size(svm_rfb_fit_onese)
lobstr::obj_sizes(search_res_svm_rfb)

butcher::axe_call(pls_fit_bst, verbose = T)


system.time({
  save(search_res_svm_rfb,
       file = "data/search_res_svm_rfb.rda",
       compress = "xz")
  save(svm_rfb_fit_bst,
       file = "data/svm_rfb_fit_bst.rda",
       compress = "xz")
  save(svm_rfb_fit_onese,
       file = "data/svm_rfb_fit_onese.rda",
       compress = "xz")
})
