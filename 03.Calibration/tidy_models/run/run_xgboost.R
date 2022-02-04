rm(list = ls())
library(parsnip)
library(usemodels)
library(recipes)
library(workflows)
library(workflowsets)

source("globals/globals-models.R")

data("training_data")
data("train_cv_partition")
data("recipe_names")

# recipe_names
# data("base_pls_recipe")
# data("base_recipe")


# ---------------------------------------------------------------------------- #
# -- Create recipes -- ####
dpin212_name <- paste0("d", pin212_name)
# 1. data_full_rec
# bodyweight ~ parity + ns(milk_yield, df=4) + ns(dim, df=4) + spectra
data_full_rec <- recipes::recipe(x = training_data) %>%
  recipes::update_role(bodyweight, new_role = "outcome") %>%
  # recipes::update_role(milk_yield, parity_ord, dim, new_role = "predictor") %>%
  recipes::update_role(milk_yield, dim, new_role = "predictor") %>%
  # recipes::update_role(tidyselect::all_of(dpin212_name),
  #                      new_role = "predictor") %>%
  # recipes::step_dummy(parity_ord) %>%
  # recipes::step_normalize(recipes::all_numeric_predictors()) %>%
  # recipes::step_ns(milk_yield, deg_free = 4) %>%
  # recipes::step_ns(dim, deg_free = 4)

# 2. data_pls_full_rec
# bodyweight ~ pls(parity + ns(milk_yield, df=4) + ns(dim, df=4) + spectra)
# Keep 212 comp because they explain 100% of the variance.
data_pls_full_rec <- data_full_rec %>%
  recipes::step_pls(
    recipes::all_predictors(),
    outcome = dplyr::vars(bodyweight),
    num_comp = 215
  )

# 3. data_pls_spectra_rec
# bodyweight ~ parity + ns(milk_yield, df=4) + ns(dim, df=4) + pls(spectra)
data_pls_spectra_rec <- data_full_rec %>%
  recipes::step_pls(
    tidyselect::all_of(dpin212_name),
    outcome = dplyr::vars(bodyweight),
    num_comp = 212)
# -- Create recipes -- #
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# -- Parsnip model specification -- ####
xgb_linear_spec <- parsnip::boost_tree(
  learn_rate = tune::tune(),
  stop_iter = 50,
) %>%
  parsnip::set_engine(
    engine = "xgboost",
    params = list(booster = "gblinear"),
    # booster = "gblinear",
    # lambda = tune::tune(),
    # alpha = tune::tune(),
    # updater = "coord_descent",
    # feature_selector = "greedy", top_k = 10,
    # objective = "reg:squarederror"
  ) %>%
  parsnip::set_mode(mode = "regression")

# xgb_spec %>% translate()

# Which hyper parameters could be tuned ?
# tune::tunable(xgb_spec)
# -- Parsnip model specification -- #
# ---------------------------------------------------------------------------- #

wflset_trees <- workflowsets::workflow_set(
  preproc = list(
    full = data_full_rec
    # pls_full = data_pls_full_rec,
    # pls_spectra = data_pls_spectra_rec
  ),
  models = list(
    xgb = xgb_linear_spec
  ),
  cross = T
)

# Default params
wflset_trees %>%
  workflowsets::extract_workflow(id = "full_xgb") %>%
  tune::parameters() %>%
  dplyr::pull()

# Same parameter update for all three because they share the same parameters:
param_updt <- wflset_trees %>%
  workflowsets::extract_workflow(id = "full_xgb") %>%
  dials::parameters() %>%
  update(
    # learn_rate = dials::learn_rate(c(-1, 0))
    # lambda = dials::penalty_L2(),
    # alpha = dials::penalty_L1()
  )


wflset_trees %<>%
  workflowsets::option_add(param_info = param_updt, id = "full_xgb") %>%
  workflowsets::option_add(param_info = param_updt, id = "pls_full_xgb") %>%
  workflowsets::option_add(param_info = param_updt, id = "pls_spectra_xgb")


# Run

options(tidymodels.dark = TRUE)

cl <- parallel::makePSOCKcluster(5)
doParallel::registerDoParallel(cl)

tictoc::tic()
wflset_trees_tuned <- wflset_trees %>%
  workflowsets::workflow_map(
    fn = "tune_bayes",
    verbose = TRUE,
    seed = 42,
    resample = train_cv_partition,
    iter = 1,
    initial = 2,
    control = tune::control_bayes(
      verbose = TRUE,
      no_improve = 15,
      uncertain = 5,
      parallel_over = "resamples"
    ))
tictoc::toc()

save(wflset_trees_tuned, file = "data/wflset_trees_tuned.rda")

data("wflset_trees_tuned")
wflset_trees_tuned

