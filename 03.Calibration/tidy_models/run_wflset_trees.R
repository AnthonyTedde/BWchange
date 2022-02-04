library(magrittr)

data("train_cv_partition")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Global variable ####
# TODO Change those parameter once on the server.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

source(here::here("globals", "globals-models.R"))

doParallel::registerDoParallel(cores = ncores)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Set up the workflow ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Depending on the file, write the workflowset to map
data("wflset_trees")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Tune the workflows ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

tictoc::tic()
wflset_trees_tuned <- wflset_trees %>%
  # butcher::butcher(verbose = T) %>%
  workflowsets::workflow_map(
    fn = "tune_bayes",
    verbose = verbose_opt,
    seed = 42,
    resample = train_cv_partition,
    iter = tune_iter,
    initial = init_bayes,
    control = tune::control_bayes(
      verbose = verbose_opt,
      no_improve = last_noimproved
    ))
tictoc::toc()


autoplot(wflset_trees_tuned)

# ------------------------------------------------------------------------------
# Exctract ####
# ------------------------------------------------------------------------------

data("train_data")
pls_rf_wfl <- wflset_trees_tuned %>%
  workflowsets::extract_workflow_set_result(id = "pls_rf")


pls_rf_param <- pls_rf_wfl %>%
  tune::select_best()

pls_rf_mod <- wflset_trees_tuned %>%
  workflowsets::extract_workflow(id = "pls_rf") %>%
  tune::finalize_workflow(pls_rf_param) %>%
  fit(train_data)


# pls_glm_gamma_mod <- wflset_linear_tuned %>%
#   workflowsets::extract_workflow(id = "pls_glm") %>%
#   tune::finalize_workflow(pls_glm_gamma_param) %>%
#   fit(train_data_tune)

lobstr::obj_size(pls_lm_mod)
# lobstr::obj_size(pls_glm_gamma_mod)

pls_lm_mod %<>%
  butcher::butcher(verbose = T)
lobstr::obj_size(pls_lm_mod)

# pls_glm_gamma_mod_butch <- pls_glm_gamma_mod %>%
#   butcher::butcher(verbose = T)

# lobstr::obj_size(pls_lm_mod)

# Nothing to Butch in workflowsets
saveRDS(pls_lm_mod, file = here::here("models", "pls_lm_mod.rds"))

# Nothing to Butch in workflowsets
saveRDS(wflset_trees_tuned,
        file = here::here("models", "wflset_trees_tuned.rds"),
        compress = "xz")
