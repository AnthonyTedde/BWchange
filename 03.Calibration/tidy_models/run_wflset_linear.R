library(magrittr)

data("train_cv_partition")
data("train_data")
data("train_data_tune")

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
data("wflset_linear")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Tune the workflows ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

tictoc::tic()
wflset_linear_tuned <- wflset_linear %>%
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


autoplot(wflset_linear_tuned)

a <- wflset_linear_tuned %>%
  workflowsets::extract_workflow_set_result(id = "pls_lm")
# b <- wflset_linear_tuned %>%
#   workflowsets::extract_workflow_set_result(id = "pls_glm")

# tictoc::tic()
# wflset_linear_rmse_mod <- tidyposterior::perf_mod(
#   a,
#   hetero_var = T,
#   seed = 4242,
#   iter = 5000,
#   verbose = verbose_opt, refresh = 0
# )
# tictoc::toc()

wflset_linear_rmse_mod
a_mod <- wflset_linear_rmse_mod
autoplot(a_mod)



lobstr::obj_size(a)
lobstr::obj_size(wflset_linear_tuned)

## Bayesian analysis
tictoc::tic()
wflset_linear_rmse_mod <- tidyposterior::perf_mod(
  wflset_linear_tuned,
  hetero_var = T,
  seed = 4242,
  iter = 5000,
  verbose = verbose_opt, refresh = 0
)
tictoc::toc()

class(wflset_linear_rmse_mod) # TODO: to save!
lobstr::obj_size(wflset_linear_rmse_mod)
autoplot(wflset_linear_rmse_mod, type = "posteriors")


#+++++++++++++++++
# Extract model ####
#+++++++++++++++++

pls_lm_wfl <- wflset_linear_tuned %>%
  workflowsets::extract_workflow_set_result(id = "pls_lm")

# pls_glm_gamma_wfl <- wflset_linear_tuned %>%
#   workflowsets::extract_workflow_set_result(id = "pls_glm")


pls_lm_param <- pls_lm_wfl %>%
  tune::select_best()

# pls_glm_gamma_param <- pls_glm_gamma_wfl %>%
# tune::select_best()

pls_lm_mod <- wflset_linear_tuned %>%
  workflowsets::extract_workflow(id = "pls_lm") %>%
  tune::finalize_workflow(pls_lm_param) %>%
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
# saveRDS(pls_glm_gamma_mod_butch, file = here::here("models", "pls_glm_gamma_mod_butch.rds"))



# Alamano
my_param <- tibble::tibble(
  degree_dim = 1, degree_my = 1, num_comp = 15
)
pls_lm_mod2 <- wflset_linear_tuned %>%
  workflowsets::extract_workflow(id = "pls_lm") %>%
  tune::finalize_workflow(my_param) %>%
  fit(train_data_tune)

pls_lm_mod2_butch <- pls_lm_mod2 %>%
  butcher::butcher(verbose = T)
saveRDS(pls_lm_mod2_butch, file = here::here("models", "pls_lm_mod2_butch.rds"))
