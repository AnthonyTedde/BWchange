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
data("wflset_SVM")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Tune the workflows ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

tictoc::tic()
wflset_SVM_tuned <- wflset_SVM %>%
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

autoplot(wflset_SVM_tuned)

data("train_data")

cor_linearsvm_wfl <- wflset_SVM_tuned %>%
  workflowsets::extract_workflow_set_result(id = "cor_linearsvn")

cor_linearsvm_param <- cor_linearsvm_wfl %>%
  tune::select_best()

cor_linearsvm_mod <- wflset_SVM_tuned %>%
  workflowsets::extract_workflow(id = "cor_linearsvn") %>%
  tune::finalize_workflow(cor_linearsvm_param) %>%
  fit(train_data)

lobstr::obj_size(cor_linearsvm_mod)

cor_linearsvm_mod %<>%
  butcher::butcher(verbose = T)
lobstr::obj_size(cor_linearsvm_mod)

# Nothing to Butch in workflowsets
saveRDS(cor_linearsvm_mod,
        file = here::here("models",
                          "cor_linearsvm_mod.rds"))

rmse <- function(dat, mdl){
  tibble::tibble(
    # ici
    # pred = predict(model, newdata = dat, ncomp = ncomp, type = "response"),
    pred = predict(mdl, new_data = dat, type = "raw") %>%
      drop %>% unlist %>% unname,
    obs = dat$bodyweight
  ) %>%
    yardstick::rmse(truth = obs, estimate = pred)
}

rmse(dat = train_data, mdl = cor_linearsvm_mod)

