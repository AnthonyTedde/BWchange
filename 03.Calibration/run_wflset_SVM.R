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


autoplot(wflset_SVM_tuned)

# Nothing to Butch in workflowsets
saveRDS(wflset_SVM_tuned,
        file = here::here("models", "wflset_SVM_tuned.rds"),
        compress = "xz")
