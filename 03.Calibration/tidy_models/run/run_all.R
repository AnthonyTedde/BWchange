rm(list = ls())
library(magrittr)
library(tidymodels)
library(plsmod)


#-------------------------------------------------------------------------------
# Load data ####
#-------------------------------------------------------------------------------

data("wfl_names")
data("train_cv_partition")

source("globals/globals-models.R")

if(file.exists("data/wfl_done.rda")){
  data("wfl_done")
}else{
  wfl_done <- vector(mode = "character")
}

# Workflow to execute.
wfl_to_execute <- setdiff(wfl_names, wfl_done)


# ------------------------------------------------------------------------------
# Set parallel ####
# ------------------------------------------------------------------------------

#
# nproc in globals/globals-models.R
cl <- parallel::makePSOCKcluster(nproc)
doParallel::registerDoParallel(cl)

# ------------------------------------------------------------------------------
# Range of value for Hyperparameters
# ------------------------------------------------------------------------------

# TODO change variable location
p <- list(
  deg_free_dim = dials::deg_free(c(1L, 4L)),
  deg_free_my = dials::deg_free(c(1L, 4L)),
  num_comp = dials::num_comp(c(1L, 100L)),
  hidden_units = dials::hidden_units(c(1L, 100L)),
  mtry = dials::mtry(c(1, 70)),
  neighbors = dials::neighbors(c(1L, 50L))
)

for(wfl in wfl_to_execute){
  message("-------------------------------------------------------------------")
  message(glue::glue("IS RUNNING {wfl}"))
  message("-------------------------------------------------------------------")
  data(list = wfl)
  workflow <- get(wfl)

  # Update the parameters list
  # ---------------------------
  parameters <- workflow %>%
    dials::parameters()

  p_to_update <- p[names(p) %in% parameters$id]
  args <- c(
    list(object = parameters),
    p_to_update
  )

  if(length(p_to_update) > 0){
    parameters <- do.call(update, args = args)
  }

  init <- dim(parameters)[1] * 3

  # Train the models
  # ---------------------
  # TOCOMMENT
  # options(tidymodels.dark = TRUE)

  set.seed(42)
  search_res_pls <- tune::tune_bayes(
    workflow,
    resamples = train_cv_partition,
    iter = 200,
    initial = init,
    metrics = yardstick::metric_set(yardstick::rmse),
    param_info = parameters,
    control = tune::control_bayes(verbose = T,
                                  no_improve = 25,
                                  seed = 1010,
                                  save_workflow = T,
                                  parallel_over = "resamples")
  )

  # Save the state of computation and model results
  res_name <- paste(wfl, "results", sep = "_")
  assign(res_name, search_res_pls)
  save(list = res_name,
       file = glue::glue("data/{res_name}.rda"),
       compress = "xz")
  wfl_done <- c(wfl_done, wfl)
  save(wfl_done, file = "data/wfl_done.rda")

  # Free spaces
  rm(search_res_pls, workflow)
  rm(list = c(wfl, res_name))
  gc()
}
