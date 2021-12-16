rm(list = ls())
library(magrittr)


#------------------------------------------------------------------------------#
# Data and options ####
#------------------------------------------------------------------------------#

data("recipe_names")
data("model_spec_names")
data(list = recipe_names)
data(list = model_spec_names)

## Global option
source(here::here("globals", "globals-models.R"))


#------------------------------------------------------------------------------#
# functions ####
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# Linear model workflowsets ####
#------------------------------------------------------------------------------#

# Model specs:
#   * glm_spec
# Recipes:
#   * bencmark_recipe
#   * base_interation_recipe

glm_wflst <- workflowsets::workflow_set(
  preproc = list(
    benchmark = benchmark_recipe,
    base = base_recipe,
    base_pls = base_pls_recipe,
    base_pca = base_pca_recipe,
    base_cor = base_cor_recipe,
    base_interaction = base_interaction_recipe,
    base_interaction_pls = base_interaction_pls_recipe,
    base_interaction_pca = base_interaction_pca_recipe,
    base_interaction_cor = base_interaction_cor_recipe
  ),
  models = list(
    glm = glm_spec
  ),
  cross = T
)



# Model specs:
#   * pls_spec
# Recipes:
#   * base_recipe
#   * base_interaction_recipe

pls_wflst <- workflowsets::workflow_set(
  preproc = list(
    base_interaction = base_interaction_recipe,
    base = base_recipe
  ),
  models = list(
    pls = pls_spec
  ),
  cross = T
)

tune::tunable(pls_wflst)

linear_wflst <- dplyr::bind_rows(glm_wflst, pls_wflst)

#------------------------------------------------------------------------------#
# Others ####
#   * random forest
#   * Gradient Boosting trees
#   * K nearest neighbor
#   * Support Vector Machine (linear, polynomial and radial basis)
#   * Multi-layer perceptron
#------------------------------------------------------------------------------#

# rf_spec
#


nonlinear_wflst <- workflowsets::workflow_set(
  preproc = list(
    benchmark = benchmark_recipe,
    base_pls = base_pls_recipe,
    base_cor = base_cor_recipe,
    base_pca = base_pca_recipe
  ),
  models = list(
    rf = rf_spec,
    xgb = xgb_spec,
    knn = knn_spec,
    svm_linear = svm_linear_spec,
    svm_poly = svm_poly_spec,
    svm_rbf = svm_rbf_spec,
    mlp = mlp_spec
  ),
  cross = T
)


#------------------------------------------------------------------------------#
# All ####
#------------------------------------------------------------------------------#

all_wflst <- dplyr::bind_rows(linear_wflst, nonlinear_wflst)


#------------------------------------------------------------------------------#
# Save ####
#------------------------------------------------------------------------------#

for(i in all_wflst$wflow_id){
  wfl_name <- paste(i, "wfl", sep = "_")
  assign(
    wfl_name,
    workflowsets::extract_workflow(all_wflst, id = i)
  )
  save(list = wfl_name,
       file = glue::glue("data/{wfl_name}.rda"),
       compress = "xz")
}

wfl_names <- paste(all_wflst$wflow_id, "wfl", sep = "_")

save(wfl_names, file = "data/wfl_names.rda")
