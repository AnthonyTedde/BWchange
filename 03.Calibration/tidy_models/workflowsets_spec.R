library(magrittr)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Data and options ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

## Load the recipes
data("base_recipe")
data("pls_recipe")
data("pca_recipe")
data("filter_recipe")
# data("base_season_recipe")
# data("pls_season_recipe")
# data("pca_season_recipe")
# data("filter_season_recipe")

## Load the models specifications
data("lm_spec")
data("glm_spec")
data("rf_spec")
data("xgb_spec")
data("knn_spec")
data("linearSVM_spec")
data("polySVM_spec")
data("rbfSVM_spec")

## Global option
source(here::here("globals", "globals-models.R"))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# functions ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

update_map <- function(wfls, pattern, ...){
  #TODO take into account multiple arguments in (...)
  arg <- list(...)
  wfls_ids <- grep(pattern, wfls$wflow_id, value = T)
  for(wfl_id in wfls_ids){
    optionset <- tryCatch(
      wfls %>%
        dplyr::filter(wflow_id == wfl_id) %>%
        dplyr::pull(option) %>%
        purrr::map_chr(names),
      error = function(e) NULL
    )
    # Param already updated ? do not erase.
    if("param_info" %in% optionset){
      rf_options <- wfls %>%
        dplyr::filter(wflow_id == wfl_id) %>%
        dplyr::pull(option)
      rf_params <- rf_options[[1]]$param_info
    }else{
      rf_params <- wfls %>%
        workflowsets::extract_workflow(id = wfl_id) %>%
        dials::parameters()
    }
    rf_params %<>%
      update(!!names(arg) := arg[[1]])
    wfls %<>%
      workflowsets::option_add(param_info = rf_params,
                               id = wfl_id)
  }
  return(wfls)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Linear model workflowsets ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

wflset_linear <- workflowsets::workflow_set(
  preproc = list(
    pls = pls_recipe
    # pca = pca_recipe
    # cor = filter_recipe
  ),
  models = list(
    lm = lm_spec
    # glm = glm_spec
  ),
  cross = T
)

wflset_linear %<>% update_map(
  pattern = "^(?:pls|pca)",
  num_comp = dials::num_comp(c(6L, 15L))
) %>%
  update_map(
  pattern = "^(?:pls|pca)",
  degree_dim = dials::num_comp(c(1L, 5L))
)

# Check ([[1]], just for an example)
wflset_linear$option[[1]]$param_info$object


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Tree-based model workflowsets ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Specify
wflset_trees <- workflowsets::workflow_set(
  preproc = list(
    pls = pls_recipe
    # pca = pca_recipe,
    # cor = filter_recipe
  ),
  models = list(
    rf = rf_spec,
    xgb = xgb_spec
  ),
  cross = T
)


wflset_trees %<>% update_map(
  pattern = "^(?:pls|pca)",
  num_comp = dials::num_comp(c(1L, 15L))
) %>%
  update_map(
    pattern = "(?:pls|pca|cor)_(?:rf|xgb)$",
    mtry = dials::mtry(c(1L, max_mtry))
  )


wflset_trees$option[[1]]$param_info$object

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Distance-based model workflowsets ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

wflset_neighbor <- workflowsets::workflow_set(
  preproc = list(
    pls = pls_recipe,
    pca = pca_recipe,
    cor = filter_recipe
  ),
  models = list(
    knn = knn_spec
  ),
  cross = T
)

wflset_neighbor %<>% update_map(
  pattern = "^(?:pls|pca)",
  num_comp = dials::num_comp(c(1L, 15L))
)

wflset_neighbor$option[[1]]$param_info$object


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# SVM model workflowsets ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

wflset_SVM <- workflowsets::workflow_set(
  preproc = list(
    pls = pls_recipe,
    # pca = pca_recipe,
    cor = filter_recipe
  ),
  models = list(
    linearsvn = linearSVM_spec,
    polysvm = polySVM_spec
    # rbfsvm = rbfSVM_spec
  ),
  cross = T
)

wflset_SVM %<>% update_map(
  pattern = "^(?:pls|pca)",
  num_comp = dials::num_comp(c(6L, 15L))
)

wflset_SVM$option[[1]]$param_info$object


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Save ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

save(wflset_linear, file = here::here("data", "wflset_linear.rda"),
     compress = "xz")
save(wflset_trees, file = here::here("data", "wflset_trees.rda"),
     compress = "xz")
save(wflset_neighbor, file = here::here("data", "wflset_neighbor.rda"),
     compress = "xz")
save(wflset_SVM, file = here::here("data", "wflset_SVM.rda"),
     compress = "xz")
