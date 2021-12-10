library(magrittr)

data("training_data")
source("globals/globals-models.R")

dpins <- paste0("d", pin212_name)


#-------------------------------------------------------------------------------
# Bare, base and interaction recipes ####
#
# Bare is the skeleton. Base is the base.
# base_interaction is yet another base but with interaction.
#
# See the file workflows specification for the effective usage.
#
#-------------------------------------------------------------------------------

bare_recipe <- recipes::recipe(x = training_data) %>%
  recipes::update_role(bodyweight, new_role = "outcome") %>%
  recipes::update_role(milk_yield, dim, parity_fct) %>%
  recipes::update_role(dplyr::all_of(dpins)) %>%
  recipes::step_dummy(parity_fct) %>%
  recipes::step_normalize(recipes::all_numeric_predictors())

base_recipe  <- bare_recipe %>%
  recipes::step_ns(dim, deg_free = tune::tune(id = "deg_free_dim")) %>%
  recipes::step_ns(milk_yield, deg_free = tune::tune(id = "deg_free_my"))

base_interaction_recipe <- base_recipe %>%
  recipes::step_interact(terms = ~dim:milk_yield) %>%
  recipes::step_interact(terms = ~parity:milk_yield)

recipes_lst <- list(
  bare_recipe = bare_recipe,
  base_recipe = base_recipe,
  base_interaction_recipe = base_interaction_recipe
)

rm(bare_recipe, base_recipe, base_interaction_recipe, training_data)


#-------------------------------------------------------------------------------
# Add dimensionality reduction step #####
#-------------------------------------------------------------------------------

# PLS
#---------------------

recipes_pls_lst <- lapply(recipes_lst, FUN = function(recipe){
  recipe %>%
    recipes::step_pls(
      tidyselect::starts_with("dpin"),
      outcome = dplyr::vars(bodyweight),
      num_comp = tune::tune()
    )
}) %>%
  structure(names = gsub(pattern = "_recipe",
                         replacement = "_pls_recipe",
                         x = names(recipes_lst)))


# PCA
#---------------------

recipes_pca_lst <- lapply(recipes_lst, FUN = function(recipe){
  recipe %>%
    recipes::step_pca(recipes::all_numeric_predictors(),
                      num_comp = tune::tune(),
                      threshold = tune::tune())
}) %>%
  structure(names = gsub(pattern = "_recipe",
                         replacement = "_pca_recipe",
                         x = names(recipes_lst)))

# TODO: Add isomap step.


#-------------------------------------------------------------------------------
# Add feature selection step #####
#-------------------------------------------------------------------------------

# correlation
#---------------------

recipes_cor_lst <- lapply(recipes_lst, FUN = function(recipe){
  recipe %>%
    recipes::step_corr(tidyselect::starts_with("dpin"),
                       threshold = tune::tune())
}) %>%
  structure(names = gsub(pattern = "_recipe",
                         replacement = "_cor_recipe",
                         x = names(recipes_lst)))


#-------------------------------------------------------------------------------
# bind all lists together and save ####
#-------------------------------------------------------------------------------

recipes_all_lst <- c(
  recipes_lst,
  recipes_cor_lst,
  recipes_pca_lst,
  recipes_pls_lst
)

for(n in names(recipes_all_lst)){
  assign(n, recipes_all_lst[[n]])
  save(list = n,
       file = here::here("data", glue::glue("{n}.rda")),
       compress = "xz")
}
