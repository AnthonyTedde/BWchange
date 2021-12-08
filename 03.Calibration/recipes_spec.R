library(magrittr)

data("training_data")
source("globals/globals-models.R")

dpins <- paste0("d", pin212_name)


#-------------------------------------------------------------------------------
# Base recipe ####
#-------------------------------------------------------------------------------

base_recipe <- recipes::recipe(x = train_data_tune) %>%
  recipes::update_role(bodyweight, new_role = "outcome") %>%
  recipes::update_role(milk_yield, dim, parity) %>%
  recipes::update_role(dplyr::all_of(dpins)) %>%
  recipes::step_log(dim) %>%
  recipes::step_interact(terms = ~dim:milk_yield) %>%
  recipes::step_interact(terms = ~parity:milk_yield) %>%

  # recipes::step_poly(dim, degree = tune::tune("degree_dim")) %>%
  # recipes::step_poly(milk_yield, degree = tune::tune("degree_my")) %>%
  recipes::step_normalize(recipes::all_numeric_predictors())


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Pls custom recipe #####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

base_season_recipe <- base_recipe %>%
  recipes::update_role(test_MIR_season, new_role = "predictor") %>%
  recipes::step_dummy(recipes::all_nominal_predictors())

pls_recipe <- base_recipe %>%
  recipes::step_pls(recipes::all_numeric_predictors(),
                    outcome = recipes::all_outcomes(),
                    num_comp = tune::tune())

# PCA custome recipe ####
pca_recipe <- base_recipe %>%
  recipes::step_pca(recipes::all_numeric_predictors(),
                    num_comp = tune::tune(),
                    threshold = tune::tune())

filter_recipe <- base_recipe %>%
  recipes::step_corr(dplyr::starts_with("dpin"),
                     threshold = tune::tune())


pls_season_recipe <- base_season_recipe %>%
  recipes::step_pls(recipes::all_numeric_predictors(),
                    outcome = recipes::all_outcomes(),
                    num_comp = tune::tune())

pca_season_recipe <- base_season_recipe %>%
  recipes::step_pca(recipes::all_numeric_predictors(),
                    num_comp = tune::tune(),
                    threshold = tune::tune())

filter_season_recipe <- base_season_recipe %>%
  recipes::step_corr(dplyr::starts_with("dpin"),
                     threshold = tune::tune())
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Save ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

save(base_recipe,
     file = here::here("data", "base_recipe.rda"),
     compress = "xz")
save(pls_recipe,
     file = here::here("data", "pls_recipe.rda"),
     compress = "xz")
save(pca_recipe,
     file = here::here("data", "pca_recipe.rda"),
     compress = "xz")
save(filter_recipe,
     file = here::here("data", "filter_recipe.rda"),
     compress = "xz")

save(base_season_recipe,
     file = here::here("data", "base_season_recipe.rda"),
     compress = "xz")
save(pls_season_recipe,
     file = here::here("data", "pls_season_recipe.rda"),
     compress = "xz")
save(pca_season_recipe,
     file = here::here("data", "pca_season_recipe.rda"),
     compress = "xz")
save(filter_season_recipe,
     file = here::here("data", "filter_season_recipe.rda"),
     compress = "xz")
