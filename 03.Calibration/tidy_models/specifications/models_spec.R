rm(list = ls())
library(magrittr)


#-------------------------------------------------------------------------------
# Linear based models ####
#-------------------------------------------------------------------------------


# Ordinary least square
#---------------------

lm_spec <- parsnip::linear_reg() %>%
  parsnip::set_engine("lm")

# Which hyper parameters could be tuned ?
tune::tunable(lm_spec) # No one... what a shame XD


# Generalized Linear Model
#---------------------

glm_spec <- parsnip::linear_reg(penalty = tune::tune(),
                                mixture = tune::tune()) %>%
  parsnip::set_engine(engine = "glmnet", family = gaussian(link = "log"))

# Which hyper parameters could be tuned ?
tune::tunable(glm_spec) # penalty and mixture.


# Partial least square
#---------------------

tune::tunable(plsmod::pls() %>% parsnip::set_engine("mixOmics"))

pls_spec <- plsmod::pls(
  num_comp = tune::tune(),
  predictor_prop = tune::tune()
) %>%
  parsnip::set_engine("mixOmics") %>%
  parsnip::set_mode("regression")

#-------------------------------------------------------------------------------
# Trees based methods ####
#-------------------------------------------------------------------------------


# Random forest
#---------------------

rf_spec <- parsnip::rand_forest(
  mtry = tune::tune(),
  trees = tune::tune(),
  min_n = tune::tune()
) %>%
  parsnip::set_mode(mode = "regression") %>%
  parsnip::set_engine(engine = "ranger")

# Which hyper parameters could be tuned ?
tune::tunable(rf_spec) #mtry, trees, min_n


# XgBoost
#---------------------

xgb_spec <- parsnip::boost_tree(
  tree_depth = tune::tune(),
  trees = tune::tune(),
  learn_rate = tune::tune(),
  mtry = tune::tune(),
  min_n = tune::tune(),
  loss_reduction = tune::tune(),
  sample_size = tune::tune(),
  stop_iter = tune::tune()
) %>%
  parsnip::set_engine("xgboost") %>%
  parsnip::set_mode(mode = "regression")

# Which hyper parameters could be tuned ?
tune::tunable(xgb_spec)


#-------------------------------------------------------------------------------
# Distance based methods ####
#-------------------------------------------------------------------------------


# K nearest neighbor
#---------------------

knn_spec <- parsnip::nearest_neighbor(
  neighbors = tune::tune(),
  weight_func = tune::tune(),
  dist_power = tune::tune()
) %>%
  parsnip::set_mode(mode = "regression") %>%
  parsnip::set_engine(engine = "kknn")

# Which hyper parameters could be tuned ?
tune::tunable(knn_spec)


#-------------------------------------------------------------------------------
# Support vector machine ####
#-------------------------------------------------------------------------------


# Linear SVM
#---------------------

# Which hyper parameters could be tuned ?
tune::tunable(parsnip::svm_linear())

svm_linear_spec <- parsnip::svm_linear(
  cost = tune::tune(),
  margin = tune::tune()
) %>%
  parsnip::set_mode("regression") %>%
  parsnip::set_engine("LiblineaR")


# Polynomial SVM
#---------------------

# Which hyper parameters could be tuned ?
tune::tunable(parsnip::svm_poly())

svm_poly_spec <- parsnip::svm_poly(
  cost = tune::tune(),
  degree = tune::tune(),
  scale_factor = tune::tune(),
  margin = tune::tune()
) %>%
  parsnip::set_mode("regression") %>%
  parsnip::set_engine("kernlab")


# Radial basis SVM
#---------------------

# Which hyper parameters could be tuned ?
tune::tunable(parsnip::svm_rbf())

svm_rbf_spec <- parsnip::svm_rbf(
  cost = tune::tune(),
  margin = tune::tune(),
  rbf_sigma = tune::tune()
) %>%
  parsnip::set_mode("regression") %>%
  parsnip::set_engine("kernlab")


#-------------------------------------------------------------------------------
# Artificial Neural Net ####
#-------------------------------------------------------------------------------

tune::tunable(parsnip::mlp(engine = "keras"))

mlp_spec <- parsnip::mlp(
  hidden_units = tune::tune(),
  penalty = tune::tune(),
  epochs = tune::tune(),
  activation = tune::tune()
) %>%
  parsnip::set_mode("regression") %>%
  parsnip::set_engine("keras")

#-------------------------------------------------------------------------------
# Save ####
#-------------------------------------------------------------------------------


# All specification variables ends by _spec.
grep(pattern = "\\_spec", ls(), value = T) %>%
  for(spec in .){
    message(glue::glue("Save model {spec}."))
    save(list = spec,
         file = here::here("data", glue::glue("{spec}.rda")),
         compress = "xz")
  }
