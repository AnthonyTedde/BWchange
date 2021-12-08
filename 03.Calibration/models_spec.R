library(magrittr)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Linear based models ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Ordinary least square
#-+-+-+-+-+-+-+-+-+-+-+-
lm_spec <- parsnip::linear_reg() %>%
  parsnip::set_engine("lm")

# Which hyper parameters could be tuned ?
tune::tunable(lm_spec) # No one... what a shame XD

# Generalized Linear Model
#-+-+-+-+-+-+-+-+-+-+-+-+-+-
glm_spec <- parsnip::linear_reg(penalty = tune::tune(),
                                mixture = tune::tune()) %>%
  parsnip::set_engine(engine = "glmnet", family = Gamma)

# Which hyper parameters could be tuned ?
tune::tunable(glm_spec)
# * penalty: Level of regularization
# * mixture: Proportion of Lasso (mixture = 1) and ridge (mixture = 0)

# Not hyper parameter, but should be set:
# * What about family ? --> Gaussian as set by default.

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Trees based methods ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Random forest
#-+-+-+-+-+-+-+-
rf_spec <- parsnip::rand_forest(
  mtry = tune::tune(),
  trees = ntrees,
  min_n = tune::tune()
) %>%
  parsnip::set_mode(mode = "regression") %>%
  parsnip::set_engine(engine = "ranger")

# Which hyper parameters could be tuned ?
tune::tunable(rf_spec)
# * mtry: Number of selected predictors
# * trees: number of trees (see file globals/globals-models.R)
# * min_n: Minimal node side (below this number a node can't be split)

# XgBoost
#-+-+-+-+-
xgb_spec <- parsnip::boost_tree(
  trees = ntrees,
  tree_depth = tune::tune("xgb_tree_depth"),
  min_n = tune::tune("xgb_min_n"),
  loss_reduction = tune::tune(),
  sample_size = tune::tune(),
  mtry = tune::tune(),
  learn_rate = tune::tune(),
) %>%
  parsnip::set_engine("xgboost") %>%
  parsnip::set_mode(mode = "regression")

# Which hyper parameters could be tuned ?
tune::tunable(xgb_spec)
# * trees: number of trees (see file globals/globals-models.R)
# * tree_depth: Maximum depth of the tree
# * min_n: Minimal node side (below this number a node can't be split)
# * loss_reduction: Minimal reduction achieved in the loss function to split further
# * sample_size: Proportion of data exposed to the fitting routine
# * mtry: Number of selected predictors
# TODO * learn_rate: euuuh.....
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * stop_iter: The number of iteration without improvement before stropping


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Distance based methods ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# K nearest neighbor
#-+-+-+-+-+-+-+-+-+-+-
knn_spec <- parsnip::nearest_neighbor(
  neighbors = tune::tune(),
  weight_func = tune::tune()
) %>%
  parsnip::set_mode(mode = "regression") %>%
  parsnip::set_engine(engine = "kknn")

# Which hyper parameters could be tuned ?
tune::tunable(knn_spec)
# * neighbors: k (number of neighbors to consider)
# * weight_func: Kernel function
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * dist_power: related to the minkowski distance


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Support vector machine ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Linear SVM
#-+-+-+-+-+-+-+-+-+-+-
linearSVM_spec <- parsnip::svm_linear(
  cost = tune::tune(),
  margin = tune::tune()
) %>%
  parsnip::set_mode("regression") %>%
  parsnip::set_engine("LiblineaR")

# Which hyper parameters could be tuned ?
tune::tunable(linearSVM_spec)
# * cost: Cost of predicting a sample outside (kind of regularization parameter)
# * margin: epsilon

# Polynomial SVM
#-+-+-+-+-+-+-+-+-+-+-
polySVM_spec <- parsnip::svm_poly(
  cost = tune::tune(),
  margin = tune::tune(),
  degree = tune::tune(),
  scale_factor = tune::tune()
) %>%
  parsnip::set_mode("regression") %>%
  parsnip::set_engine("kernlab")

# Which hyper parameters could be tuned ?
tune::tunable(polySVM_spec)
# * cost: Cost of predicting a sample outside (kind of regularization parameter)
# * margin: epsilon
# * degree: Polynomial degree
# * scale_factor: Polynomial scaling factor

# Radial basis SVM
#-+-+-+-+-+-+-+-+-+-+-
rbfSVM_spec <- parsnip::svm_rbf(
  cost = tune::tune(),
  margin = tune::tune(),
  rbf_sigma = tune::tune()
) %>%
  parsnip::set_mode("regression") %>%
  parsnip::set_engine("kernlab")
# Which hyper parameters could be tuned ?
tune::tunable(rbfSVM_spec)
# * cost: Cost of predicting a sample outside (kind of regularization parameter)
# * margin: epsilon
# * rbf_sigma: # for the radial basis function


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Save ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

save(lm_spec,
     file = here::here("data", "lm_spec.rda"),
     compress = "xz")
save(glm_spec,
     file = here::here("data", "glm_spec.rda"),
     compress = "xz")
save(rf_spec,
     file = here::here("data", "rf_spec.rda"),
     compress = "xz")
save(xgb_spec,
     file = here::here("data", "xgb_spec.rda"),
     compress = "xz")
save(knn_spec,
     file = here::here("data", "knn_spec.rda"),
     compress = "xz")
save(linearSVM_spec,
     file = here::here("data", "linearSVM_spec.rda"),
     compress = "xz")
save(polySVM_spec,
     file = here::here("data", "polySVM_spec.rda"),
     compress = "xz")
save(rbfSVM_spec, file = here::here("data", "rbfSVM_spec.rda"),
     compress = "xz")
