library(splines)
library(xgboost)
library(utilitR)
rm(list = ls())
data("training_data")
data("holstein_data")
data("testing_swiss_data")
data("stratified_kfolds")
data("pls_mod")
source("globals/globals-models.R")
library(ggplot2)


# ---------------------------------------------------------------------------- #
# -- PLS on spectra only -- ####
dpin212_name <- paste0("d", pin212_name)

pin_form <- paste(dpin212_name, collapse = " + ")
pin_form <- formula(paste("bodyweight", pin_form, sep = " ~ "))

pls_spectra_mod <- pls::mvr(pin_form,
                        data = training_data,
                        ncomp = 30,
                        scale = T,
                        method = "oscorespls")
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# -- Create X_pls_spectra_mat and corresponding X_pls_test_spectra_mat -- #
X_pls_spectra_mat <- df_to_DMatrix(
  plsmod = pls_spectra_mod,
  data = training_data,
  additional_columns = c("parity_ord", "dim", "milk_yield"),
  y_name = "bodyweight"
)

# Test
X_pls_spectra_test_mat <- df_to_DMatrix(
  plsmod = pls_spectra_mod,
  data = testing_swiss_data,
  additional_columns = c("parity_ord", "dim", "milk_yield"),
  y_name = "bodyweight"
)
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# -- Create X_full_pls -- #
X_pls_full_mat <- df_to_DMatrix(
  plsmod = pls_mod,
  data = training_data,
  threshold = 1,
  y_name = "bodyweight"
)

X_pls_full_test_mat <- df_to_DMatrix(
  plsmod = pls_mod,
  data = testing_swiss_data,
  threshold = 1,
  y_name = "bodyweight"
)
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# -- Create X_full_mat -- #
# ---------------------------------------------------------------------------- #
cols <- c("dim", "parity_ord", "milk_yield", paste0("d", pin212_name))
X_full_mat <- df_to_DMatrix(
  data = training_data,
  additional_columns = cols,
  y_name = "bodyweight"
)
X_full_test_mat <- df_to_DMatrix(
  data = testing_swiss_data,
  additional_columns = cols,
  y_name = "bodyweight"
)

# -- Train xgb -- ####
# Full  pls
train <- X_pls_full_mat
test <- X_pls_full_test_mat
# Only pls for spectra
train <- X_pls_spectra_mat
test <- X_pls_spectra_test_mat
# Full
train <- X_full_mat
test <- X_full_test_mat


# ---------------------------------------------------------------------------- #
# -- Linear XGB -- ####
linear_cv <- xgb.cv(
  data = train,
  nrounds = 2000, nthread = 5,
  folds = stratified_kfolds,
  metrics = list("rmse"),
  early_stopping_rounds = 50,
  # parameters
  updater = "coord_descent",
  # updater = "shotgun",
  eta = 1,
  lambda = 0, alpha = 0,
  feature_selector = "greedy", top_k = 10,
  # feature_selector = "thrifty", top_k = 4,
  # feature_selector = "shuffle",
  # colsample_bynode = .0,
  # max_depth = 6,
  # gamma = 20000,
  # min_child_weight = 3,
  # subsample = .5,
  booster = 'gblinear',
  objective = "reg:squarederror"
  # objective = "reg:gamma"
)

fit <- xgb.train(params = cv$params,
                 # data = X_pls_spectra_mat,
                 data = train,
                 nrounds = 2000)

# xgb.importance(model = fit)

sqrt(mean((predict(fit, newdata = train) - training_data$bodyweight)^2))
sqrt(mean((predict(fit, newdata = test) - testing_swiss_data$bodyweight)^2))
# -- Linear XGB -- #
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# --  Tree XGB -- ####

training_data_sub <- training_data[training_data$uid %in% holstein_data[
  !holstein_data$provider == "utrobe",
  "uid", drop = T
], ]
training_data_trobe <- training_data[training_data$uid %in% holstein_data[
  holstein_data$provider == "utrobe",
  "uid", drop = T
], ]

# -- Create X_pls_spectra_mat and corresponding X_pls_test_spectra_mat -- #
X_pls_spectra_sub_mat <- df_to_DMatrix(
  plsmod = pls_spectra_mod,
  data = training_data_sub,
  additional_columns = c("parity_ord", "dim", "milk_yield"),
  y_name = "bodyweight"
)
X_pls_spectra_trobe_mat <- df_to_DMatrix(
  plsmod = pls_spectra_mod,
  data = training_data_trobe,
  additional_columns = c("parity_ord", "dim", "milk_yield"),
  y_name = "bodyweight"
)

train <- X_pls_spectra_sub_mat
test <- X_pls_spectra_test_mat

tree_cv <- xgb.cv(
  data = train,
  nrounds = 100000, nthread = 5,
  # folds = stratified_kfolds,
  nfold = 10,
  # metrics = list("rmse"),
  early_stopping_rounds = 10,
  # -----------
  # parameters
  # -----------
  booster = 'gbtree', # The default and the one supported by parsnip
  max_depth = 6, # tree_depth
  eta = .01, # learning_rate [-10, 0, log10]
  colsample_bynode = .5, # mtry [0.1, 1] (Column subsampling)
  # colsample_bylevel = .5,
  # colsample_bytree = .4,
  subsample = .2, # sample_size [.1, 1] (Row subsampling)
  gamma = 300, # loss_reduction
  # min_child_weight = 1000, # min_split_loss [0, 300]
  lambda = 10000, # set_engine->lambda, dials->penalty_L2
  alpha = 0, # set_engine->alpha, dials->penalty_L1
  # num_parallel_tree = 5,
  # max_depth = 6,
  objective = "reg:squarederror"
  # objective = "reg:gamma"
)


fit <- xgb.train(params = tree_cv$params,
                 # data = X_pls_spectra_mat,
                 data = train,
                 # nrounds = 50000)
                 nrounds = tree_cv$best_iteration)

# xgb.importance(model = fit)

sqrt(mean((predict(fit, newdata = train) - training_data$bodyweight)^2))
# sqrt(mean((predict(fit, newdata = train) - training_data_sub$bodyweight)^2))
# sqrt(mean((predict(fit, newdata = X_pls_spectra_trobe_mat) - training_data_trobe$bodyweight)^2))
sqrt(mean((predict(fit, newdata = test) - testing_swiss_data$bodyweight)^2))

# --  XGB -- ####
# ---------------------------------------------------------------------------- #
