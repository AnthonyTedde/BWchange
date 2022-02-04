library(xgboost)
library(ggplot2)
S <- c(1, 2)
R <- matrix(c(1, .8, .8, 1), ncol = 2)
C <- diag(S) %*% R %*% diag(S)
train <- MASS::mvrnorm(n = 1000, mu = c(0, 0), Sigma = C)
train <- structure(as.data.frame(train),
                   names = c("y", "x"))

var(train[, 2])
var(train[, 1])
cor(train)

lm.mod <- lm(y ~ x, data = train)
summary(lm.mod)

train_xgb <- xgb.DMatrix(as.matrix(train[, "x", drop = F]),
                         label = train[, "y"])

linear_cv <- xgb.cv(
  data = train_xgb,
  nrounds = 2000, nthread = 5,
  nfold = 5,
  metrics = list("rmse"),
  early_stopping_rounds = 10,
  # parameters
  # updater = "coord_descent",
  updater = "shotgun",
  eta = 1,
  lambda = 0, alpha = 0,
  # feature_selector = "greedy", top_k = 10,
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

fit <- xgb.train(params = linear_cv$params,
                 data = train_xgb,
                 nrounds = 1)

cor(train$y, predict(fit, newdata = train_xgb))^2


# ---------------------------------------------------------------------------- #
# -- gbtree -- ####
tree_cv <- xgb.cv(
  data = train_xgb,
  nrounds = 500, nthread = 5,
  nfold = 5,
  metrics = list("rmse"),
  early_stopping_rounds = 5,
  # -----------
  # parameters
  # -----------
  booster = 'gbtree', # The default and the one supported by parsnip
  max_depth = 5, # tree_depth
  eta = .5, # learning_rate [-10, 0, log10]
  colsample_bynode = .7, # mtry [0.1, 1] (Column subsampling)
  # colsample_bylevel = .5,
  # colsample_bytree = .9,
  subsample = .7, # sample_size [.1, 1] (Row subsampling)
  gamma = 0, # loss_reduction
  min_child_weight = 10, # min_split_loss [0, 300]
  lambda = .5, # set_engine->lambda, dials->penalty_L2
  alpha = 000, # set_engine->alpha, dials->penalty_L1
  # num_parallel_tree = 5,
  # max_depth = 6,
  # objective = "reg:squaredlogerror"
  objective = "reg:squarederror"
  # objective = "reg:gamma"
)


fit <- xgb.train(params = tree_cv$params,
                 # data = X_pls_spectra_mat,
                 data = train_xgb,
                 nrounds = 2)
cor(train$y, predict(fit, newdata = train_xgb))^2

train$y_hat <- predict(fit, newdata = train_xgb)

ggplot(data = train, mapping = aes(x = x, y = y)) +
  geom_point() +
  geom_line(mapping = aes(y = y_hat), color = "red")

# -- gbtree -- #
# ---------------------------------------------------------------------------- #
