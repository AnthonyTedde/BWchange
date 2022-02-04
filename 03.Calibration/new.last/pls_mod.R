library(splines)
rm(list = ls())
data("training_data")
data("testing_swiss_data")
data("stratified_kfolds")
source("globals/globals-models.R")

# -- Global models -- ####
dpin212_name <- paste0("d", pin212_name)

benchmark_form <- paste("ns(milk_yield, df = 4)",
                        "ns(dim, df = 4)",
                        "parity_ord",
                        sep = " + ")
pin_form <- paste(dpin212_name, collapse = " + ")
X_form <- paste(benchmark_form, pin_form, sep = " + ")
pls_global_form <- formula(paste("bodyweight", X_form, sep = " ~ "))

pls_mod <- pls::mvr(
  formula = pls_global_form, data = training_data,
  scale = T, center = T, validation = "CV",
  segments = stratified_kfolds
)

# find best model

cross_validated_pred <- data.frame(
  drop(pls_mod$validation$pred)
)

# TODO: oneSE selection. -> Make it a function.
# TODO RMSE function
rmse_cv <- sqrt(
  colMeans((cross_validated_pred - training_data$bodyweight)^2)
)
rmse_calib <- sqrt(
  colMeans(residuals(pls_mod)^2)
)

# Best
bst <- drop(cross_validated_pred[, pls_mod$ncomp, ])

RMSE_cv_by_split <- sapply(stratified_kfolds, FUN = function(x){
  sqrt(mean((bst[x] - training_data[x, "bodyweight", drop = T])^2))
})
se <- sd(RMSE_cv_by_split)

onese_comp <- min(which(rmse_cv < rmse_cv_bst + se))


err <- drop(predict(pls_mod, testing_swiss_data, ncomp = onese_comp)) -
  testing_swiss_data$bodyweight
sqrt(mean(err^2))


save(pls_mod, file = "data/pls_mod.rda")
