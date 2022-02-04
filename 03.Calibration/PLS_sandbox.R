library(mgcv)
library(splines)

data("training_data")
data("holstein_data")
data("testing_swiss_data")
data("stratified_kfolds")

rmse <- function(md, dat, ...){
  pred <- predict(md, newdata = dat, ...)
  sqrt(mean((dat$bodyweight - pred)^2))
}


# ------------------------------------------------------------------------------
# Benchmark ####
# ------------------------------------------------------------------------------
lm_benchmark <- lm(bodyweight ~ milk_yield + dim + parity_ord,
                   data = training_data)
summary(lm_benchmark) #R2: 49.29
rmse(lm_benchmark, training_data)
rmse(lm_benchmark, testing_swiss_data)

lm_benchmark_splines <- lm(bodyweight ~
                             ns(milk_yield, df = 4) +
                             ns(dim, df = 4) + parity_ord,
                   data = training_data)
summary(lm_benchmark_splines) #R2 55.15
rmse(lm_benchmark_splines, training_data)
rmse(lm_benchmark_splines, testing_swiss_data)

lm_benchmark_gam <- gam(bodyweight ~ te(milk_yield, dim) + parity_ord,
                        family = inverse.gaussian(link = "log"),
                        data = training_data, method = "REML")
summary(lm_benchmark_gam) #R2 56.9
rmse(lm_benchmark_gam, training_data, type = "response")
rmse(lm_benchmark_gam, testing_swiss_data, type = "response")

plot(rstandard(lm_benchmark) ~ fitted(lm_benchmark))
plot(rstandard(lm_benchmark_splines) ~ fitted(lm_benchmark_splines))
plot(cooks.distance(lm_benchmark), type = "h")

plot(lm_benchmark)
plot(lm_benchmark_gam)

# Predicted vs. observed
plot(testing_swiss_data$bodyweight,
     predict(lm_benchmark_gam, newdata = testing_swiss_data, type = "response"),
     pch = 20,
     xlab = "Bodyweight observation", ylab = "Bodyweight prediction")
points(testing_swiss_data$bodyweight, col = "red",
     predict(lm_benchmark, newdata = testing_swiss_data) |> unname(),
     pch = 20,
     xlab = "Bodyweight observation", ylab = "Bodyweight prediction")

# predicted vs. dim (gam)
plot(testing_swiss_data$dim,
     predict(lm_benchmark, newdata = testing_swiss_data, type = "response"),
     pch = 20,
     xlab = "Days in milk", ylab = "Bodyweight prediction")
points(testing_swiss_data$dim,
       testing_swiss_data$bodyweight |> unname(),
       col = "red",
       pch = 20,
       xlab = "Days in milk", ylab = "Bodyweight observation")

# predicted vs. dim (gam)
plot(training_data$dim,
     predict(lm_benchmark_gam, newdata = training_data, type = "response"),
     pch = 20,
     xlab = "Days in milk", ylab = "Bodyweight prediction")
points(training_data$dim,
       training_data$bodyweight |> unname(),
       col = "red",
       pch = 20,
       xlab = "Days in milk", ylab = "Bodyweight observation")

# predicted vs. dim (ns)
plot(training_data$dim,
     predict(lm_benchmark_splines, newdata = training_data, type = "response"),
     pch = 20,
     xlab = "Days in milk", ylab = "Bodyweight prediction")
points(training_data$dim,
       training_data$bodyweight |> unname(),
       col = "red",
       pch = 20,
       xlab = "Days in milk", ylab = "Bodyweight observation")
# ------------------------------------------------------------------------------
# Partial least square ####
# ------------------------------------------------------------------------------

dpin <- grep("^dpin", names(training_data), value = T)
form <- paste(dpin, collapse = " + ")
form <- paste("bodyweight", form, sep = "~") |> formula()
pls_mod <- pls::mvr(form, data = training_data,
                    method = "oscorespls",
                    scale = T, center = T,
                    validation = "CV",
                    segments = stratified_kfolds)
# Compute oneSE
predictions <- drop(pls_mod$validation$pred)
rmse_cv_full <- sqrt(colMeans((predictions - training_data$bodyweight)^2))
bst <- predictions[, pls_mod$ncomp]
rmse_split <- sapply(stratified_kfolds, FUN = function(x){
  sqrt(mean((bst[x] - training_data[x, "bodyweight", drop = T])^2))
})
bst_se <- sd(rmse_split)
bst_oneSE <- sqrt(mean((bst - training_data$bodyweight)^2)) + bst_se
ncomp_onese <- min(which(rmse_cv_full < bst_oneSE))

training_data_pls <- cbind(
  training_data,
  structure(
    data.frame(predict(pls_mod, ncomp = 1:ncomp_onese, type = "scores")),
    names = paste0("cmp",
                   stringr::str_pad(1:ncomp_onese, width = 2, pad = 0))
  )
)

testing_swiss_data_pls <- cbind(
  testing_swiss_data,
  structure(
    data.frame(predict(pls_mod, newdata = testing_swiss_data,
                       ncomp = 1:ncomp_onese, type = "scores")),
    names = paste0("cmp",
                   stringr::str_pad(1:ncomp_onese, width = 2, pad = 0))
  )
)

## Gam
cor_var <- c("milk_yield", "dim",
             grep("^cmp", names(training_data_pls), value = T))
cor(training_data_pls[, cor_var])

pls_mir_var <- grep("^cmp", names(training_data_pls), value = T) |>
  paste(collapse = " + ")
left_hand <- paste("te(milk_yield, dim)", "parity_ord", pls_mir_var,
                   sep = " + ")
form <- paste("bodyweight", left_hand, sep = " ~ ") |> formula()
pls_gam <- gam(form,
               family = inverse.gaussian(link = "log"),
               data = training_data_pls, method = "REML")
summary(pls_gam) #R2 56.9
rmse(pls_gam, training_data_pls, type = "response")
rmse(pls_gam, testing_swiss_data_pls, type = "response")


# Full pls
h <- holstein_data[!(holstein_data$provider %in% c("seenorest", "swiss", "utrobe")), ]
dim(training_data)
dim(h)
training <- training_data[training_data$uid %in% h$uid, ]
training <- training_data
dim(training)
pls_mir_var <- grep("^dpin", names(training), value = T) |>
  paste(collapse = " + ")
left_hand <- paste("ns(milk_yield, df = 4)",
                   "ns(dim, df = 4)",
                   "parity_ord", pls_mir_var,
                   sep = " + ")
form <- paste("bodyweight", left_hand, sep = " ~ ") |> formula()

pls::pls.options(parallel = 2)
pls_full <- pls::mvr(form, data = training,
                     method = "oscorespls",
                     # ncomp = 50,
                     scale = T, center = T,
                     validation = "CV",
                     segments = stratified_kfolds)

predictions <- drop(pls_full$validation$pred)
rmse_cv_full <- sqrt(colMeans((predictions - training$bodyweight)^2))
bst <- predictions[, pls_full$ncomp]
rmse_split <- sapply(stratified_kfolds, FUN = function(x){
  sqrt(mean((bst[x] - training[x, "bodyweight", drop = T])^2))
})
bst_se <- sd(rmse_split)
bst_oneSE <- sqrt(mean((bst - training$bodyweight)^2)) + bst_se/2
ncomp_onese <- min(which(rmse_cv_full < bst_oneSE))

# Using PRESS
which.min(pls_full$validation$PRESS / pls_full$validation$PRESS0)
plot(pls_full$validation$PRESS / pls_full$validation$PRESS0, x = 1:222,
     type = "l")
abline(v = 116)

# Using adjusted wold's R criterion
class(pls_full$validation$PRESS)
R = tail(drop(pls_full$validation$PRESS), -1) / head(drop(pls_full$validation$PRESS), -1)
which.min(R < 0.999)
plot(R, x = 2:222, type = "l")

summary(pls_full) #R2 56.9
rmse(pls_full, training, ncomp = 18, type = "response")
rmse(pls_full, testing_swiss_data, ncomp = 18, type = "response")


# --------------
# MCCV
# --------------

library(pls)
training <- training_data

pls_mir_var <- grep("^dpin", names(training), value = T) |>
  paste(collapse = " + ")
left_hand <- paste("ns(milk_yield, df = 4)",
                   "ns(dim, df = 4)",
                   "parity_ord", pls_mir_var,
                   sep = " + ")
form <- paste("bodyweight", left_hand, sep = " ~ ") |> formula()

# Preamble data
N <- 1000
set.seed(1010)
# for loop start here
pls_lst <- list()
n <- nrow(training)
RMSE_v <- matrix(nrow = 0, ncol = 222)
RMSE_c <- matrix(nrow = 0, ncol = 222)

for(i in 1:N){
  message(paste("Loop", i))
  # segment <- list(
  #   uid_validation = sample(1:n, size = round(n * .10))
  # )
  sset = sample(1:n, size = round(n * .40))
  # uid_calibration <- setdiff(training$uid, uid_validation)
  # pls_lst[[i]] <- pls::mvr(
  pls_lst <- pls::mvr(
    form, data = training, method = pls::pls.options()$plsralg,
    subset = sset,
    center = T, scale = T,
    validation = "none"
  )
  # v <- pls_lst[[i]]$validation$segments$uid_validation
  vsset <- setdiff(1:nrow(training), sset)
  csset <- sset
  # v <- 1:nrow(training)
  p_v <- predict(pls_lst, newdata = training[vsset, ], type = "response") |> drop()
  p_c <- fitted(pls_lst) |> drop()
  PRESS_v <- (p_v - training[vsset, "bodyweight", drop = T])^2
  PRESS_c <- (p_c - training[csset, "bodyweight", drop = T])^2
  RMSE_v <- rbind(RMSE_v, sqrt(colSums(PRESS_v) / length(vsset)) / N)
  RMSE_c <- rbind(RMSE_c, sqrt(colSums(PRESS_c) / length(csset)) / N)
}

#for pls_lst[[1]]
# for (i in 1:N){
#   message(paste("Loop", i))
#   # v <- pls_lst[[i]]$validation$segments$uid_validation
#   vsset <- setdiff(1:nrow(training), as.integer(rownames(pls_lst[[i]]$model)))
#   csset <- as.integer(rownames(pls_lst[[i]]$model))
#   # v <- 1:nrow(training)
#   p_v <- predict(pls_lst[[i]], newdata = training[vsset, ], type = "response") |> drop()
#   p_c <- fitted(pls_lst[[i]]) |> drop()
#   PRESS_v <- (p_v - training[vsset, "bodyweight", drop = T])^2
#   PRESS_c <- (p_c - training[csset, "bodyweight", drop = T])^2
#   RMSE_v <- rbind(RMSE_v, sqrt(colSums(PRESS_v) / length(vsset)) / N)
#   RMSE_c <- rbind(RMSE_c, sqrt(colSums(PRESS_c) / length(csset)) / N)
# }
RMSE_v_sum <- colSums(RMSE_v)
RMSE_c_sum <- colSums(RMSE_c)
ncomp_v <- which.min(RMSE_v_sum)
ncomp_c <- which.min(RMSE_c_sum)
plot(RMSE_c_sum[1:100], type = "l", col = "blue")
lines(RMSE_v_sum[1:100], col = "red")
abline(v = ncomp_v, lty = 2)

training <- training[training$bodyweight < 630, ]

pls_final <- pls::mvr(
  form, data = training, method = pls::pls.options()$plsralg,
  # ncomp = ncomp_v,
  ncomp = 100,
  center = T, scale = T,
  validation = "none"
)

rmse(pls_final, training, ncomp = ncomp_v, type = "response")
rmse(pls_final, testing_swiss_data, ncomp = ncomp_v, type = "response")

summary(pls_final)


RMSE_pls <- sqrt(pls_full$validation$PRESS / n) |> drop()
plot(RMSE_pls, type = "l")


rmsecv <- sqrt(pls_lst[[1]]$validation$PRESS / (n))
sqrt(pls_lst[[1]]$validation$PRESS / n)
# rmsecv <- sqrt(pls_full$validation$PRESS / n)
which.min(rmsecv)
# plot(x = 1:222, y = rmsecv)
















