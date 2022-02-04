# Run the 06.




# ################################################################################
# #######
# # Flag outliers values
# #######
# #
# # 1. Linear model (OLS)
# # 2. Flag potential outliers based on the Cook's distance
# #
# ################################################################################
#
# rm(list = ls())
# library(magrittr)
# data("train_data")
#
# ################################################################################
# # Global variable for the whole file
# ################################################################################
#
# # cooksd_threshold <- 4 / (nrow(dataset_cleaned))
#
#
# ################################################################################
# # Linear model
# ################################################################################
#
# #######################################
# # Create the formula
# ######################################
#
# dpin <- names(train_data) %>%
#   grep(pattern = "^dpin", value = T) %>%
#   paste(collapse = " + ")
#
# lm_formula <- formula(glue::glue(
#   "bodyweight ~ parity + milk_yield + dim + {dpin} +
#   poly(milk_yield, degree = 2) + poly(dim, degree = 4)+
#   test_MIR_season"
# ))
#
#
# #######################################
# ## Calibrate the model and assess the
# # Coefficients significance
# ######################################
#
# lm_full <- lm(lm_formula, data = train_data)
# summary(lm_full)
#
# influancial <- influence.measures(lm_full)
#
# idxToRemove <- influancial$is.inf %>%
#   rowSums() %>% which(x = . > 1)
#
# length(idxToRemove) / nrow(train_data)
#
# train_data <- train_data[-idxToRemove, ]
#
# save(train_data, file = "data/train_data.rda")
