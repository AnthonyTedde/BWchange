library(splines)
rm(list = ls())
data("training_data")
data("holstein_data")
data("testing_swiss_data")
data("stratified_kfolds")
source("globals/globals-models.R")

# -- Global models -- ####
dpin212_name <- paste0("d", pin212_name)

pin_form <- paste(dpin212_name, collapse = " + ")
X_form <- paste(pin_form, sep = " + ")
pls_global_form <- formula(paste("bodyweight", X_form, sep = " ~ "))

pls_mod <- pls::mvr(
  formula = pls_global_form, data = training_data,
  scale = T, center = T, validation = "CV",
  method = "oscorespls",
  segments = stratified_kfolds
)

cumvar <- cumsum(pls_mod$Xvar / pls_mod$Xtotvar)
cumvar[cumvar > .99][1]

uid <- c(training_data$uid, testing_swiss_data$uid)
final_dataset_python <- holstein_data[holstein_data$uid %in% uid, ]
dim(final_dataset_python)

scores <- predict(pls_mod, final_dataset_python, ncomp = 1:20,
                  type = "scores") %>%
  tibble::as_tibble() %>%
  setNames(nm = paste0("pls", stringr::str_pad(1:20, width = 2, pad = "0")))

class(scores)
dim(scores)

final_dataset_python <- cbind(final_dataset_python, scores)
dim(final_dataset_python)

feather::write_feather(final_dataset_python,
                       path = "data/final_dataset_python.feather")



