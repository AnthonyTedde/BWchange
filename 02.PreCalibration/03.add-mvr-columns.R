# Simply no.



# rm(list = ls())
# library(magrittr)
# data("train_data")
# data("test_data")
# data("out_of_sample")
#
# # TODO 212 !!!
#
# dpin_form <- names(train_data) %>%
#   grep(pattern = "^dpin", x = ., value = T) %>%
#   paste(collapse = " + ")
#
# form <- paste("bodyweight", dpin_form, sep = " ~ ") %>%
#   formula
#
# mvr_mdl <- pls::mvr(formula = form, ncomp = 45, data = train_data,
#                     center = T, scale = T)
#
# ncomp <- (cumsum(mvr_mdl$Xvar /  mvr_mdl$Xtotvar) < .975) %>%
#   which %>%
#   seq_along()
#
# train_data %<>%
#   tibble::add_column(
#     predict(mvr_mdl, newdata = train_data, ncomp = ncomp, type = "scores") %>%
#       tibble::as_tibble() %>%
#       setNames(nm = paste0("Comp", stringr::str_pad(ncomp, width = 2, pad = "0")))
#   )
#
# test_data %<>%
#   tibble::add_column(
#     predict(mvr_mdl, newdata = test_data, ncomp = ncomp, type = "scores") %>%
#       tibble::as_tibble() %>%
#       setNames(nm = paste0("Comp", stringr::str_pad(ncomp, width = 2, pad = "0")))
#   )
#
# out_of_sample %<>%
#   tibble::add_column(
#     predict(mvr_mdl, newdata = out_of_sample, ncomp = ncomp, type = "scores") %>%
#       tibble::as_tibble() %>%
#       setNames(nm = paste0("Comp", stringr::str_pad(ncomp, width = 2, pad = "0")))
#   )
#
# save(train_data, file = "data/train_data.rda")
# save(test_data, file = "data/test_data.rda")
# save(out_of_sample, file = "data/out_of_sample.rda")
