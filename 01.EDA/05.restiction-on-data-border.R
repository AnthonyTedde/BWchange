#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#++++++
# Restrict the domain of the data
#++++++
#
# 1. Based on physical measure (BW, DIM, MY)
# 2. From spectral analysis
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

rm(list = ls())
library(magrittr)
source(file = "helper/plot_desc.R")
source(file = here::here("globals", "globals-models.R"))
data("dataset_original")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Global variable for the whole file
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# Npcs <- 10
# GH_threshold <- 5


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Bodyweigh description
# Rules:
#  * Keep data with 350 <= BW <= 1000
#  * Keep data with 10 <= MY <= 60
#  * Keep data within a year of dim (< 365)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

dataset_cleaned_phs1 <- dataset_original %>%
  dplyr::filter(bodyweight >=350 & bodyweight <= 1000) %>%
  dplyr::filter(milk_yield >=10 & milk_yield <= 60) %>%
  dplyr::filter(dim <= 365)

plot_desc(dataset_cleaned_phs1)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Spectral analysis - Restrict domain base on spectral analysis
# Rules:
#  0. Preanalysis
#  1. PCA
#  2.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#


#++++++++++++++++++++++++++++++++++++#
## Do the species need to be separated?
#++++++++++++++++++++++++++++++++++++#

# dpin <- paste0("d", pin212_name)
#
# spectra_pca <- dataset_cleaned_phs1 %>%
#   dplyr::select(dplyr::all_of(dpin)) %>%
#   FactoMineR::PCA(graph = F)
#   # FactoMineR::PCA(quali.sup = 1)
#
# grp <- factor(dataset_cleaned_phs1$breed)
#
# factoextra::fviz_pca_ind(spectra_pca, label = "none", habillage = grp,
#                          alpha.ind = .00,
#                          addEllipses = T, ellipse.level = .95) +
#   ggplot2::coord_cartesian(xlim = c(-50, 50), ylim = c(-30, 30))+
#   ggsci::scale_fill_nejm(name = "Breed") +
#   ggsci::scale_color_nejm(name = "Breed") +
#   ggplot2::scale_shape(name = "Breed")
#
#
# spectra_tbl_grp <- dataset_cleaned_phs1 %>%
#   dplyr::group_by(breed)
#
# spectra_tbl_lst <- spectra_tbl_grp %>%
#   dplyr::group_split() %>%
#   setNames(dplyr::group_keys(spectra_tbl_grp) %>%
#              dplyr::pull())


## Compute PCA / group

# spectra_pca_lst <-  spectra_tbl_lst %>%
#   purrr::map(.f = function(tbl){
#       tbl %>%
#         dplyr::select(dplyr::all_of(dpin)) %>%
#         FactoMineR::PCA(graph = F, ncp = Npcs)
#   })
#
# pca_holstein <- spectra_pca_lst$Holstein
# pca_holstein$ind$coord
# factoextra::fviz_pca_ind(
#   pca_holstein,
#   label = "none",
#   habillage = factor(spectra_tbl_lst$Holstein$provider),
#   axes = c(2, 3)
#   )



## Compute Mahalanobis distance / group

# mahalanobis_lst <- spectra_pca_lst %>%
#   purrr::map(.f = function(lst){
#     lst$ind$coord %>%
#       mahalanobis(., center = colMeans(.), cov = cov(.))
#   })


## Divide by the number of PCs to get the GH-distance

# GH_lst <- mahalanobis_lst %>%
#   purrr::map(`/`, Npcs)

## We kept the entries with GH distance lower than `GH_threshold`
# (See Global variable at the beginning of the file for the threshold value)


# rowtokeep_lst <- GH_lst %>%
#   purrr::map(`<`, GH_threshold)

# Proportion of deleted data per breed

# rowtokeep_lst %>%
#   purrr::map(~mean(!.x))

## Graph the output with removed entries in red

# spectra_pca_plt_lst <- spectra_pca_lst %>%
#   purrr::imap(.f = function(mdl, key){
#     grp <- rowtokeep_lst[[key]] %>%
#       ifelse("Kept", "Removed") %>%
#       factor(levels = c("Removed", "Kept"))
#     factoextra::fviz_pca_ind(mdl, label = "none",
#                              habillage = grp,
#                              title = key, pointshape = 19) +
#       ggsci::scale_fill_nejm(name = "From GH > 5") +
#       ggsci::scale_color_nejm(name = "From GH > 5", drop = F)
#   })
# cowplot::plot_grid(plotlist = spectra_pca_plt_lst)


## Reduce the dataset

# dataset_cleaned <- spectra_tbl_lst %>%
#   purrr::map2(rowtokeep_lst, .f <- function(tbl, key){
#     tbl %>% dplyr::filter(key)
#   }) %>%
#   purrr::reduce(dplyr::bind_rows)
#
# plot_desc(dataset_cleaned)

# Test RMSE
# TODO put RMSE functions in a single file to source.

# data("pls_test")
# mdl <- pls_test
#
# rmse <- function(d, m){
#   tibble::tibble(
#     prd = predict(m, newdata = d, ncomp = 15) %>% drop,
#     tth = d$bodyweight
#   )  %>%
#     yardstick::rmse(truth = tth, estimate = prd)
# }
#
# rmse_byprovider <- function(d, m, by = "provider"){
#   d %>%
#     dplyr::group_by(provider) %>%
#     dplyr::group_map(.f = function(d, k){
#       tibble::tibble(
#         prd = predict(m, newdata = d, ncomp = 15) %>% drop,
#         tth = d$bodyweight
#       ) %>%
#         yardstick::rmse(truth = tth, estimate = prd) %>%
#         tibble::add_column(provider = k, .before = 1)
#     }) %>%
#     purrr::reduce(dplyr::bind_rows)
# }
#
# with(dataset_cleaned,{
#   table(breed, provider)
# })
# rmse_byprovider(dataset_cleaned, mdl)
#
#
# source(file = "helper/plot_desc2.R")
# plot_desc2(dataset_cleaned)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Save
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# No change.
# dataset_cleaned_phs1 <- dataset_cleaned
save(dataset_cleaned_phs1, file = "data/dataset_cleaned_phs1.rda")
