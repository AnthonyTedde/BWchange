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
data("dataset_original")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Global variable for the whole file
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

Npcs <- 10
GH_threshold <- 5


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
  dplyr::filter(dim <= 364)

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

spectra_pca <- dataset_cleaned_phs1 %>%
  dplyr::select(dplyr::starts_with("dpin")) %>%
  FactoMineR::PCA(graph = F)
  # FactoMineR::PCA(quali.sup = 1)

grp <- factor(dataset_cleaned_phs1$breed)

factoextra::fviz_pca_ind(spectra_pca, label = "none", habillage = grp,
                         alpha.ind = .00,
                         addEllipses = T, ellipse.level = .95) +
  ggplot2::coord_cartesian(xlim = c(-50, 50), ylim = c(-30, 30))+
  ggsci::scale_fill_nejm(name = "Breed") +
  ggsci::scale_color_nejm(name = "Breed") +
  ggplot2::scale_shape(name = "Breed")

factoextra::fviz_pca_biplot(spectra_pca, label = "var", habillage = grp,
                         alpha.ind = .1,
                         addEllipses = T, ellipse.level = .95,
                         select.var = list(contrib = 5)) +
  ggsci::scale_fill_nejm(name = "Breed") +
  ggsci::scale_color_nejm(name = "Breed") +
  ggplot2::scale_shape(name = "Breed")


#++++++++++++++++++++++++++++++++++++#
# Separate by breed
#++++++++++++++++++++++++++++++++++++#

## Create group
spectra_tbl_grp <- dataset_cleaned_phs1 %>%
  dplyr::group_by(breed)

spectra_tbl_lst <- spectra_tbl_grp %>%
  dplyr::group_split() %>%
  setNames(dplyr::group_keys(spectra_tbl_grp) %>% dplyr::pull())

## Compute PCA / group
spectra_pca_lst <-  spectra_tbl_lst %>%
  purrr::map(.f = function(tbl){
      tbl %>%
        dplyr::select(dplyr::starts_with("dpin")) %>%
        FactoMineR::PCA(graph = F, ncp = Npcs)
  })

## Compute Mahalanobis distance / group
mahalanobis_lst <- spectra_pca_lst %>%
  purrr::map(.f = function(lst){
    lst$ind$coord %>%
      mahalanobis(., center = colMeans(.), cov = cov(.))
  })
## Divide by the number of PCs to get the GH-distance
GH_lst <- mahalanobis_lst %>%
  purrr::map(`/`, Npcs)

## We kept the entries with GH distance lower than `GH_threshold`
# (See Global variable at the beginning of the file for the threshold value)
rowtokeep_lst <- GH_lst %>%
  purrr::map(`<`, GH_threshold)

# Proportion of deleted data per breed
rowtokeep_lst %>%
  purrr::map(~mean(!.x))

## Graph the output with removed entries in red
spectra_pca_plt_lst <- spectra_pca_lst %>%
  purrr::imap(.f = function(mdl, key){
    grp <- rowtokeep_lst[[key]] %>%
      ifelse("Kept", "Removed") %>%
      factor(levels = c("Removed", "Kept"))
    factoextra::fviz_pca_ind(mdl, label = "none",
                             habillage = grp,
                             title = key, pointshape = 19) +
      ggsci::scale_fill_nejm(name = "From GH > 5") +
      ggsci::scale_color_nejm(name = "From GH > 5", drop = F)
  })
cowplot::plot_grid(plotlist = spectra_pca_plt_lst)

## Reduce the dataset
dataset_cleaned <- spectra_tbl_lst %>%
  purrr::map2(rowtokeep_lst, .f <- function(tbl, key){
    tbl %>% dplyr::filter(key)
  }) %>%
  purrr::reduce(dplyr::bind_rows)

plot_desc(dataset_cleaned)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Save
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

save(dataset_cleaned, file = "data/dataset_cleaned.rda")
