library(magrittr)

data("test_data_swiss")
data("train_data")
source("globals/globals-models.R")

# ------------------------------------------------------------------------------
# Globals
# ------------------------------------------------------------------------------

Npcs <- 10
GH_threshold <- 7
dpins <- paste0("d", pin212_name)


# ------------------------------------------------------------------------------
# Add new variable
# ------------------------------------------------------------------------------

train_data %<>%
  dplyr::mutate(
    lactation_stage_fct = dplyr::case_when(
      dim < 75 ~ "early",
      dim > 200 ~ "late",
      TRUE ~ "mid"
    ) %>% factor(labels = c("early", "mid", "late"))
  )


# ------------------------------------------------------------------------------
# Compute PCA
# ------------------------------------------------------------------------------

pca_lst <- train_data %>%
  dplyr::group_by(parity_fct, lactation_stage_fct) %>%
  dplyr::group_map(.f = function(dat, key){
    dat_to_pca <- dat %>%
      dplyr::select(dplyr::all_of(dpins))
    # TODO mixOmics
    pca <- FactoMineR::PCA(dat_to_pca, graph = F, ncp = Npcs)
    c <- pca$ind$coord
    dat %<>%
      dplyr::mutate(
        mahalanobis = mahalanobis(c, center = colMeans(c), cov = cov(c)),
        GH = mahalanobis / Npcs
      )
    key %<>%
      dplyr::mutate(pca = list(pca))
    list(dat = dat, fnc = key)
  },.keep = T)

train_data_augmented <- pca_lst %>%
  purrr::map(~.x$dat) %>%
  purrr::reduce(dplyr::bind_rows)

pca_trsfrm <- pca_lst %>%
  purrr::map(~.x$fnc) %>%
  purrr::reduce(dplyr::bind_rows)


# ------------------------------------------------------------------------------
# Transform the testing dataset
# ------------------------------------------------------------------------------

test_data_swiss_augmented <- test_data_swiss %>%
  dplyr::mutate(
    lactation_stage_fct = dplyr::case_when(
      dim < 75 ~ "early",
      dim > 200 ~ "late",
      TRUE ~ "mid"
    ) %>% factor(labels = c("early", "mid", "late"))
  ) %>%
  dplyr::group_by(parity_fct, lactation_stage_fct) %>%
  dplyr::group_map(.f = function(dat, key){
    pca_fn <- pca_trsfrm %>%
      dplyr::inner_join(key) %>%
      dplyr::pull(pca)
    coord <- predict(pca_fn, dat)[[1]]$coord
    dat %>%
      dplyr::mutate(
        mahalanobis = mahalanobis(coord,
                                  center = colMeans(coord),
                                  cov = cov(coord)),
        GH = mahalanobis / ncol(coord)
      )
  }, .keep = T) %>%
  purrr::reduce(dplyr::bind_rows)


tictoc::tic()
dpins <- paste0("d", pin212_name)
form <- paste(dpins, collapse = " + ") %>%
  paste("dim", "milk_yield", "dim:milk_yield",
        "poly(dim, 3)", "poly(milk_yield, 3)",
        ., sep = " + ") %>%
  paste("bodyweight", ., sep = " ~ ") %>% formula()
se <- vector(mode = "numeric", length = nrow(test_data_swiss))
for(i in 1:nrow(test_data_swiss)){
  row <- test_data_swiss_augmented[i, ]
  trn <- train_data_augmented %>%
    dplyr::inner_join(row[, c("parity_fct", "lactation_stage_fct")])
  idx <- order((trn$GH - row$GH)^2)[1:min(100, nrow(trn))]
  sub_train_data <- trn[idx, ]
  mdl <- pls::mvr(form, ncomp = 6, data = sub_train_data,
                  scale = T, center = T)
  prd <- predict(mdl, newdata = row, comps = 6, type = "response")
  se[i] <- (row$bodyweight - prd)^2
}
tictoc::toc()

sqrt(mean(se))
