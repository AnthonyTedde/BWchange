rm(list = ls())
library(magrittr)
#
data("training_data")
data("holstein_data")
data("testing_swiss_data")
data("pls_mod")

dataset_train_full <- holstein_data[
  holstein_data$uid %in% training_data$uid,
]
dataset_test_full <- holstein_data[
  holstein_data$uid %in% testing_swiss_data$uid,
]


model <- pls_mod
# Onese best model:
ncomp <- 15


# Test performance of the model ####
rmse <- function(dat){
}

# rmse(training_data)
# rmse(out_of_sample_mir)
# rmse(test_data_seenorest)

# Create summary statistics datasets ####
out_of_sample_bw_count <- dataset_test_full %>%
  # ici
  dplyr::filter(dim < 365 & parity_full < 4) %>%
  # dplyr::filter(dim < 100) %>%
  dplyr::group_by(parity_full, expl_id, an_id,  name = "nBW") %>%
  dplyr::summarise(
    nBW = dplyr::n(),
    min_dim = min(dim)
  ) %>%
  dplyr::arrange(desc(nBW), min_dim) %>%
  dplyr::ungroup()

out_of_sample_mir <- dataset_test_full %>%
  dplyr::inner_join(out_of_sample_bw_count)

# ------------------------------------------------------------------------------
# Out of sample diff
# ------------------------------------------------------------------------------

out_of_sample_diff_mir <- out_of_sample_mir %>%
  tibble::rowid_to_column() %>%
  dplyr::group_by(an_id, expl_id, parity_full) %>%
  dplyr::group_map(.f = function(x, y){
    x %<>%
      dplyr::arrange(dim) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        key_next = ifelse(
          any(seq(from=dim+25,
                  to=dim+35,
                  by=1) %in% x$dim),
          x$rowid[which.min(abs(x$dim - (dim + 30)))],
          NA
          )
      ) %>% dplyr::ungroup()
  }, .keep = T) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::arrange(rowid)

out_of_sample_diff_mir %<>%
  dplyr::mutate(
    .pred = predict(model, ncomp = ncomp,
                    newdata = out_of_sample_diff_mir,
                    type = "response") %>% drop
  )


out_of_sample_diff_mir <- out_of_sample_diff_mir %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    dim_next = ifelse(
      !is.na(key_next),
      out_of_sample_diff_mir[[key_next, "dim"]], NA),
    bodyweight_next = ifelse(
      !is.na(key_next),
      out_of_sample_diff_mir[[key_next, "bodyweight"]], NA),
    .pred_next = ifelse(
      !is.na(key_next),
      out_of_sample_diff_mir[[key_next, ".pred"]], NA)
  ) %>%
  tidyr::drop_na() %>%
  dplyr::ungroup() %>%
  dplyr::select(an_id, parity_full,
                dim, bodyweight, .pred,
                dim_next, bodyweight_next, .pred_next)

out_of_sample_bwc_mir <- out_of_sample_diff_mir %>%
  dplyr::mutate(
    bwc = bodyweight_next - bodyweight,
    # bwc_roc = (bodyweight_next - bodyweight) / bodyweight,
    bwcr = bodyweight_next / bodyweight,
    bw0 = bodyweight,
    bwn = bodyweight_next,
    bwcp = .pred_next - .pred,
    # bwcp_roc = (.pred_next - .pred) / .pred,
    bwcpr = .pred_next/.pred,
    bwp0 = .pred,
    bwpn = .pred_next,
  ) %>%
  dplyr::select(an_id, parity_full, dim, dim_next,
                bw0, bwn, bwc, bwp0, bwpn, bwcp, bwcr, bwcpr)

out_of_sample_bwc_mir %>%
  dplyr::arrange(bwc_roc)

d <-  out_of_sample_bwc_mir %>%
  dplyr::filter(dim < 50)
d %>%
  ggplot2::ggplot(ggplot2::aes(x=bwc, y=bwcp)) +
  ggplot2::scale_x_continuous(breaks = seq(-80, 80, by = 5)) +
  ggplot2::scale_y_continuous(breaks = seq(-80, 80, by = 5)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::geom_abline(intercept = 0, slope = 1) +
  # coord_fixed(ratio = 1,
  #             xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  ggplot2::annotate("rect", xmin = -60, xmax = -25, ymin = -60, ymax = -25,
                    alpha = .5,fill = "green") +
  ggplot2::annotate("rect", xmin = -60, xmax = -25, ymin = -25, ymax = 0,
                    alpha = .5,fill = "blue") +
  ggplot2::annotate("rect", xmin = -60, xmax = -25, ymin = 0, ymax = 60,
                    alpha = .5,fill = "red") +
  ggplot2::annotate("rect", xmin = 0, xmax = 70, ymin = -60, ymax = -25,
                    alpha = .5,fill = "orange")
with(out_of_sample_bwc_mir, cor(bwc, bwcp)^2)

m <- lm(bwc ~ bwcp, data = d)
summary(m)
rev(sort(cooks.distance(m)))
d[7, ]


