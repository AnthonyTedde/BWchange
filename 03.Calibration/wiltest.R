library(bodyweight.data)
library(magrittr)

## TODO: Add all variables to the initial dataset.
## DONE: Check the computation of the drift correction
## TODO: Unsmooth data from bodyweight.data
## TODO: Rerun all script and explicitely change the dpin ton consider.


# Data ####
data("out_of_sample_mir")
data("out_of_sample_bw")
data("train_data")
data("test_data")
data("BW_seenorest")

# mdl ####
data("pls_lst")

# Global variable ####
model <- pls_lst$HSO_all

# Test performance of the model ####
rmse <- function(dat){
  tibble::tibble(
    pred = predict(model, newdata = dat),
    obs = dat$bodyweight
  ) %>%
    yardstick::rmse(truth = obs, estimate = pred)
}

rmse(test_data)
rmse(train_data)
rmse(out_of_sample_mir)

# Create summary statistics datasets ####
out_of_sample_bw_count <- out_of_sample_bw %>%
  dplyr::filter(dim < 365 & parity_full < 4) %>%
  dplyr::count(parity_full, expl_id, an_id,  name = "nBW") %>%
  dplyr::arrange(desc(nBW))

out_of_sample_mir_count <- out_of_sample_mir %>%
  dplyr::filter(dim < 365 & parity_full < 4) %>%
  dplyr::count(parity_full, expl_id, an_id, parity_full, name = "nMIR") %>%
  dplyr::arrange(desc(nMIR)) %>%
  dplyr::filter(nMIR > 9)

out_of_sample_merged_count <- dplyr::inner_join(
  out_of_sample_bw_count,
  out_of_sample_mir_count
)


# Function for ................ ####
# dataset <- out_of_sample_mir %>%
#   dplyr::inner_join(out_of_sample_merged_count)
# mdl <- model

augment_dataframe <- function(dataset, mdl){
  if(!missing(mdl)){
    dataset %<>%
      dplyr::mutate(bodyweight = predict(mdl, dataset))
  }
  dataset %>%
    # Computation of the regression line by animal and parity.
    dplyr::group_by(an_id, expl_id, parity_full) %>%
    dplyr::group_map(.f = function(dat, key){
    ##################################################
    # Estimate k
    max_wil <- 40
    sortiewiltest3 <- array(0, dim = c(max_wil, 8))
    coefwil=0.1
    for (j in 1:max_wil){
      coefwil<-coefwil-0.005
      dat %<>%
        dplyr::mutate(
          wil = exp(as.integer(dim) * coefwil)
        )
      reg <- lm(bodyweight~ dim + wil, data=dat)
      summaryreg <- summary(reg)
      nobs=nrow(dat)
      RMSE=sqrt(mean(summaryreg$residuals^2))

      final<-data.frame(coefwil,
                        nobs,
                        summaryreg$r.squared,
                        summaryreg$adj.r.squared,
                        RMSE,
                        reg$coefficients[1],
                        reg$coefficients[2],
                        reg$coefficients[3])
      sortiewiltest3[j,]<-as.matrix(final)
    }
    # diffR<-array(0,dim=c(max_wil))
    # for (j in 2:max_wil){
    #   diffR[j]=sortiewiltest3[j,3]-sortiewiltest3[j-1,3]
    # }
    # j=2
    # while(diffR[j]>0.001 & j < max_wil){
    #   j=j+1
    # }
    j <- which.max(sortiewiltest3[, 3])
    corfinal <- sortiewiltest3[j, 3]
    coefwil2<-sortiewiltest3[j]
    coeffinal<-coefwil2[1]

    dat %<>%
      dplyr::mutate(
        wil = exp(dim*coeffinal)
      )
    reg <- lm(bodyweight ~ dim + wil, data=dat)
    # print(sortiewiltest3)
    ##################################################
    key %>%
      dplyr::mutate(
        mdl_prd = tibble::lst(lm = reg),
        k = coeffinal,
        r2 = corfinal,
        n = nrow(dat),
        sortiewil = tibble::lst(s = sortiewiltest3),
        dataset = tibble::lst(dat),
        max_dim = max(dat$dim),
        min_dim = min(dat$dim)
      )
  }, .keep = T) %>%
  purrr::reduce(dplyr::bind_rows)
}

# Compute the augmented dataframe ####
out_of_sample_mir_cleaned <- out_of_sample_mir %>%
  dplyr::inner_join(out_of_sample_merged_count)
out_of_sample_bw_cleaned <- out_of_sample_bw %>%
  dplyr::inner_join(out_of_sample_merged_count)

augmented_oos_mir <- augment_dataframe(out_of_sample_mir_cleaned, mdl = model)
augmented_oos_bw <- augment_dataframe(out_of_sample_bw_cleaned)


augmented_oos <- augmented_oos_bw %>%
  tibble::add_column(type = "obs") %>%
  dplyr::bind_rows(
    augmented_oos_mir %>%
      tibble::add_column(type = "prd")
  )


# Print some ####
out_of_sample_merged_count

#1
ai <- "5502345414"
ei <- "55331006"
pt <- 2
#2
ai <- "5502345395"
ei <- "55331006"
pt <- 2
#3
ai <- "5502419987"
ei <- "55331006"
pt <- 1
#4
ai <- "5502345401"
ei <- "55331006"
pt <- 2
#5
ai <- "5502419996"
ei <- "55331006"
pt <- 1
#6
ai <- "5502489963"
ei <- "55331006"
pt <- 1
#7
ai <- "5502419996"
ei <- "55331006"
pt <- 1
#8
ai <- "5502345401"
ei <- "55331006"
pt <- 2

# Regression trend line
aoos_mir <- augmented_oos %>%
  dplyr::filter(an_id == ai, parity_full == pt) %>%
  dplyr::mutate(
    max_dim = min(max_dim),
    min_dim = max(min_dim)
  )
# datapoint
aoos_obs <- aoos_mir %>%
  dplyr::filter(type == "obs")
aoos_prd <- aoos_mir %>%
  dplyr::filter(type == "prd")
obs_dp <- aoos_obs$dataset$dat
prd_dp <- aoos_prd$dataset$dat

max_dim <- aoos_mir$max_dim %>% unique
min_dim <- aoos_mir$min_dim %>% unique

trend_df <- aoos_mir$type %>%
  purrr::map(.f = function(typ){
    tibble::tibble(
      dim = min_dim:max_dim,
      wil = exp(dim * aoos_mir %>%
                  dplyr::filter(type == typ) %>%
                  dplyr::pull(k)
      ),
      type = typ
    )
  }) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::group_by(type) %>%
  dplyr::group_map(.f = function(dat, key){
    mod <- aoos_mir %>%
      dplyr::filter(type == key$type) %>%
      dplyr::pull(mdl_prd) %>% `$`('lm')
    dat %>%
      dplyr::mutate(
        bodyweight = predict(mod, dat)
      )
  },.keep = T) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::mutate(type = factor(type, levels = c("prd", "obs")))

obs_dp %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight)) +
  ggplot2::geom_point(color = "steelblue") +
  ggplot2::geom_point(data = prd_dp, color = "darkred") +
  ggplot2::geom_line(ggplot2::aes(color = type), data = trend_df)
