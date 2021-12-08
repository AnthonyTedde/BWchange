library(magrittr)
rm(list = ls())
data("dataset_cleaned_phs1")

seeno <- dataset_cleaned_phs1 %>%
  dplyr::filter(provider %in% c("seenovia", "seenorest")) %>%
  dplyr::mutate(
    fat_rate = fat_rate/10,
    protein_rate = protein_rate/10,
    lactose_rate = lactose_rate/100
  )
swiss <- dataset_cleaned_phs1 %>%
  dplyr::filter(provider == "swiss") %>%
  dplyr::mutate(
    fat_rate = fat_rate/10,
    protein_rate = protein_rate/10,
    lactose_rate = lactose_rate/10
  )

dataset_cleaned_phs1 <- dplyr::bind_rows(
  dataset_cleaned_phs1 %>%
    dplyr::filter(!(provider %in% c("seenovia", "seenorest", "swiss"))),
  seeno, swiss
) %>%
  dplyr::arrange(id0) %>%
  dplyr::mutate(
    protein_rate_1_prd = protein_rate_1_prd/10,
  )


co <- function(a, b, prvd){
  dat <- dataset_cleaned_phs1 %>%
    dplyr::select(provider, dplyr::all_of(c(a, b))) %>%
    tidyr::drop_na()
  if (!missing(prvd)){
    dat %<>%
      dplyr::filter(provider == prvd)
  }
  with(dat,{
    a <- get(a)
    b <- get(b)
    cor(a, b)
  })
}

co("protein_rate", "protein_rate_1_prd")
co("lactose_rate", "lactose_rate_1_prd")
co("fat_rate", "fat_rate_1_prd")
co("fat_rate", "fat_rate_2_prd")

dataset_cleaned_phs1 %>%
  dplyr::filter(!is.na(fat_rate)) %>%
  dplyr::pull(provider) %>% unique

co("fat_rate", "fat_rate_2_prd")
co("fat_rate", "fat_rate_2_prd", "seenorest")
co("fat_rate", "fat_rate_2_prd", "seenovia")
co("fat_rate", "fat_rate_2_prd", "swiss")
co("fat_rate", "fat_rate_2_prd", "hso")

co("protein_rate", "protein_rate_1_prd")
co("protein_rate", "protein_rate_1_prd", "seenorest")
co("protein_rate", "protein_rate_1_prd", "seenovia")
co("protein_rate", "protein_rate_1_prd", "swiss")
co("protein_rate", "protein_rate_1_prd", "hso")

co("lactose_rate", "lactose_rate_1_prd")
co("lactose_rate", "lactose_rate_1_prd", "seenorest")
co("lactose_rate", "lactose_rate_1_prd", "seenovia")
co("lactose_rate", "lactose_rate_1_prd", "swiss")
co("lactose_rate", "lactose_rate_1_prd", "hso")

# plot

dataset_cleaned_phs1 %>%
  dplyr::filter(provider %in% c("seenorest", "seenovia", "swiss", "hso")) %>%
  dplyr::select(an_id, provider, fat_rate, fat_rate_2_prd) %>%
  ggplot2::ggplot(ggplot2::aes(x = fat_rate, y = fat_rate_2_prd,
                               color = provider)) +
  ggplot2::geom_point()


dataset_cleaned_phs1 %>%
  dplyr::filter(provider %in% c("seenorest", "seenovia", "swiss", "hso")) %>%
  dplyr::select(an_id, provider, protein_rate, protein_rate_1_prd) %>%
  ggplot2::ggplot(ggplot2::aes(x = protein_rate, y = protein_rate_1_prd,
                               color = provider)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1)

dataset_cleaned_phs1 %>%
  dplyr::filter(provider %in% c("seenorest", "seenovia", "swiss", "hso")) %>%
  dplyr::select(an_id, provider, lactose_rate, lactose_rate_1_prd) %>%
  ggplot2::ggplot(ggplot2::aes(x = lactose_rate, y = lactose_rate_1_prd,
                               color = provider)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1)

# ------------------------------------------------------------------------------
# Fat rate
# ------------------------------------------------------------------------------

data_to_remove <- (
  dataset_cleaned_phs1$fat_rate_2_prd < 1.5 |
    dataset_cleaned_phs1$fat_rate_2_prd > 9 |
    dataset_cleaned_phs1$protein_rate_1_prd < 1 |
    dataset_cleaned_phs1$protein_rate_1_prd > 7
)
mean(data_to_remove)



dataset_cleaned_phs1 <- dataset_cleaned_phs1[!data_to_remove, ]

# Teset
rmse_byprovider <- function(d, m, by = "provider"){
  d %>%
    dplyr::group_by(provider) %>%
    dplyr::group_map(.f = function(d, k){
      tibble::tibble(
        prd = predict(m, newdata = d, ncomp = 15) %>% drop,
        tth = d$bodyweight
      ) %>%
        yardstick::rmse(truth = tth, estimate = prd) %>%
        tibble::add_column(provider = k, .before = 1)
    }) %>%
    purrr::reduce(dplyr::bind_rows)
}

with(dataset_cleaned_phs1,{
  table(breed, provider)
})
data("pls_test")
mdl <- pls_test
rmse_byprovider(dataset_cleaned_phs1, mdl)

dim(dataset_cleaned_phs1)

dataset_cleaned_phs1 %>%
  dplyr::select(provider, fat_rate_2_prd) %>%
  tidyr::drop_na() %>%
  ggplot2::ggplot(ggplot2::aes(x = fat_rate_2_prd,
                               color = provider, group = provider)) +
  ggplot2::geom_boxplot()

dataset_cleaned_phs1 %>%
  dplyr::select(provider, protein_rate_1_prd) %>%
  tidyr::drop_na() %>%
  ggplot2::ggplot(ggplot2::aes(x = protein_rate_1_prd,
                               color = provider, group = provider)) +
  ggplot2::geom_boxplot()

dataset_cleaned_phs1 %>%
  dplyr::select(provider, lactose_rate_1_prd) %>%
  tidyr::drop_na() %>%
  ggplot2::ggplot(ggplot2::aes(x = lactose_rate_1_prd,
                               color = provider, group = provider)) +
  ggplot2::geom_boxplot()


# ------------------------------------------------------------------------------
# Filter by protein and fat_rate
# ------------------------------------------------------------------------------

dataset_cleaned_phs1 %<>%
  dplyr::mutate(
    protein_rate_error = protein_rate - protein_rate_1_prd,
    fat_rate_error = fat_rate - fat_rate_1_prd,
  )
seenorest <- dataset_cleaned_phs1 %>%
  dplyr::filter(provider == "seenorest")

# Fat all
dataset_cleaned_phs1 %>%
  ggplot2::ggplot(ggplot2::aes(x = fat_rate_error)) +
  ggplot2::geom_histogram(bins = 100)

# Protein all by provider
dataset_cleaned_phs1 %>%
  ggplot2::ggplot(ggplot2::aes(x = protein_rate_error,
                               color = provider)) +
  ggplot2::geom_boxplot()

# Fat all by provider
dataset_cleaned_phs1 %>%
  ggplot2::ggplot(ggplot2::aes(x = fat_rate_error,
                               color = provider)) +
  ggplot2::geom_boxplot()

seenorest %>%
  ggplot2::ggplot(ggplot2::aes(x = protein_rate_error)) +
  ggplot2::geom_histogram(bins = 100)

# ------------------------------------------------------------------------------
# Filter by protein and fat_rate
# ------------------------------------------------------------------------------

predictor_v <- names(dataset_cleaned_phs1)[
  stringr::str_detect(names(dataset_cleaned_phs1),
                      pattern = "^dpin(?=[:digit:]{4})|^pred(?=[:digit:]{3})",
                      negate = T)
]

FAs <- predictor_v[stringr::str_detect(
  predictor_v,
  pattern = "^C(?=[:digit:])"
)]

dataset_cleaned_phs1 %<>%
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::all_of(FAs),
      .fns = ~ .x / fat_rate_2_prd,
      .names = "{.col}_in_fat"
    )
  )

dataset_cleaned <- dataset_cleaned_phs1

source(file = here::here("helper", "plot_desc2.R"))
plot_desc2(dataset_cleaned %>% dplyr::filter(breed == "Holstein"))
dataset_cleaned %>% dplyr::filter(breed == "Holstein") %>%
  dplyr::pull(provider) %>% table


# restore unique identifier
dataset_cleaned %<>%
  dplyr::select(-id0) %>%
  tibble::rowid_to_column(var = "uid") %>%
  dplyr::mutate(
    an_uid = as.integer(factor(an_id)),
    expl_uid = as.integer(factor(expl_id))
  )

# ------------------------------------------------------------------------------
# Save ####
# ------------------------------------------------------------------------------
save(dataset_cleaned, file = "data/dataset_cleaned.rda")

