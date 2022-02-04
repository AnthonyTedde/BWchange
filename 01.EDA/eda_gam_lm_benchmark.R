#-------------------------------------------------------------------------------
# Analysis
#-------------------------------------------------------------------------------
rm(list = ls())
library(magrittr)
library(splines)
data("dataset_cleaned")

# Add lactation period
dataset_cleaned %<>%
  dplyr::mutate(
    lactation_time = dplyr::case_when(
      dim < 100 ~ 'early',
      dim < 200 ~ 'mid',
      T ~ 'late'
    ) %>% ordered(levels = c('early', 'mid', 'late'))
  )

# Relation function
--------------------
  plt <- function(y = "bodyweight", x, by){
    p <- dataset_cleaned %>%
      ggplot2::ggplot(ggplot2::aes_string(y = y, x = x)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "gam")
    if(!missing(by)){
    p <- p + ggplot2::facet_grid(by)
    }
    return(p)
  }
# Correlation function
#----------------------
coco <- function(y = "bodyweight", x, by){
  dat <- dataset_cleaned
  if(!missing(by)){
    dat %<>%
      dplyr::group_by(dplyr::across(dplyr::all_of(by)))
  }
  dat %>%
    # dplyr::summarise(cor = cor(bodyweight, {{x}}))
    dplyr::summarise(
      cor = cor(!!rlang::sym(y), !!rlang::sym(x))
    )
}
# Density function
#----------------------
densty <- function(var, by, dataset){
  if(missing(dataset))
    dataset <- dataset_cleaned
  if(missing(by)){
    dataset %>%
      ggplot2::ggplot(ggplot2::aes_string(x = var)) +
      ggplot2::geom_histogram() +
      ggplot2::geom_vline(xintercept = 0, color = "darkred")
  }else{
    dataset %>%
      ggplot2::ggplot(ggplot2::aes_string(x = by, y = var)) +
      ggplot2::geom_violin() +
      ggplot2::geom_hline(yintercept = 0, color = "darkred")
  }
}

# Density Components function
#----------------------
densty_components <- function(dataset, cols){
  if(missing(dataset)){
    dataset <- dataset_cleaned
  }
  dataset %>%
    dplyr::select(provider, dplyr::all_of(cols)) %>%
    tidyr::pivot_longer(cols = cols, names_to = "Component") %>%
    dplyr::mutate(Component = factor(Component, levels = cols)) %>%
    ggplot2::ggplot(ggplot2::aes(y = value, x = Component)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_hline(yintercept = 0, color = "darkred") +
    ggplot2::coord_flip()
}

# FAT
#-------------
var <- "fat_rate_2_prd"
plt(x=var, by = "provider")
coco(x = var, by = "provider")
plt(x=var)
coco(x = var)
plt(x=var, by="lactation_time~parity_fct")
coco(x = var, by = c("lactation_time", "parity_fct"))

# protein
#-------------
var <- "protein_rate_1_prd"
plt(x=var, by = "provider")
coco(x = var, by = "provider")
plt(x=var)
coco(x = var)
plt(x=var, by="lactation_time~parity_fct")
coco(x = var, by = c("lactation_time", "parity_fct"))

# lactose
#-------------
var <- "lactose_rate_1_prd"
plt(x=var, by = "provider")
coco(x = var, by = "provider")
plt(x=var)
coco(x = var)
plt(x=var, by="lactation_time~parity_fct")
coco(x = var, by = c("lactation_time", "parity_fct"))

# C4
#-------------
var <- "C4_prd"
densty(var)
plt(x=var, by = "provider")
coco(x = var, by = "provider")
plt(x=var)
coco(x = var)
plt(x=var, by="lactation_time~parity_fct")
coco(x = var, by = c("lactation_time", "parity_fct"))

# C6
#-------------
var <- "C6_prd"
densty(var)
densty(var, by = "provider")
densty(var, by = "lactation_time")
plt(x=var, by = "provider")
coco(x = var, by = "provider")
plt(x=var)
coco(x = var)
plt(x=var, by="lactation_time~parity_fct")
coco(x = var, by = c("lactation_time", "parity_fct"))

# C8
#-------------
var <- "C8_prd"
densty(var)
densty(var, by = "provider")
densty(var, by = "lactation_time")
plt(x=var, by = "provider")
coco(x = var, by = "provider")
plt(x=var)
coco(x = var)
plt(x=var, by="lactation_time~parity_fct")
coco(x = var, by = c("lactation_time", "parity_fct"))


# ------------------------------------------------------------------------------
# Graph component
# ------------------------------------------------------------------------------

# All
#-------------
comps <- grep(pattern = "^C[[:digit:]]+.*prd$", x = names(dataset_cleaned),
     value = T)
densty_components(cols = comps)

# Saturated, mono, poly, insatudated
--------------
comps <- c('FA_sat_prd', 'FA_mono_prd', 'FA_poly_prd', 'FA_insat_prd')
densty_components(cols = comps)

# short, medium, long
--------------
comps <- c('FA_short_prd', 'FA_medium_prd', 'FA_long_prd')
densty_components(cols = comps)

# O my gaaaa
--------------
comps <- c('FA_omega3_prd', 'FA_omega6_prd')
densty_components(cols = comps)

# Na, Ca, P, Mg, K
--------------
comps <- c('Na_prd', 'Ca_prd', 'P_prd', 'Mg_prd', 'K_prd')
densty_components(cols = comps)

# Acetone, Citrate, BHB et plus si affinité
--------------
comps <- c('Acetone_std_prd')
dataset_cleaned %>%
  dplyr::filter(Acetone_std_prd < 100) %>%
densty_components(cols = comps)

comps <- c('citrates_std_prd')
densty_components(cols = comps)

comps <- c('BHB_std_prd')
densty_components(cols = comps)

comps <- c('lactoferrin_prd')
densty_components(cols = comps)

comps <- c('pH_prd')
densty_components(cols = comps)

comps <- c('solid_recovery_prd', 'fat_recovery_prd', 'protein_recovery_prd')
densty_components(cols = comps)

#-------------------------------------------------------------------------------
# Models
#-------------------------------------------------------------------------------

# data
mdl_data <- dataset_cleaned %>%
  # dplyr::filter(!provider %in% c("seenorest", "swiss", "utrobe"))
  dplyr::filter(!provider %in% c("seenorest", "swiss"))
mdl_test <- dataset_cleaned %>%
  dplyr::filter(provider %in% "swiss")

# linear
benchmark_mdl <- lm(bodyweight ~ dim + parity_fct, data = mdl_data)
summary(benchmark_mdl)

tidyr::expand_grid(
  dim = 0:365,
  parity_fct = unique(dataset_cleaned$parity_fct)) %>%
  dplyr::mutate(
    bodyweight = predict(benchmark_mdl, newdata = data.frame(dim, parity_fct))
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight, color = parity_fct)) +
  ggplot2::geom_point()

# dim with natural spline
benchmark_mdl <- lm(bodyweight ~ ns(dim, 3) + parity_fct, data = mdl_data)
summary(benchmark_mdl)
# 37% -> 49%

tidyr::expand_grid(
  dim = 0:365,
  parity_fct = unique(dataset_cleaned$parity_fct)) %>%
  dplyr::mutate(
    bodyweight = predict(benchmark_mdl, newdata = data.frame(dim, parity_fct))
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight, color = parity_fct)) +
  ggplot2::geom_point()

# dim with natural spline + mikl yield
benchmark_mdl <- lm(
  bodyweight ~ ns(dim, 3) + milk_yield + parity_fct,
  data = mdl_data
  )
summary(benchmark_mdl)
# 49% -> 55%

mdl_data %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = milk_yield)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "gam") +
  ggplot2::facet_grid("parity_fct")

my_gam <- mgcv::gam(milk_yield ~ s(dim) + parity_fct ,
                    data = mdl_data, method = "REML")

tidyr::expand_grid(
  dim = 0:365,
  parity_fct = unique(dataset_cleaned$parity_fct),
  ) %>%
  dplyr::mutate(
    milk_yield = predict(my_gam,
                         newdata = data.frame(dim, parity_fct)),
    bodyweight = predict(benchmark_mdl,
                         newdata = data.frame(dim, parity_fct, milk_yield))
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight, color = parity_fct)) +
  ggplot2::geom_point()


# dim with natural spline + mikl yield with natural spline
benchmark_mdl <- lm(
  bodyweight ~ ns(dim, 3) + ns(milk_yield, 3) + parity_fct,
  data = mdl_data
  )
summary(benchmark_mdl)
# 55% -> 56%

mdl_data %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = milk_yield)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "gam") +
  ggplot2::facet_grid("parity_fct")

my_gam <- mgcv::gam(milk_yield ~ s(dim) + parity_fct ,
                    data = mdl_data, method = "REML")

tidyr::expand_grid(
  dim = 0:365,
  parity_fct = unique(dataset_cleaned$parity_fct),
  ) %>%
  dplyr::mutate(
    milk_yield = predict(my_gam,
                         newdata = data.frame(dim, parity_fct)),
    bodyweight = predict(benchmark_mdl,
                         newdata = data.frame(dim, parity_fct, milk_yield))
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight, color = parity_fct)) +
  ggplot2::geom_point()



# dim with natural spline + mikl yield
benchmark_mdl <- lm(
  bodyweight ~ ns(dim, 3) + milk_yield + parity_fct +
    milk_yield:dim + milk_yield:parity_fct,
  data = mdl_data
  )
summary(benchmark_mdl)
# 55% -> 55%

my_gam <- mgcv::gam(milk_yield ~ s(dim) + parity_fct ,
                    data = mdl_data, method = "REML")

tidyr::expand_grid(
  dim = 0:365,
  parity_fct = unique(dataset_cleaned$parity_fct),
  ) %>%
  dplyr::mutate(
    milk_yield = predict(my_gam,
                         newdata = data.frame(dim, parity_fct)),
    bodyweight = predict(benchmark_mdl,
                         newdata = data.frame(dim, parity_fct, milk_yield))
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight, color = parity_fct)) +
  ggplot2::geom_point()


# ---------------------------------------------------
# dim with natural spline + mikl yield
# Final benchmark
# ---------------------------------------------------
benchmark_mdl <- lm(
  bodyweight ~ ns(dim, 3) + milk_yield + parity_fct + milk_yield:parity_fct,
  data = mdl_data
  )
summary(benchmark_mdl)
# 55% -> 55%

sqrt(mean((predict(benchmark_mdl, newdata = mdl_data) - mdl_data$bodyweight)^2))
sqrt(mean((predict(benchmark_mdl, newdata = mdl_test) - mdl_test$bodyweight)^2))

my_gam <- mgcv::gam(milk_yield ~ s(dim) + parity_fct ,
                    data = mdl_data, method = "REML")

tidyr::expand_grid(
  dim = 0:365,
  parity_fct = unique(dataset_cleaned$parity_fct),
  ) %>%
  dplyr::mutate(
    milk_yield = predict(my_gam,
                         newdata = data.frame(dim, parity_fct)),
    bodyweight = predict(benchmark_mdl,
                         newdata = data.frame(dim, parity_fct, milk_yield))
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight, color = parity_fct)) +
  ggplot2::geom_point()


# ---------------------------------------------------
# dim with natural spline + mikl yield +
# FA sat, mono, poly
# ---------------------------------------------------
benchmark_mdl <- lm(
  bodyweight ~ ns(dim, 3) + milk_yield + parity_fct + milk_yield:parity_fct +
  FA_sat_prd + FA_mono_prd + FA_poly_prd + FA_insat_prd,
  data = mdl_data
  )
summary(benchmark_mdl)
# 55% -> 59%

sqrt(mean((predict(benchmark_mdl, newdata = mdl_data) - mdl_data$bodyweight)^2))
sqrt(mean((predict(benchmark_mdl, newdata = mdl_test) - mdl_test$bodyweight)^2))


mdl_test %>%
  # ggplot2::ggplot(ggplot2::aes(x = dim, y = FA_sat_prd)) +
  # ggplot2::ggplot(ggplot2::aes(x = dim, y = FA_mono_prd)) +
  # ggplot2::ggplot(ggplot2::aes(x = dim, y = FA_poly_prd)) +
  ggplot2::ggplot(ggplot2::aes(x = dim, y = FA_insat_prd)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "gam") +
  ggplot2::facet_grid(~parity_fct)


my_gam <- mgcv::gam(milk_yield ~ s(dim) + parity_fct ,
                    data = mdl_test, method = "REML")
sat_gam <- mgcv::gam(FA_sat_prd ~ s(dim) + parity_fct,
                     data = mdl_test, method = "REML")
mono_gam <- mgcv::gam(FA_mono_prd ~ s(dim) + parity_fct,
                      data = mdl_test, method = "REML")
poly_gam <- mgcv::gam(FA_poly_prd ~ s(dim) + parity_fct,
                      data = mdl_test, method = "REML")
insat_gam <- mgcv::gam(FA_insat_prd ~ s(dim) + parity_fct,
                       data = mdl_test, method = "REML")


tidyr::expand_grid(
  dim = 0:365,
  parity_fct = unique(dataset_cleaned$parity_fct),
  ) %>%
  dplyr::mutate(
    milk_yield = predict(my_gam,
                         newdata = data.frame(dim, parity_fct)),
    FA_sat_prd = predict(sat_gam,
                         newdata = data.frame(dim, parity_fct)),
    FA_mono_prd = predict(mono_gam,
                         newdata = data.frame(dim, parity_fct)),
    FA_poly_prd = predict(poly_gam,
                         newdata = data.frame(dim, parity_fct)),
    FA_insat_prd = predict(insat_gam,
                         newdata = data.frame(dim, parity_fct)),
    bodyweight = predict(benchmark_mdl,
                         newdata = data.frame(dim, parity_fct, milk_yield,
                                              FA_sat_prd, FA_mono_prd,
                                              FA_poly_prd, FA_insat_prd))
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight, color = parity_fct)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(data = mdl_test, alpha = .2)


# ---------------------------------------------------
# dim with natural spline + mikl yield +
# FA sat, mono, poly
# ---------------------------------------------------
# lin
benchmark_mdl <- lm(
  bodyweight ~ ns(dim, 3) + milk_yield + parity_fct + milk_yield:parity_fct +
  ns(FA_sat_prd, 5) + ns(FA_mono_prd, 5) + ns(FA_poly_prd, 5) +
    ns(FA_insat_prd, 5) +
    ns(Na_prd, 3) + ns(Ca_prd3) + ns(P_prd, 3) + ns(Mg_prd, 3) + ns(K_prd, 3),
  data = mdl_data
  )
summary(benchmark_mdl)
# 55% -> 61%

# gam
benchmark_mdl_gam <- mgcv::gam(
  bodyweight ~ s(dim) + milk_yield + parity_fct + milk_yield:parity_fct +
  te(dim, FA_sat_prd) + te(dim, FA_mono_prd) + te(dim, FA_poly_prd) +
    te(dim, FA_insat_prd) +
    te(dim, Na_prd) + te(dim, Ca_prd) + te(dim, P_prd) + te(dim, Mg_prd) +
    te(dim, K_prd),
  data = mdl_data
  )
summary(benchmark_mdl_gam)
# 55 -> 64

sqrt(mean((predict(benchmark_mdl, newdata = mdl_data) - mdl_data$bodyweight)^2))
sqrt(mean((predict(benchmark_mdl, newdata = mdl_test) - mdl_test$bodyweight)^2))
sqrt(mean((predict(benchmark_mdl_gam, newdata = mdl_data) - mdl_data$bodyweight)^2))
sqrt(mean((predict(benchmark_mdl_gam, newdata = mdl_test) - mdl_test$bodyweight)^2))


mdl_test %>%
  # ggplot2::ggplot(ggplot2::aes(x = dim, y = FA_sat_prd)) +
  # ggplot2::ggplot(ggplot2::aes(x = dim, y = FA_mono_prd)) +
  # ggplot2::ggplot(ggplot2::aes(x = dim, y = FA_poly_prd)) +
  ggplot2::ggplot(ggplot2::aes(x = dim, y = FA_insat_prd)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "gam") +
  ggplot2::facet_grid(~parity_fct)


my_gam <- mgcv::gam(milk_yield ~ s(dim, k=3) + parity_fct ,
                    data = mdl_test, method = "REML")
sat_gam <- mgcv::gam(FA_sat_prd ~ s(dim, k=3) + parity_fct,
                     data = mdl_test, method = "REML")
mono_gam <- mgcv::gam(FA_mono_prd ~ s(dim, k=3) + parity_fct,
                      data = mdl_test, method = "REML")
poly_gam <- mgcv::gam(FA_poly_prd ~ s(dim, k=3) + parity_fct,
                      data = mdl_test, method = "REML")
insat_gam <- mgcv::gam(FA_insat_prd ~ s(dim, k=3) + parity_fct,
                       data = mdl_test, method = "REML")
na_gam <- mgcv::gam(Na_prd ~ s(dim, k=3) + parity_fct,
                    data = mdl_test, method = "REML")
ca_gam <- mgcv::gam(Ca_prd ~ s(dim, k=3) + parity_fct,
                    data = mdl_test, method = "REML")
p_gam <- mgcv::gam(P_prd ~ s(dim, k=3) + parity_fct,
                    data = mdl_test, method = "REML")
mg_gam <- mgcv::gam(Mg_prd ~ s(dim, k=3) + parity_fct,
                    data = mdl_test, method = "REML")
k_gam <- mgcv::gam(K_prd ~ s(dim, k=3) + parity_fct,
                    data = mdl_test, method = "REML")


tidyr::expand_grid(
  dim = 0:365,
  parity_fct = unique(dataset_cleaned$parity_fct),
  ) %>%
  dplyr::mutate(
    milk_yield = predict(my_gam,
                         newdata = data.frame(dim, parity_fct)),
    FA_sat_prd = predict(sat_gam,
                         newdata = data.frame(dim, parity_fct)),
    FA_mono_prd = predict(mono_gam,
                         newdata = data.frame(dim, parity_fct)),
    FA_poly_prd = predict(poly_gam,
                         newdata = data.frame(dim, parity_fct)),
    FA_insat_prd = predict(insat_gam,
                         newdata = data.frame(dim, parity_fct)),
    Na_prd = predict(na_gam,
                     newdata = data.frame(dim, parity_fct)),
    Ca_prd = predict(ca_gam,
                     newdata = data.frame(dim, parity_fct)),
    P_prd = predict(p_gam,
                     newdata = data.frame(dim, parity_fct)),
    Mg_prd = predict(mg_gam,
                     newdata = data.frame(dim, parity_fct)),
    K_prd = predict(k_gam,
                     newdata = data.frame(dim, parity_fct)),
    bodyweight = predict(benchmark_mdl,
                         newdata = data.frame(dim, parity_fct, milk_yield,
                                              FA_sat_prd, FA_mono_prd,
                                              FA_poly_prd, FA_insat_prd,
                                              Na_prd, Ca_prd, P_prd,Mg_prd,
                                              K_prd))
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight, color = parity_fct)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(data = mdl_test, alpha = .2)

tidyr::expand_grid(
  dim = 0:365,
  parity_fct = unique(dataset_cleaned$parity_fct),
  ) %>%
  dplyr::mutate(
    milk_yield = predict(my_gam,
                         newdata = data.frame(dim, parity_fct)),
    FA_sat_prd = predict(sat_gam,
                         newdata = data.frame(dim, parity_fct)),
    FA_mono_prd = predict(mono_gam,
                         newdata = data.frame(dim, parity_fct)),
    FA_poly_prd = predict(poly_gam,
                         newdata = data.frame(dim, parity_fct)),
    FA_insat_prd = predict(insat_gam,
                         newdata = data.frame(dim, parity_fct)),
    Na_prd = predict(na_gam,
                     newdata = data.frame(dim, parity_fct)),
    Ca_prd = predict(ca_gam,
                     newdata = data.frame(dim, parity_fct)),
    P_prd = predict(p_gam,
                     newdata = data.frame(dim, parity_fct)),
    Mg_prd = predict(mg_gam,
                     newdata = data.frame(dim, parity_fct)),
    K_prd = predict(k_gam,
                     newdata = data.frame(dim, parity_fct)),
    bodyweight = predict(benchmark_mdl_gam,
                         newdata = data.frame(dim, parity_fct, milk_yield,
                                              FA_sat_prd, FA_mono_prd,
                                              FA_poly_prd, FA_insat_prd,
                                              Na_prd, Ca_prd, P_prd,Mg_prd,
                                              K_prd))
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = dim, y = bodyweight, color = parity_fct)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(data = mdl_test, alpha = .2)
