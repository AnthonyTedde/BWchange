
c <- dataset_original %>%
  purrr::reduce(dplyr::bind_rows)

pin212_name

b <- c %>%
  dplyr::select(dplyr::starts_with("dpin"))
b%>%
  tidyr::drop_na() %>%
  dplyr::summarise(
    dplyr::across(.fns = sd)/
    dplyr::across(.fns = mean)
    # dplyr::across(.fns = sd, .names = "{.col}_{.fn}")/
    # dplyr::across(.fns = mean, .names = "{.col}_{.fn}")
  ) %>%
  tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Wavenumber",
                      values_to = "Coef") %>%
  ggplot2::ggplot(ggplot2::aes(x = Wavenumber, y = Coef)) +
  ggplot2::geom_point()  +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

c %<>%
  dplyr::select(dplyr::starts_with("dpin")) %>%
  tidyr::drop_na() %>%
  cor()

c %>%
  tidyr::drop_na()

sum(is.na(c))
dim(c)

corrplot::corrplot(c, order = "hclust")

  data("dataset_original")
