plot_desc2 <- function(dat){


  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  # Plot bodyweight ####
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

  BW_plt <- dat %>%
    tibble::add_column(ttl = "Bodyweight") %>%
    ggplot2::ggplot(ggplot2::aes(x = bodyweight, y = provider,
                                 color = provider)) +
    ggplot2::geom_boxplot() +
    ggsci::scale_fill_nejm(name = "Datasets") +
    ggsci::scale_color_nejm(name = "Datasets") +
    ggplot2::scale_x_continuous(name = "BW (kg)") +
    ggplot2::scale_y_discrete(name = "Datasets") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      axis.title.x = ggplot2::element_text(size = 10, family = "sans"),
      axis.text.x = ggplot2::element_text(size = 9, family = "sans"),
      legend.title = ggplot2::element_text(size = 10, family = "sans"),
      legend.text = ggplot2::element_text(size = 9, family = "sans"),
      axis.text.y = ggplot2::element_text(size = 9, family = "sans"),
      axis.title.y = ggplot2::element_text(size = 10, family = "sans")
    )  +
    ggplot2::facet_grid(~ttl)

  legend <- cowplot::get_legend(BW_plt)

  BW_plt <-  BW_plt +
    ggplot2::theme(
      legend.position = "none"
    )


  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  # plot Milk-yield ####
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

  MY_plt <- dat %>%
    tibble::add_column(ttl = "Milk yield") %>%
    ggplot2::ggplot(ggplot2::aes(x = milk_yield, y = provider,
                                 color = provider)) +
    ggplot2::geom_boxplot() +
    ggsci::scale_fill_nejm(name = "Datasets") +
    ggsci::scale_color_nejm(name = "Datasets") +
    ggplot2::scale_x_continuous(name = "Milk yield (kg)") +
    ggplot2::scale_y_discrete(name = "Datasets") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x = ggplot2::element_text(size = 10, family = "sans"),
      axis.text.x = ggplot2::element_text(size = 9, family = "sans"),
      legend.title = ggplot2::element_text(size = 10, family = "sans"),
      legend.text = ggplot2::element_text(size = 9, family = "sans"),
      axis.text.y = ggplot2::element_text(size = 9, family = "sans"),
      axis.title.y = ggplot2::element_text(size = 10, family = "sans")
    )   +
    ggplot2::facet_grid(~ttl)


  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  # plot parity ####
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

  PRT_plt <- dat %>%
    dplyr::mutate(parity = ifelse(parity > 3, 3, parity)) %>%
    dplyr::mutate(parity = factor(parity)) %>%
    tibble::add_column(ttl = "Parity") %>%
    ggplot2::ggplot(ggplot2::aes(x = provider,
                                 color = parity,
                                 fill = parity)) +
    ggplot2::geom_bar(position = "fill") +
    ggsci::scale_fill_uchicago() +
    ggsci::scale_color_uchicago() +
    ggplot2::scale_x_discrete(name = "Parity") +
    ggplot2::scale_y_continuous(name = "Count (n)",
                                labels = scales::percent) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      axis.title.x = ggplot2::element_text(size = 10, family = "sans"),
      axis.text.x = ggplot2::element_text(size = 9, family = "sans"),
      legend.title = ggplot2::element_text(size = 10, family = "sans"),
      legend.text = ggplot2::element_text(size = 9, family = "sans"),
      axis.text.y = ggplot2::element_text(size = 9, family = "sans"),
      axis.title.y = ggplot2::element_text(size = 10, family = "sans")
    ) +  ggplot2::facet_grid(~ttl)

  legend_parity <- cowplot::get_legend(PRT_plt)

  PRT_plt <-  PRT_plt +
    ggplot2::theme(
      legend.position = "none"
    )


  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  # plot weeks of lactation ####
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

  wol_size <- 7


  WOL_plt <- dat %>%
    dplyr::mutate(
      wol_fct = cut(dim,
                    breaks = seq(0, max(dim) + wol_size - max(dim) %% wol_size ,
                                 by = wol_size),
                    include.lowest = T, ordered_result = T)
    ) %>%
    tibble::add_column(ttl = "Week of lactation") %>%
    ggplot2::ggplot(ggplot2::aes(x = as.numeric(wol_fct), y = provider,
                                 color = provider)) +
    ggplot2::geom_boxplot() +
    ggsci::scale_fill_nejm() +
    ggsci::scale_color_nejm() +
    ggplot2::scale_x_continuous(
      name = "Week of lactration"
    ) +
    ggplot2::scale_y_discrete(name = "Datasets") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x = ggplot2::element_text(size = 10, family = "sans"),
      # axis.text.x = ggplot2::element_text(size = 9, family = "sans",
      #                                     angle = 90, vjust = 0.5, hjust=1),
      axis.text.x = ggplot2::element_text(size = 9, family = "sans"),
      legend.title = ggplot2::element_text(size = 10, family = "sans"),
      legend.text = ggplot2::element_text(size = 9, family = "sans"),
      axis.text.y = ggplot2::element_text(size = 9, family = "sans"),
      axis.title.y = ggplot2::element_text(size = 10, family = "sans")
    ) +  ggplot2::facet_grid(~ttl)


  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  # plot spectra ####
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

  spectral_data <- dat %>%
    dplyr::select(provider, an_id, country, dplyr::starts_with("dpin"))

  ## Compute PCA on sprecta data.
  preprocess_lst <-  spectral_data %>%
    dplyr::select(-c(an_id, country)) %>%
    dplyr::group_by(provider) %>%
    dplyr::group_map(caret::preProcess,
                     method = c("center", "scale", "pca"),
                     pcaComp = 2) %>%
    setNames(
      nm = spectral_data %>%
        dplyr::group_by(provider) %>%
        dplyr::group_keys() %>% dplyr::pull()
    )



  pca_prdt <- preprocess_lst %>%
    purrr::imap(function(mdl, dt_provider){
      dt <- spectral_data %>%
        dplyr::filter(provider == dt_provider)

      predict(mdl, dt) %>%
        tibble::as_tibble()
    }) %>%
    purrr::reduce(dplyr::bind_rows)

  data_pca <- pca_prdt

  spectra_plt <- data_pca %>%
    tibble::add_column(ttl = "Spectral data projected on principal component (PC, n=2)") %>%
    ggplot2::ggplot(ggplot2::aes(x = PC1, y = PC2, color = provider)) +
    ggplot2::geom_point() +
    ggsci::scale_fill_nejm() +
    ggsci::scale_color_nejm() +
    ggplot2::scale_x_continuous(name = "PC1") +
    ggplot2::scale_y_continuous(name = "PC2") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x = ggplot2::element_text(size = 10, family = "sans"),
      axis.text.x = ggplot2::element_text(size = 9, family = "sans"),
      legend.title = ggplot2::element_text(size = 10, family = "sans"),
      legend.text = ggplot2::element_text(size = 9, family = "sans"),
      axis.text.y = ggplot2::element_text(size = 9, family = "sans"),
      axis.title.y = ggplot2::element_text(size = 10, family = "sans")
    ) +  ggplot2::facet_grid(ttl ~ provider)


  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  # Merge all plot ####
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

  BW_prt_plt <- cowplot::plot_grid(
    BW_plt, PRT_plt,
    nrow = 1,
    label_size = 10, label_fontface = "plain",
    rel_widths = c(2, 1)
  )

  wol_my_plt <- cowplot::plot_grid(
    WOL_plt, MY_plt,
    nrow = 1,
    label_size = 10, label_fontface = "plain"
  )
  spct_plt <- spectra_plt

  hi_plt <- cowplot::plot_grid(
    BW_prt_plt,
    wol_my_plt,
    spct_plt,
    nrow = 3
  )

  lg <- cowplot::plot_grid(
    legend,
    legend_parity,
    nrow = 1
  )

  cowplot::plot_grid(
    hi_plt,
    lg,
    nrow = 2,
    rel_heights = c(20,1)
  )
}
