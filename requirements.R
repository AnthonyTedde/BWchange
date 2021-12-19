packages_lst <- c("glue", "pls", "tibble", "stringr", "rsample", "parsnip",
                  "tune", "tidy", "here", "dials", "recipes", "purrr",
                  "workflowsets", "doParallel", "BiocManager", "magrittr",
                  "tidymodels", "tidyverse", "kernlab", "keras", "plsmod")


install.packages(pkgs = packages_lst,
                 repos = "https://www.freestatistics.org/cran/")

# Install from BioConductor project
BiocManager::install('mixOmics')
