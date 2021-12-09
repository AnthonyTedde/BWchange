packages_lst <- c("glue", "pls", "tibble", "stringr", "rsample", "parsnip",
                  "tune", "tidy", "here", "dials", "recipes", "purrr",
                  "workflowsets", "doParallel", "BiocManager", "magrittr",
                  "tidymodels", "tidyverse", "kernlab")


install.packages(pkgs = packages_lst)

# Install from BioConductor project
BiocManager::install('mixOmics')
