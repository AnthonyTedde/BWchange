data("dataset_cleaned")
dim(dataset_cleaned)

str(dataset_cleaned)

lobstr::obj_size(dataset_cleaned$mdl_prd)
lobstr::obj_size(dataset_cleaned$bodyweight)
lobstr::obj_size(dataset_cleaned$dim)
lobstr::obj_size(dataset_cleaned$dpin0012)



dat <- dataset_cleaned %>%
  dplyr::summarise(
    dplyr::across(lobstr::obj_size)
  )


feather::write_feather(dataset_cleaned, path = "data/dataset_cleaned.feather")
