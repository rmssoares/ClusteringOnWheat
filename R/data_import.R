library(dplyr)

seedlabeled <- seeds_dataset_header
seedunlabeled <- select(seedlabeled, -Label)