 ## code to prepare `DATASET` dataset goes here

sample_data <- read.csv ("Data.csv", path = "inst/extdata/Data.csv")
usethis::use_data(Data, overwrite = TRUE)
