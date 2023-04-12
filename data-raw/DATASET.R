## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)


# A.setosa dataset --------------------------------------------------------
data("A.setosa")

url = "https://api.gbif.org/v1/occurrence/download/request/0008110-190621201848488.zip"
download.file(url, "data-raw/A_setosa.zip")
unzip("data-raw/A_setosa.zip", exdir = "data-raw")

A.setosa <- read.csv("data-raw/0008110-190621201848488.csv",
                       sep = "\t")

use_data(A.setosa)


