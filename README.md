
# naturaList

<!-- badges: start -->
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/naturaList)](https://cran.r-project.org/package=naturaList)

[![R-CMD-check](https://github.com/avrodrigues/naturaList/workflows/R-CMD-check/badge.svg)](https://github.com/avrodrigues/naturaList/actions)
<!-- badges: end -->

The goal of naturaList is to provide tools for check identification reliability 
in species occurrence records data sets. The rationale is that by providing a list specialists in the taxon of interest, the user is able to classify which records were identified by specialists. In addition other characteristics of the records could be used to derive six levels of confidence. 

## Levels of confidence

The package allows to classify the confidence in the identification of the specimen using the `classify_occ()` function. The most reliable identification of a specimen is made by a specialist in the taxa. The other levels are derived from information contained in the occurrence dataset. Thus, the default order of confidence levels is:

 * Level 1 - species is identified by a specialist, if not;
 * Level 2 - species is identified by a taxonomist, if not;
 * Level 3 - occurrence record has an image associated, if not;
 * Level 4 - the specimen is preserved in a scientific collection, if not;
 * Level 5 - the identification was done in filed observation, if not;
 * Level 6 - no criteria was met.
 
The user can alter this order, depending on his/her objectives, except for the Level 1 that is always a species determined by a specialist. 

## Installation

You can install the released version of naturaList from github:

``` r
install.packages("devtools")
install_github("avrodrigues/naturaList", build_vignettes = T)
```

## Example

To conduct a classification you have to provide two data frames to `classify_occ()` function. The first containing the occurrences records and the second with a list of specialists.
The `classify_occ()` function add a new column in the occurrences dataset named `naturaList_levels`, which contains the classification.

``` r
library(naturaList)
data("A.setosa")
data("speciaLists")

occ.cl <- classify_occ(A.setosa, speciaLists)

```

## Other resources

See [vignette](https://avrodrigues.github.io/naturaList/articles/natutaList_vignette.html).


