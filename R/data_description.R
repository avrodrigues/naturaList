#' Occurrence records of Alsophila setosa downloaded from Global Biodiversity
#' Information Facility (GBIF).
#'
#' A GBIF raw dataset contaning 508 occurrence records for the tree fern
#' Alsophila setosa.
#'
#' @format A data frame with 508 rows and 45 variables
#'
#' @source GBIF.org (08 July 2019) GBIF Occurrence Download \url{https://doi.org/10.15468/dl.6jesg0}
"A.setosa"

#' Specialists of ferns and lycophytes of Brazil
#'
#' A dataset containing the specialists of ferns and lycophytes of Brazil formated
#' to be used by \code{naturaList} package.
#'
#' @details This data serves as a format example for \code{spec} argument in
#' \code{\link{classify_occ}}
#'
#' @format A data frame with 27 rows and 8 columns:
#' \describe{
#'   \item{LastName}{Last name of the specialist.}
#'   \item{Name}{Columns with the names of specialist. Could be repeated as long
#'   as needed. In this data \code{Name} was repeated four times.}
#'   \item{Abbrev}{Columns with the abbreviation (one character) of the names of
#'    specialists. Could be repeated as long as needed. In this data \code{Abbrev}
#'     was repeated three times.}
#'   }
#'
#'   @source The specialists names was derived from the authors of paper:
#'   \url{http://dx.doi.org/10.1590/2175-7860201566410}
"speciaLists"
