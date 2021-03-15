#' Get the names in the 'determined.by' column
#'
#' This function facilitates the search for non-taxonomist strings in the
#' 'determined.by' column of occurrence records dataset
#'
#' @param occ Data frame with occurrence records information.
#' @param determined.by Column name of \code{occ} with the name of who determined the
#'  species.
#' @param freq Logical. If TRUE output contain the number of times each string is
#'  repeated in the 'determined.by' column. Default FALSE.
#' @param decreasing logical. sort strings in decreasing order of frequency. Default =
#'  TRUE
#'
#' @return Character vector containing characters in \code{determined.by}
#'     column of \code{occ}.
#'
#' @export

get_det_names <- function(occ,
                          determined.by = "identifiedBy",
                          freq = FALSE,
                          decreasing = TRUE){
    det.string <- names(table(occ[, determined.by]))

  if(freq){
    det.string <-   table(occ[, determined.by])[order(table(occ[, determined.by]),
                                                      decreasing = decreasing)]
  }

  return(det.string)
}
