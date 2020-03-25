#' get the names in the 'determined.by' column
#'
#' This function facilitates the search for non-taxonomist strings in the
#' 'determined.by' column of occurrence records dataset
#'
#' @param occ Data frame with occurrence records information.
#' @param determined.by Column name of \code{occ} with the name of who determined the
#'  species.
#'
#'

get_det_names <- function(occ, determined.by = "identifiedBy"){
  names(table(occ.df[, determined.by]))
}
