#' Extends a list with an named element if the element does not exist
#'
#' @param ell list to be extended (usually an ellipsis as list(...))
#' @param name character with the name for the element to be added
#' @param value value that will be stored in \code{ell[[el_name]]}
#'
#' @return if \code{ell} already contained the element \code{name}, then
#'   \code{ell} is returned without being modified. Otherwise, \code{ell}
#'   is returned extended by a new element with name \code{name} and value 
#'   \code{value}.
#'
#' @examples 
#' ell <- list(a = 1, b = 2)
#' sanityTracker:::h_complete_list(ell = ell, name = "a", value = 100)
#' sanityTracker:::h_complete_list(ell = ell, name = "d", value = Inf)
h_complete_list <- function(ell, name, value){
  ret <- ell
  if (!name %in% names(ret)) {
    ret[[name]] <- value
  }
  return(ret)
}