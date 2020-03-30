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

#' Collapse a vector of characters to a string with separators
#'
#' @param v vector of chars to be collapsed
#' @param collapse character that separates the elements in the returned object
#' @param qoute character that surronds every element in \code{v} in the
#'   returned object
#'
#' @return collapsed version of \code{v}
#'
#' @examples
#' cat(h_collapse_char_vec(v = letters[1:4]))
h_collapse_char_vec <- function(v, collapse = ", ", qoute = "'") {
  paste(qoute, v, qoute, collapse = collapse, sep = "")
}
