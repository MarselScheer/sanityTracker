#' Checks that the elements of a column belong to a certain set
#'
#' @param object table with a column specified by \code{col}
#' @param col name as a character of the column which is checked
#' @param feasible_elements vector with characters that are feasible for \code{col}
#' @param ... further parameters that are passed to \link{add_sanity_check}.
#'
#' @return logical vector where TRUE indicates where the check failed.
#'   This might be helpful if one wants to apply a counter-measure.
#' @export
#'
#' @examples 
#' d <- data.frame(type = letters[1:4], nmb = 1:4)
#' sc_col_elements(object = d, col = "type", feasible_elements = letters[2:4])
#' get_sanity_checks()
sc_col_elements <- function(object, col, feasible_elements, ...){
  
  if (!(col %in% names(object))){
    stop(sprintf("Column '%s' does not exist", col))
  }

  ret <-
    h_add_sanity_check(
      ellipsis = list(...),
      fail_vec = !(object[[col]] %in% feasible_elements),
      description = sprintf(
        "Elements in '%s' should contain only %s.",
        col,
        h_collapse_char_vec(feasible_elements)
      ),
      data = object)
 
  return(ret)
}

#' Checks that all elements from the specified columns are positive
#'
#' @param object 
#' @param cols 
#' @param zero_feasible 
#' @param ... 
#'
#' @return list of logical vectors where TRUE indicates where the check failed.
#'   Every list entry represents one of the columns specified in cols.
#'   This might be helpful if one wants to apply a counter-measure.
#' @export
#'
#' @examples
sc_cols_positive <- function(object, cols, zero_feasible = TRUE, ...){
  
}

#' Checks that all elements from the specified columns are above a certain number
#'
#' @param object 
#' @param cols 
#' @param include_lower_bound 
#' @param ... 
#'
#' @return list of logical vectors where TRUE indicates where the check failed.
#'   Every list entry represents one of the columns specified in cols.
#'   This might be helpful if one wants to apply a counter-measure
#' @export
#'
#' @examples
sc_cols_bounded_below <- function(object, cols, include_lower_bound = TRUE, ...) {
  
}

#' Checks that all elements from the specified columns are above a certain number
#'
#' @param object 
#' @param cols 
#' @param include_upper_bound 
#' @param ... 
#'
#' @return list of logical vectors where TRUE indicates where the check failed.
#'   Every list entry represents one of the columns specified in cols.
#'   This might be helpful if one wants to apply a counter-measure
#' @export
#'
#' @examples
sc_cols_bounded_above <- function(object, cols, include_upper_bound = TRUE, ...) {
  
}


#' Checks that all elements from the specified columns are in a certain range
#'
#' @param object 
#' @param cols 
#' @param include_lower_bound 
#' @param include_upper_bound 
#' @param ... 
#'
#' @return list of logical vectors where TRUE indicates where the check failed.
#'   Every list entry represents one of the columns specified in cols.
#'   This might be helpful if one wants to apply a counter-measure
#' @export
#'
#' @examples
sc_cols_bounded <- function(object, cols, include_lower_bound = TRUE, include_upper_bound = TRUE, ...) {
  
}



#' Checks that all elements from the specified columns are not NA
#'
#' @param object 
#' @param cols 
#' @param ... 
#'
#' @return logical vector where TRUE indicates where the check failed.
#'   This might be helpful if one wants to apply a counter-measure.
#' @export
#'
#' @examples
sc_cols_non_NA <- function(object, cols, ...){
  
}

#' Checks that the combination of the specified columns is unique
#'
#' @param object 
#' @param cols 
#' @param ... 
#'
#' @return logical vector where TRUE indicates where the check failed.
#'   This might be helpful if one wants to apply a counter-measure.
#' @export
#'
#' @examples
sc_cols_unique <- function(object, cols, ...){
  
}

#' Performs various checks after a left-join was performed
#'
#' @param joined 
#' @param left 
#' @param right 
#' @param by 
#' @param ... 
#'
#' @return NULL
#' @export
#'
#' @examples
sc_left_join <- function(joined, left, right, by, ...){
  
}



#' Wrapper for \link{add_sanity_check} for internal use
#'
#' The convenience function usually provide some defaults
#' like description that can be overwritten by the user
#' through the ... argument of the convenience function.
#' This function manages to set those values that were
#' NOT overwritten by the user through the ... argument
#' and then call \link{add_sanity_check}.
#' 
#' @param ellipsis usually list(...) of the function that calls this function.
#'  It contains the parameters defined by the user for add_sanity_check. 
#' @param fail_vec logical vector where \code{TRUE} indicates that a
#'   fail has happend
#' @param description will be passed to \link{add_sanity_check} if ellipsis 
#'   does not contain a element with name 'description'
#' @param data will be passed to \link{add_sanity_check} if ellipsis 
#'   does not contain a element with name 'data'
#' @param call will be passed to \link{add_sanity_check} if ellipsis 
#'   does not contain a element with name 'call'
#'
#' @return see return value of \link{add_sanity_check}
#' @example 
#' d <- data.frame(type = letters[1:4], nmb = 1:4)
#' # h_add_sanity_check is used on sc_col_elements()
#' sc_col_elements(object = d, col = "type", feasible_elements = letters[2:4])
#' get_sanity_checks()
h_add_sanity_check <- function(ellipsis, fail_vec, description, data,
                               call = deparse(sys.call(which = -2))) {
  ellipsis[["fail_vec"]] <- fail_vec
  
  ellipsis <- h_complete_list(
    ell = ellipsis,
    name = "description",
    value = description
  )
  
  ellipsis <- h_complete_list(
    ell = ellipsis,
    name = "data",
    value = data
  )
  
  ellipsis <- h_complete_list(
    ell = ellipsis,
    name = "call",
    value = call
  )
  
  return(do.call(add_sanity_check, ellipsis))
}
