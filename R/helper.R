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
h_complete_list <- function(ell, name, value) {
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
#' cat(sanityTracker:::h_collapse_char_vec(v = letters[1:4]))
h_collapse_char_vec <- function(v, collapse = ", ", qoute = "'") {
  paste(qoute, v, qoute, collapse = collapse, sep = "")
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
#' @param .generated_desc will be passed to \link{.add_sanity_check} if ellipsis
#'   does not contain a element with name 'description'
#' @param data will be passed to \link{.add_sanity_check} if ellipsis
#'   does not contain a element with name 'data'
#' @param data_name will be passed to \link{.add_sanity_check} if ellipsis
#'   does not contain a element with name 'data_name'
#' @param param_name will be passed to \link{.add_sanity_check} if ellipsis
#'   does not contain a element with name 'param_name'
#' @param call will be passed to \link{.add_sanity_check} if ellipsis
#'   does not contain a element with name 'call'
#' @param .fail_vec_str usually not used by the user. Captures what
#'   was passed to \code{fail_vec}.
#'
#' @return see return value of \link{add_sanity_check}
#' @examples
#' d <- data.frame(type = letters[1:4], nmb = 1:4)
#' # h_add_sanity_check is used on sc_col_elements()
#' sc_col_elements(object = d, col = "type", feasible_elements = letters[2:4])
#' get_sanity_checks()
h_add_sanity_check <- function(ellipsis, fail_vec, .generated_desc, data,
                               data_name = "", param_name = "",
                               call = h_deparsed_sys_call(which = -2),
                               .fail_vec_str = checkmate::vname(x = fail_vec)) {
  # NOTE: counter_meas is not parameter because the convenience functions
  #       that usally call this function do not perform any counter-measures
  ellipsis[["fail_vec"]] <- fail_vec

  ellipsis <- h_complete_list(
    ell = ellipsis,
    name = "description",
    value = "-"
  )

  ellipsis <- h_complete_list(
    ell = ellipsis,
    name = "counter_meas",
    value = "-"
  )

  ellipsis <- h_complete_list(
    ell = ellipsis,
    name = "example_size",
    value = 3
  )

  ellipsis <- h_complete_list(
    ell = ellipsis,
    name = ".generated_desc",
    value = .generated_desc
  )

  if (!missing(data)) {
    ellipsis <- h_complete_list(
      ell = ellipsis,
      name = "data",
      value = data
    )
  }

  ellipsis <- h_complete_list(
    ell = ellipsis,
    name = "data_name",
    value = data_name
  )


  ellipsis <- h_complete_list(
    ell = ellipsis,
    name = "param_name",
    value = param_name
  )

  ellipsis <- h_complete_list(
    ell = ellipsis,
    name = "call",
    value = call
  )

  ellipsis <- h_complete_list(
    ell = ellipsis,
    name = ".fail_vec_str",
    value = .fail_vec_str
  )

  return(do.call(.add_sanity_check, ellipsis))
}


#' Simply converts a call into a character
#'
#' @param which see \link{sys.call}. However the function bounds it by
#'   the number of encolsing environments.
#'
#' @return the call of the corresponding environment as character
h_deparsed_sys_call <- function(which) {
  n_sys_calls <- -length(sys.calls())
  # need -1 to skip the call of this function
  which <- max(n_sys_calls, which - 1)
  ret <- deparse(sys.call(which = which))
  ret <- paste(ret, collapse = " ")
  return(ret)
}
