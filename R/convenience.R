#' Checks that the elements of a column belong to a certain set
#'
#' @param object table with a column specified by \code{col}
#' @param col name as a character of the column which is checked
#' @param feasible_elements vector with characters that are feasible
#'   for \code{col}. Note that an element that is NA it is always
#'   counted as a fail if \code{feasible_elements} does not 
#'   explicitly contains NA.
#' @param ... further parameters that are passed to \link{add_sanity_check}.
#'
#' @return see return object of \link{add_sanity_check}
#' @export
#' @examples
#' d <- data.frame(type = letters[1:4], nmb = 1:4)
#' sc_col_elements(object = d, col = "type", feasible_elements = letters[2:4])
#' get_sanity_checks()
sc_col_elements <- function(object, col, feasible_elements,
                            ...) {

  checkmate::assert_data_frame(x = object, min.rows = 1)
  checkmate::qassert(x = col, rules = "s1")
  checkmate::assert_subset(x = col, choices = names(object))
  
  ret <-
    h_add_sanity_check(
      ellipsis = list(...),
      fail_vec = !(object[[col]] %in% feasible_elements),
      .generated_desc = sprintf(
        "Elements in '%s' should contain only %s.",
        col,
        h_collapse_char_vec(feasible_elements)
      ),
      data = object,
      data_name = checkmate::vname(x = object),
      param_name = col)
 
  return(invisible(ret))
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
sc_cols_positive <- function(object, cols, zero_feasible = TRUE, ...) {

}

#' Checks that all elements from the given columns are above a certain number
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
sc_cols_bounded_below <- function(object, cols,
                                  include_lower_bound = TRUE, ...) {

}

#' Checks that all elements from the given columns are above a certain number
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
sc_cols_bounded_above <- function(object, cols,
                                  include_upper_bound = TRUE, ...) {

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
sc_cols_bounded <- function(object, cols, include_lower_bound = TRUE,
                            include_upper_bound = TRUE, ...) {

}



#' Checks that all elements from the specified columns are not NA
#'
#' @param object table with a columns specified by \code{cols}
#' @param cols vector of characters of columns that are checked for NAs
#' @param ... further parameters that are passed to \link{add_sanity_check}.
#' @param unk_cols_callback user-defined function that is called if
#'   some of the \code{cols} are not contained in the \code{object}. 
#'   This is helpful if an additional warning or error should be thrown 
#'   or maybe a log-entry should be created. Default is the function 
#'   \code{stop}
#'
#' @return a list where every element is an object returned by 
#'   \link{add_sanity_check} for each column specified in \code{cols}
#'   that exists in \code{object}
#' @export
#'
#' @examples
#' iris[c(1,3,5,7,9), 1] <- NA
#' sc_cols_non_NA(object = iris, description = "No NAs expected in iris")
#' get_sanity_checks()
sc_cols_non_NA <- function(object, cols = names(object), ...,
                           unk_cols_callback = stop) {

  checkmate::assert_data_frame(x = object, min.rows = 1)
  checkmate::qassert(x = cols, rules = "s+")
  checkmate::assert_function(x = unk_cols_callback)
  
  all_cols_known <- checkmate::check_subset(x = cols, choices = names(object))
  if (!isTRUE(all_cols_known)) {
    unk_cols_callback(all_cols_known)
  }
  DATA_NAME <- checkmate::vname(x = object)

  
  # treat only the columns that actually exist in object
  cols <- unique(intersect(cols, names(object)))
  ret <- lapply(cols, function(col) {
    h_add_sanity_check(
      ellipsis = list(...),
      fail_vec = is.na(object[[col]]),
      .generated_desc = sprintf("Check that column '%s' does not contain NA",
                                col),
      data = object,
      data_name = DATA_NAME,
      param_name = col,
      call = deparse(sys.call(which = -3))
    )
  })
  names(ret) <- cols
  
  return(invisible(ret))
}

#' Checks that the combination of the specified columns is unique
#'
#' @param object table with a columns specified by \code{cols}
#' @param cols vector of characters which combination is checked to be unique
#' @param ... further parameters that are passed to \link{add_sanity_check}.
#'
#' @return logical vector where TRUE indicates where the check failed.
#'   This might be helpful if one wants to apply a counter-measure.
#' @export
#' @import data.table
#'
#' @examples
#' sc_cols_unique(object = iris, cols = c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length"))
#' get_sanity_checks()
#' get_sanity_checks()[["example"]]
sc_cols_unique <- function(object, cols = names(object), ...) {

  checkmate::assert_data_frame(x = object, min.rows = 1)
  checkmate::qassert(x = cols, rules = "s+")
  checkmate::assert_subset(x = cols, choices = names(object))
  
  
  dt <- data.table::as.data.table(x = object)
  dt[, ..sanity_N := .N, by = cols]
  ret <-
    h_add_sanity_check(
      ellipsis = list(...),
      fail_vec = dt$..sanity_N != 1,
      description = sprintf("The combination of %s is unique",
                            h_collapse_char_vec(v = cols)),
      data = dt)
  return(ret)
}

#' Performs various checks after a left-join was performed
#'
#' One check is that no rows were duplicated during merge
#' and the other check is that no columns were duplicated
#' during merge.
#' @param joined the result of the left-join
#' @param left the left table used in the left-join
#' @param right the right table used in the left-join
#' @param by the variables used for the left-join
#' @param ... further parameters that are passed to \link{add_sanity_check}.
#' @param find_nonunique_key if TRUE a sanity-check is performed
#'   that finds keys (defined by \code{by}) that are non-unique.
#'   However this can be a time-consuming step. If FALSE only
#'   the number of rows of the left table with the merged table
#'   is compared.
#'
#' @return list with two elements for the two sanity checks performed
#'   by this function. The structure of each element is as the
#'   return object of \link{add_sanity_check}.
#' @export
#'
#' @examples
#' ab <- data.table::data.table(a = 1:4, b = letters[1:4])
#' abc <- data.table::data.table(a = c(1:4, 2), b = letters[1:5], c = rnorm(5))
#' j <- merge(x = ab, y = abc, by = "a")
#' sc_left_join(joined = j, left = ab, right = abc, by = "a")
#' get_sanity_checks()
sc_left_join <- function(joined, left, right, by, ..., find_nonunique_key = TRUE) {

  checkmate::assert_data_frame(x = joined, min.rows = 1)
  checkmate::assert_data_frame(x = left, min.rows = 1)
  checkmate::assert_data_frame(x = right, min.rows = 1)
  checkmate::qassert(x = by, rules = "s+")
  
  # use param_name in the table of sanity-checks to store
  # information about the variables that were used for the merge
  PARAM_NAME <- sprintf("Merge-vars: %s", h_collapse_char_vec(v = by))
  
  if (find_nonunique_key) {
    ret_uniq <- sc_cols_unique(object = joined, cols = by, 
                   call = deparse(sys.call(which = -2)), 
                   ...)
  } else {
    # if user dont want to find rows where the key is not unique
    # one should at least know how many additional rows compared to
    # the "left" table are now present
    n_joined = nrow(joined)
    n_left = nrow(left)
    ret_uniq <- h_add_sanity_check(
      ellipsis = list(...),
      fail_vec = n_joined != n_left,
      description = sprintf(
        "nrow(joined table) = %i equals nrow(left table) = %i", 
        n_joined, 
        n_left),
      param_name = PARAM_NAME
    )  
  }
  
  
  duplicated_columns <- setdiff(names(joined), names(left))
  duplicated_columns <- setdiff(duplicated_columns, names(right))
  ret_dbl_col <- h_add_sanity_check(
    ellipsis = list(...),
    fail_vec = length(duplicated_columns) > 0,
    description = "No columns were duplicated by the left join",
    # make it as data.frame because example extraction assumes that data is a
    # data.frame
    data = data.table::data.table(
      cols = h_collapse_char_vec(v = duplicated_columns)
    ),
    param_name = PARAM_NAME
  )
  
  list(ret_uniq, ret_db_col)
}



