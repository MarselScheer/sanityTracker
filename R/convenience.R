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
#' dummy_call <- function(x) {
#'   sc_col_elements(object = d, col = "type", feasible_elements = letters[2:4])
#' }
#' dummy_call(x = d)
#' get_sanity_checks()
sc_col_elements <- function(object, col, feasible_elements,
                            ...) {

  checkmate::assert_data_frame(x = object, min.rows = 1)
  checkmate::qassert(x = col, rules = "s1")
  checkmate::assert_subset(x = col, choices = names(object))
  .call <- h_deparsed_sys_call(which = -1)

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
      param_name = col,
      call = .call)

  return(invisible(ret))
}


#' Checks that all elements from the specified columns are positive
#'
#' @param object table with a columns specified by \code{cols}
#' @param cols vector of characters of columns that are checked against the
#'   specified range
#' @param zero_feasible if zero is in the range or not
#' @param ... further parameters that are passed to \link{add_sanity_check}.
#'
#' @return list of logical vectors where TRUE indicates where the check failed.
#'   Every list entry represents one of the columns specified in cols.
#'   This might be helpful if one wants to apply a counter-measure.
#' @export
#'
#' @examples
#' d <- data.frame(a = c(0, 0.2, 3, Inf), b = c(1:4))
#' dummy_call <- function(x) {
#'   sc_cols_positive(d, cols = c("a", "b"), zero_feasible = FALSE,
#'     description = "Measurements are expected to be positive")
#' }
#' dummy_call(x = d)
#' get_sanity_checks()
sc_cols_positive <- function(object, cols, zero_feasible = TRUE, ...) {

  # TODO: could use sc_cols_bounded_below, but need to refactor how
  #       data_name is used because sc_cols_bounded_below sets
  #       data_name itself
  rule <- "(0, Inf)"
  if (isTRUE(zero_feasible)) {
    rule <- "[0, Inf)"
  }
  .call <- h_deparsed_sys_call(which = -1)
  ret <- sc_cols_bounded(object = object, cols = cols, rule = rule,
                  call = .call,
                  data_name = checkmate::vname(x = object),
                  ...)
  return(ret)
}

#' Checks that all elements from the given columns are above a certain number
#'
#' @param object table with a columns specified by \code{cols}
#' @param cols vector of characters of columns that are checked against
#'   the specified range
#' @param lower_bound elements of the specified columns must be above this
#'   bound
#' @param include_lower_bound if TRUE (default), elements are allowed to be
#'   equal to the \code{lower_bound}
#' @param ... further parameters that are passed to \link{add_sanity_check}.
#'
#' @return list of logical vectors where TRUE indicates where the check failed.
#'   Every list entry represents one of the columns specified in cols.
#'   This might be helpful if one wants to apply a counter-measure
#' @export
#'
#' @examples
#' d <- data.frame(a = c(0, 0.2, 3, Inf), b = c(1:4))
#' dummy_call <- function(x) {
#'   sc_cols_bounded_below(
#'     object = d, cols = c("a", "b"),
#'     lower_bound = 0.2,
#'     include_lower_bound = FALSE,
#'     description = "Measurements are expected to be bounded from below")
#' }
#' dummy_call(x = d)
#' get_sanity_checks()
sc_cols_bounded_below <- function(object, cols,
                                  lower_bound,
                                  include_lower_bound = TRUE, ...) {

  .left <- "("
  if (isTRUE(include_lower_bound)) {
    .left <- "["
  }
  rule <- sprintf("%s%s, Inf)", .left, lower_bound)

  .call <- h_deparsed_sys_call(which = -1)
  ret <- sc_cols_bounded(object = object, cols = cols, rule = rule,
                         call = .call,
                         data_name = checkmate::vname(x = object),
                         ...)
  return(ret)
}

#' Checks that all elements from the given columns are below a certain number
#'
#' @param object table with a columns specified by \code{cols}
#' @param cols vector of characters of columns that are checked against
#'   the specified range
#' @param upper_bound elements of the specified columns must be below this
#'   bound
#' @param include_upper_bound if TRUE (default), elements are allowed to be
#'   equal to the \code{upper_bound}
#' @param ... further parameters that are passed to \link{add_sanity_check}.
#'
#' @return list of logical vectors where TRUE indicates where the check failed.
#'   Every list entry represents one of the columns specified in cols.
#'   This might be helpful if one wants to apply a counter-measure
#' @export
#'
#' @examples
sc_cols_bounded_above <- function(object, cols,
                                  upper_bound,
                                  include_upper_bound = TRUE, ...) {

  .right <- ")"
  if (isTRUE(include_upper_bound)) {
    .right <- "]"
  }
  rule <- sprintf("(-Inf, %s%s", upper_bound, .right)

  .call <- h_deparsed_sys_call(which = -1)
  ret <- sc_cols_bounded(object = object, cols = cols, rule = rule,
                         call = .call,
                         data_name = checkmate::vname(x = object),
                         ...)
  return(ret)

}


#' Checks that all elements from the specified columns are in a certain range
#'
#' @param object table with a columns specified by \code{cols}
#' @param cols vector of characters of columns that are checked against
#'   the specified range
#' @param rule check as two numbers separated by a comma, enclosed by square
#'   brackets (endpoint included) or parentheses (endpoint excluded).
#'   For example, “[0, 3)” results in all(x >= 0 & x < 3).
#'   The lower and upper bound may be omitted which is the equivalent
#'   of a negative or positive infinite bound, respectively.
#'   By definition [0,] contains Inf, while [0,)
#'   does not. The same holds for the left (lower) boundary and -Inf.
#'   This explanation was copied from \code{checkmate::qtest}. That function
#'   is also the backbone of the this function.
#' @param ... further parameters that are passed to \link{add_sanity_check}.
#'
#' @return list of logical vectors where TRUE indicates where the check failed.
#'   Every list entry represents one of the columns specified in cols.
#'   This might be helpful if one wants to apply a counter-measure
#' @export
#'
#' @examples
#' dummy_call <- function(x) {
#'   sc_cols_bounded(object = iris, cols = c("Sepal.Length", "Petal.Length"),
#'     rule = "[1, 7.9)")
#' }
#' dummy_call(x = d)
#' get_sanity_checks()
sc_cols_bounded <- function(object, cols, rule = "(-Inf, Inf)", ...) {
                            #lower_limit, upper_limit,
                            #include_lower_bound = TRUE,
                            #include_upper_bound = TRUE, ...) {

  checkmate::assert_data_frame(x = object, min.rows = 1)
  checkmate::qassert(x = cols, rules = "s+")
  checkmate::assert_subset(x = cols, choices = names(object))
  checkmate::qassert(x = rule, rules = "s1")

  .user_rule <- rule
  .data_name <- checkmate::vname(x = object)
  .call <- h_deparsed_sys_call(which = -1)
  rule <- paste0("n", rule)


  ret <- lapply(cols, function(col) {
    h_add_sanity_check(
      ellipsis = list(...),
      fail_vec = sapply(
        object[[col]], function(x) !checkmate::qtest(x = x, rules = rule)
      ),
      .generated_desc = sprintf(
        "Elements in '%s' should be in %s.",
        col, .user_rule
      ),
      data = object,
      data_name = .data_name,
      param_name = col,
      call = .call)
  })
  names(ret) <- cols

  return(invisible(ret))

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
#' dummy_call <- function(x) {
#'   sc_cols_non_NA(object = iris, description = "No NAs expected in iris")
#' }
#' dummy_call(x = iris)
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
  .data_name <- checkmate::vname(x = object)
  .call <- h_deparsed_sys_call(which = -1)

  # treat only the columns that actually exist in object
  cols <- unique(intersect(cols, names(object)))
  ret <- lapply(cols, function(col) {
    h_add_sanity_check(
      ellipsis = list(...),
      fail_vec = is.na(object[[col]]),
      .generated_desc = sprintf("Check that column '%s' does not contain NA",
                                col),
      data = object,
      data_name = .data_name,
      param_name = col,
      call = .call
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
#' @return see return object of \link{add_sanity_check}. Note that if a
#'   combination appears 3 times, then n_fail will increased by 3.
#' @export
#' @import data.table
#'
#' @examples
#' dummy_call <- function(x) {
#'   sc_cols_unique(
#'     object = x,
#'     cols = c("Species", "Sepal.Length",
#'              "Sepal.Width", "Petal.Length"))
#' }
#' dummy_call(x = iris)
#' get_sanity_checks()
#' get_sanity_checks()[["example"]]
sc_cols_unique <- function(object, cols = names(object), ...) {
  # TODO: allow that every entry appears with the same multiplicity

  checkmate::assert_data_frame(x = object, min.rows = 1)
  checkmate::qassert(x = cols, rules = "s+")
  checkmate::assert_subset(x = cols, choices = names(object))

  .call <- h_deparsed_sys_call(which = -1)

  dt <- data.table::as.data.table(x = object)

  # initialize to avoid 'no visible binding for global variable', see
  # data.table vignettes datatable-importing.html
  .n_col_cmb <- NULL
  dt[, .n_col_cmb := .N, by = cols]

  ret <-
    h_add_sanity_check(
      ellipsis = list(...),
      fail_vec = dt$.n_col_cmb != 1,
      .generated_desc = sprintf("The combination of %s is unique",
                            h_collapse_char_vec(v = cols)),
      data = dt,
      data_name = checkmate::vname(x = object),
      param_name = h_collapse_char_vec(cols),
      call = .call)
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
#'   However this can be a time-consuming step.
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
#' dummy_call <- function() {
#'   sc_left_join(joined = j, left = ab, right = abc, by = "a",
#'     description = "Left join outcome to main population")
#' }
#' dummy_call()
#' get_sanity_checks()
sc_left_join <- function(joined, left, right, by, ...,
                         find_nonunique_key = TRUE) {

  checkmate::assert_data_frame(x = joined, min.rows = 1)
  checkmate::assert_data_frame(x = left, min.rows = 1)
  checkmate::assert_data_frame(x = right, min.rows = 1)
  checkmate::qassert(x = by, rules = "s+")

  # use param_name in the table of sanity-checks to store
  # information about the variables that were used for the merge
  .param_name <- sprintf("Merge-vars: %s", h_collapse_char_vec(v = by))
  .data_name <- sprintf("%s, %s, %s",
                       checkmate::vname(x = joined),
                       checkmate::vname(x = left),
                       checkmate::vname(x = right)
                       )
  .call <- h_deparsed_sys_call(which = -1)

  if (find_nonunique_key) {
    # FIXME: need to use h_complete_list and do("sc_cols_unique", )
    #        in order to not overwrite param_name and data_name
    #        that might be specified by the user
    ret_uniq <- sc_cols_unique(object = joined, cols = by,
                               call = .call,
                               param_name = .param_name,
                               data_name = .data_name,
                               ...)
  }

  # this check does not really provide additional information
  # if find_nonunique_key is TRUE but it dont hurt and it is
  # nice to see the number of rows of the data set.
  n_joined <- nrow(joined)
  n_left <- nrow(left)
  ret_uniq <- h_add_sanity_check(
    ellipsis = list(...),
    fail_vec = n_joined != n_left,
    .generated_desc = sprintf(
      "nrow(joined table) = %i equals nrow(left table) = %i",
      n_joined,
      n_left),
    param_name = .param_name,
    data_name = .data_name,
    call = .call
  )


  duplicated_columns <- setdiff(names(joined), names(left))
  duplicated_columns <- setdiff(duplicated_columns, names(right))
  ret_dbl_col <- h_add_sanity_check(
    ellipsis = list(...),
    fail_vec = length(duplicated_columns) > 0,
    .generated_desc = "No columns were duplicated by the left join",
    # make it as data.frame because example extraction assumes that data is a
    # data.frame
    data = data.table::data.table(
      cols = h_collapse_char_vec(v = duplicated_columns)
    ),
    param_name = .param_name,
    data_name = .data_name,
    call = .call
  )

  list(ret_uniq, ret_dbl_col)
}
