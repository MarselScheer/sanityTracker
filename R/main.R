TRACKER_ENV <- new.env()

#' Adds a sanity check to the list of already performed sanity checks
#'
#' @param fail_vec logical vector where \code{TRUE} indicates that a
#'   fail has happend
#' @param description (optional) of the sanity check. default is "-".
#' @param counter_meas (optional) description of the counter measures that
#'   were applied to correct the problems. default is "-".
#' @param data (optional) where the fails were found. Is used to
#'   store examples of failures. default is "-".
#' @param data_name (optional) name of the data set that was used. defaults is
#'   the name of the object passed to data.
#' @param example_size (optional) number failures to be extracted from the
#'   object passed to \code{data}. By default 3 random examples are extracted.
#' @param param_name (optional) name of the parameter(s) that is used. This may
#'   be helpful for filtering the table of all performed sanity checks.
#' @param call (optional) by default tracks the function that called
#'   \link{add_sanity_check}.
#' @param fail_callback (optional) user-defined function that is called if
#'   any element of \code{fail_vec} is \code{TRUE}. This is helpful if an
#'   additional warning or error should be thrown or maybe a log-entry
#'   should be created.
#'
#' @return a list with three elements
#'   \describe{
#'     \item{entry_sanity_table}{invisibly the sanity check that is stored
#'     internally with the other sanity checks}
#'     \item{fail_vec}{\code{fail_vec} as passed over to this function}
#'     \item{fail}{TRUE if any element of fail is TRUE. Otherwise FALSE.}
#'   }
#'   All performed sanity checks can be fetched via \link{get_sanity_checks}
#' @export
#'
#' @examples
#' d <- data.frame(person_id = 1:4, bmi = c(18,23,-1,35), age = 31:34)
#' dummy_call <- function(x) {
#'   add_sanity_check(
#'     x$bmi < 15,
#'     description = "bmi above 15",
#'     counter_meas = "none",
#'     data = x,
#'     param_name = "bmi")
#'   add_sanity_check(
#'     x$bmi > 30,
#'     description = "bmi below 30",
#'     counter_meas = "none")
#' }
#' dummy_call(x = d)
#' get_sanity_checks()
#' add_sanity_check(
#'    d$bmi < 15,
#'    description = "bmi above 15",
#'    fail_callback = warning)
add_sanity_check <- function(
  fail_vec, description = "-", counter_meas = "-",
  data, data_name = checkmate::vname(x = data),
  example_size = 3,
  param_name = "-", call = h_deparsed_sys_call(which = -3),
  fail_callback) {

  ret <-
    # NOTE: also data and fail_callback can be "missing" it works
    #       to pass these parameters to the next function
    .add_sanity_check(
      fail_vec = fail_vec, description = description,
      counter_meas = counter_meas, data = data,
      data_name = data_name, example_size = example_size,
      param_name = param_name, call = call,
      fail_callback = fail_callback,
      .fail_vec_str = checkmate::vname(x = fail_vec),
      .generated_desc = "-"
    )
  return(ret)
}

#' Adds a sanity check to the list of already performed sanity checks
#'
#' NOTE the also add_sanity_check calls this function, the parameters
#' are documented in add_sanity_check because that function gets
#' exported.
#' @param fail_vec see \link{add_sanity_check}
#' @param description see \link{add_sanity_check}
#' @param counter_meas see \link{add_sanity_check}
#' @param data see \link{add_sanity_check}
#' @param data_name see \link{add_sanity_check}
#' @param example_size see \link{add_sanity_check}
#' @param param_name see \link{add_sanity_check}
#' @param call see \link{add_sanity_check}
#' @param fail_callback see \link{add_sanity_check}
#' @param .fail_vec_str should capture what was used originally for
#'   \code{fail_vec}.
#' @param .generated_desc for convenience functions like
#'   \link{sc_col_elements} to provide additional information about
#'   the check.
#'
#' @return see \link{add_sanity_check}
#' @import data.table
.add_sanity_check <- function(
  fail_vec, description, counter_meas,
  data,
  data_name, example_size,
  param_name, call,
  fail_callback,
  .fail_vec_str,
  .generated_desc) {


  if (any(fail_vec, na.rm = TRUE) & !missing(fail_callback)) {
    fail_callback(sprintf("%s/%s: FAILED", description, .generated_desc))
  }


  row <- data.table::data.table(
    description = description,
    additional_desc = .generated_desc,
    data_name = data_name,
    n = length(fail_vec),
    n_fail = sum(fail_vec, na.rm = TRUE),
    n_na = sum(is.na(fail_vec)),
    counter_meas = counter_meas,
    fail_vec_str = .fail_vec_str,
    param_name = param_name,
    call = call)

  if (!missing(data) & any(fail_vec, na.rm = TRUE)) {
    # add some examples where the fail occured

    idx <- which(fail_vec)
    if (length(idx) == 1) {
      fail_example <- idx
    } else {
      fail_example <- which(fail_vec)
      # avoid random sampling here in order to not interfere
      # with a simulation or sth. similar
      fail_example <- fail_example[seq_len(min(row$n_fail, example_size))]
    }

    # drop = FALSE is for the case that fail_example contains only 1 number
    # a data.frame may reduce otherwise to a vector.
    row$example <- list(list(data[fail_example, , drop = FALSE]))
  }

  TRACKER_ENV[["checks"]] <- data.table::rbindlist(
    list(TRACKER_ENV[["checks"]], row),
    use.names = TRUE,
    fill = TRUE
  )

  return(invisible(list(
    entry_sanity_table = row,
    fail_vec = fail_vec,
    fail = any(fail_vec, na.rm = TRUE)
  ))
  )
}


#' Returns all performed sanity checks
#'
#' @return all sanity checks, i.e. a data.table with the following column
#' \describe{
#'   \item{description}{character that was provided by the user through the
#'     parameter \code{description}}
#'   \item{additional_desc}{character that provides additional information
#'     about the check that was generated by the convenience functions}
#'   \item{data_name}{name of the data set that passed to the function that
#'     performed the sanity check. This can also be specified by the user}
#'   \item{n}{a logical vector is the basis of all sanity checks. This is
#'     length of the logical vector that was used, which in general is the
#'     number of rows of the table that was checked}
#'   \item{n_fail}{how often the logical vector was TRUE}
#'   \item{n_na}{how often the logical vector was NA}
#'   \item{counter_meas}{character provided by the user about how a fail will
#'     be addressed. Note that this never happens inside a function of
#'     \code{sanityTracker} but is realized by the user after the check was
#'     performed. It is only for documentation when the results of the checks
#'     are displayed.}
#'   \item{fail_vec_str}{this captures how the actual logical vector of fails
#'     was build}
#'   \item{param_name}{usually generated by the convenience functions and it
#'     also may be a composition of more than one parameter name. However this
#'     parameter could also have been provided by the user}
#'   \item{call}{character of the call where the sanity check happend}
#'   \item{example}{if a check failed and the table is available,
#'     then some examples of rows that lead to the fail are stored here}
#' }
#' @seealso \link{add_sanity_check}
#' @export
get_sanity_checks <- function() {
  return(TRACKER_ENV[["checks"]])
}

#' Removes all tracked sanity checks
#'
#' @return NULL
#' @export
clear_sanity_checks <- function() {
  TRACKER_ENV[["checks"]] <- NULL
}
