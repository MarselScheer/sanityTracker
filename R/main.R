TRACKER_ENV <- new.env()

#' Adds a sanity check to the list of already performed sanity checks
#'
#' @param fail_vec logical vector where TRUE indicates that a fail has happend
#' @param description of the sanity check
#' @param counter_meas description of the counter measures that were applied
#'   to correct the problems
#' @param data optional. Data set where the fails were found. Is used to store examples
#'   of failures
#' @param example_size number failures to be extracted from the object passed to the parameter
#'   data. By default 3 random examples are extracted.
#' @param call by default tracks the function that called add_sanity_check.
#' @param fail_callback user-defined function that is called if \code{any(fail_vec)} is \code{TRUE}.
#'   This is helpful if an additional warning or error should be thrown or maybe a
#'   log-entry should be created.
#'
#' @return invisibly the sanity check that is stored internally with the other sanity checks.
#'   All performed sanity checks can be fetched via \link{get_sanity_checks}
#' @export
#'
#' @examples
#' d <- data.frame(person_id = 1:4, bmi = c(18,23,-1,35), age = 31:34)
#' add_sanity_check(d$bmi < 15, description = "bmi above 15", counter_meas = "none",
#'   data = d)
#' add_sanity_check(d$bmi > 30, description = "bmi below 30", counter_meas = "none")
#' get_sanity_checks()
#' add_sanity_check(d$bmi < 15, description = "bmi above 15", counter_meas = "none",
#'   data = d, fail_callback = warning)
add_sanity_check <- function(
  fail_vec, description, counter_meas = "None", data, example_size = 3, 
  call = deparse(sys.call(which = -1)),
  fail_callback) {

  if (any(fail_vec, na.rm = TRUE) & !missing(fail_callback)) {
    fail_callback(sprintf("%s: FAILED", description))
  }
  
  row <- data.table::data.table(
    description = description,
    n = length(fail_vec),
    n_fail = sum(fail_vec, na.rm = TRUE),
    n_na = sum(is.na(fail_vec)),
    counter_meas = counter_meas,
    call = call)

  if (!missing(data) & any(fail_vec, na.rm = TRUE)) {
    # add some examples where the fail occured

    idx <- which(fail_vec)
    if (length(idx) == 1) {
      fail_example <- idx
    } else {
      fail_example <- sample(which(fail_vec), size = min(example_size, row$n_fail))
    }

    # drop = FALSE is for the case that fail_example contains only 1 number
    # a data.frame may reduce otherwise to a vector.
    row$example <- list(data[fail_example, , drop = FALSE])
  }

  TRACKER_ENV[["checks"]] <- data.table::rbindlist(
    list(TRACKER_ENV[["checks"]], row),
    use.names = TRUE,
    fill = TRUE
  )
  
  return(invisible(row))
}

#' Returns all performed sanity checks
#'
#' @return all sanity checks
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
