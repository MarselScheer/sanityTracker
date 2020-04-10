testthat::context("Convenience functions")


# sc_col_elements -----------------------------------------------------

clear_sanity_checks()
d <- data.frame(
  a = c(LETTERS[1:3], NA),
  b = 1:4
)
sc_col_elements(
    object = d, 
    col = "a",
    feasible_elements = LETTERS[1:4],
    description = "user desc")
sc_col_elements(
  object = d, 
  col = "a",
  feasible_elements = c(LETTERS[1:4], NA))
sc_col_elements(
  object = d, 
  col = "a",
  feasible_elements = c(LETTERS[2:3]),
  param_name = "user-param-name",
  data_name = "user-data-name")
test_that("sc_col_elements counts (also NA) correctly", {

  expect_equivalent(
    get_sanity_checks()[1, c("n", "n_fail", "n_na")],
    data.table::data.table(n = 4, n_fail = 1, n_na = 0))
  expect_equivalent(
    get_sanity_checks()[2, c("n", "n_fail", "n_na")],
    data.table::data.table(n = 4, n_fail = 0, n_na = 0))
  expect_equivalent(
    get_sanity_checks()[3, c("n", "n_fail", "n_na")],
    data.table::data.table(n = 4, n_fail = 2, n_na = 0))
})

test_that("sc_col_elements examples are extracted correctly", {
  expect_equivalent(
    get_sanity_checks()[["example"]][[1]],
    d[4,,drop=FALSE]
  )
  expect_equivalent(
    get_sanity_checks()[["example"]][[3]],
    d[c(1,4),,drop=FALSE]
  )
})
# TODO: check more meta-information. see sc_cols_non_NA

test_that("sc_col_elements can set param_name and data_name",  {
  expect_equal(get_sanity_checks()[["param_name"]], c("a", "a", "user-param-name"))
  expect_equal(get_sanity_checks()[["data_name"]], c("d", "d", "user-data-name"))
})

test_that("sc_col_elements can set description and generates a separate description",  {
  expect_equal(get_sanity_checks()[["description"]], c("user desc", "-", "-"))
  expect_true(all(
    grepl(pattern = "Elements in 'a' should contain only", 
          x = get_sanity_checks()[["additional_desc"]]
    )
  ))
})

# sc_cols_non_NA -----------------------------------------------------

clear_sanity_checks()
d <- data.frame(id = 1:4, a = c(1:3, NA), b = c(NA, letters[2:3], NA))
dummy_call <- function(x) {
  sc_cols_non_NA(object = x, description = "Check for all cols", counter_meas = "nada")
}
dummy_call(x = d)

test_that("sc_cols_non_NA counts correctly all columns", {
  expect_equivalent(
    get_sanity_checks()[,c("n", "n_fail", "n_na")],
    data.table::data.table(n = 4,
                           n_fail = 0:2,
                           n_na = 0))
})

test_that("sc_cols_non_NA correct meta information", {
  expect_equivalent(
    get_sanity_checks()[,c("description", "additional_desc", "data_name", "counter_meas", "param_name", "call")],
    data.table::data.table(
      description = "Check for all cols",
      additional_desc = paste0("Check that column '", c("id", "a", "b"), "' does not contain NA"),
      data_name = "x",
      counter_meas = "nada",
      param_name = c("id", "a", "b"),
      call = "dummy_call(x = d)"
    )
  )
})

clear_sanity_checks()
msg_regexp <- "subset.*'id'.*'a'.*'b'.*but.*'d'.*'e'"
test_that("call_back functionality works", {
  expect_error(sc_cols_non_NA(object = d, cols = c("d", "e")), regexp = msg_regexp)
  expect_warning(
    sc_cols_non_NA(object = d, cols = c("a","d", "e"), unk_cols_callback = warning), 
    regexp = msg_regexp
  )
  expect_equivalent(
    get_sanity_checks()[,c("n", "n_fail", "n_na", "param_name")],
    data.table::data.table(n = 4,
                           n_fail = 1,
                           n_na = 0,
                           param_name = "a")
  )
})



# sc_cols_unique -----------------------------------------------------


clear_sanity_checks()
d <- data.frame(col1 = c(1:2, 1:2, NA, NA, NA), a = c(NA, 1, NA, 2, NA, NA, NA), b = c(NA, letters[2:3], NA, NA, NA, NA))
dummy_call <- function(x) {
  sc_cols_unique(object = x, description = "Check for duplicate entries", counter_meas = "nada")
  sc_cols_unique(object = x, cols = c("col1", "a"), example_size = Inf)
}
dummy_call(x = d)

test_that("sc_cols_unique counts correctly all columns and subset of columns", {
  expect_equivalent(
    get_sanity_checks()[,c("n", "n_fail", "n_na")],
    data.table::data.table(n = 7,
                           n_fail = c(3,5),
                           n_na = 0))
})

PARAM_NAME <- c(sanityTracker:::h_collapse_char_vec(c("col1", "a", "b")),
                sanityTracker:::h_collapse_char_vec(c("col1", "a")))
test_that("sc_cols_unique correct meta information", {
  expect_equivalent(
    get_sanity_checks()[,c("description", "additional_desc", "data_name", "counter_meas", "param_name", "call")],
    data.table::data.table(
      description = c("Check for duplicate entries", "-"),
      additional_desc = paste("The combination of", PARAM_NAME, "is unique"),
      data_name = "x",
      counter_meas = c("nada", "-"),
      param_name = PARAM_NAME,
      call = "dummy_call(x = d)"
    )
  )
})

