testthat::context("Convenience functions")

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

