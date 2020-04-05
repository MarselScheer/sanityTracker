testthat::context("Convenience functions")

clear_sanity_checks()
d <- data.frame(
  a = c(letters[1:3], NA),
  b = 1:4
)
sc_col_elements(
    object = d, 
    col = "a",
    feasible_elements = letters[1:4])
sc_col_elements(
  object = d, 
  col = "a",
  feasible_elements = c(letters[1:4], NA))
sc_col_elements(
  object = d, 
  col = "a",
  feasible_elements = c(letters[2:3]),
  param_name = "a")
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
    get_sanity_checks()[1][["example"]][[1]],
    d[4,,drop=FALSE]
  )
  expect_equivalent(
    get_sanity_checks()[3][["example"]][[1]],
    d[c(1,4),,drop=FALSE]
  )
})

test_that("sc_col_elements can set param_name",  {
  expect_equal(get_sanity_checks()[["param_name"]], c("", "", "a"))
})

