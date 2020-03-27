testthat::context("add and get sanity checks")

add_sanity_check(
  fail_vec = c(T, F),
  description = "A",
  counter_meas = "B")
test_that(
  "Number of checks, fails and NAs",
  expect_equal(
    object = get_sanity_checks()[, c("n", "n_fail", "n_na")], 
    expected = data.table::data.table(n = 2, n_fail = 1, n_na = 0)
  )
)
