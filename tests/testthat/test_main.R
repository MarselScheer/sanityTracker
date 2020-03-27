testthat::context("add and get sanity checks")

add_sanity_check(
  fail_vec = c(T, F, NA),
  description = "A")
test_that(
  "Number of checks, fails and NAs",
  expect_equivalent(
    object = get_sanity_checks()[, c("n", "n_fail", "n_na")], 
    expected = data.table::data.table(n = 3, n_fail = 1, n_na = 1)
  )
)

clear_sanity_checks()
add_sanity_check(
  fail_vec = c(F, T),
  description = "A",
  data = data.frame(a = 1:2))
test_that(
  "Number of checks, fails and NAs",
  expect_equivalent(
    object = get_sanity_checks()[["example"]], 
    expected = list(data.frame(a = 2))
  )
)
