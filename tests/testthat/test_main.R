testthat::context("add and get sanity checks")

clear_sanity_checks()
add_sanity_check(
  fail_vec = c(T, F, NA),
  description = "A")
add_sanity_check(
  fail_vec = c(NA, NA), #edge-case
  description = "A")
test_that(
  "Number of checks, fails and NAs", {
    expect_equivalent(
      object = get_sanity_checks()[1, c("n", "n_fail", "n_na")],
      expected = data.table::data.table(n = 3, n_fail = 1, n_na = 1)
    )
    expect_equivalent(
      object = get_sanity_checks()[2, c("n", "n_fail", "n_na")],
      expected = data.table::data.table(n = 2, n_fail = 0, n_na = 2)
    )
  }
)

test_that(
  "fail_callback is called if any fails happen", {
    expect_error(
      add_sanity_check(
        fail_vec = c(T, F, NA),
        description = "A",
        fail_callback = stop)
    )
  }
)
 
clear_sanity_checks()
add_sanity_check(
  fail_vec = c(F, T),
  description = "A",
  data = data.frame(a = 1:2))
test_that(
  "Example is stored",
  expect_equivalent(
    object = get_sanity_checks()[["example"]],
    expected = list(data.frame(a = 2))
  )
)
test_that(
  "Name of the data set is stored",
  expect_equal(
    object = get_sanity_checks()$data_name,
    expected = "data.frame(a = 1:2)"
  )
)
