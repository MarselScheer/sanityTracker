
clear_sanity_checks()
add_sanity_check(
  fail_vec = c(T, F, NA),
  description = "A")
add_sanity_check(
  fail_vec = c(NA, NA), #edge-case
  description = "A")

expect_equivalent(
  current = get_sanity_checks()[1, c("n", "n_fail", "n_na")],
  target = data.table::data.table(n = 3, n_fail = 1, n_na = 1),
  info = "Number of checks, fails and NAs"
)

expect_equivalent(
  current = get_sanity_checks()[2, c("n", "n_fail", "n_na")],
  target = data.table::data.table(n = 2, n_fail = 0, n_na = 2),
  info = "Number of checks, fails and NAs"
)

expect_error(
  add_sanity_check(
    fail_vec = c(T, F, NA),
    description = "A",
    fail_callback = stop),
  info = "fail_callback is called if any fails happen"
)

clear_sanity_checks()
add_sanity_check(
  fail_vec = c(F, T),
  description = "A",
  data = data.frame(a = 1:2))

expect_equivalent(
  current = get_sanity_checks()[["example"]],
  target = list(data.frame(a = 2)),
  info = "Example is stored"
)

expect_equal(
  current = get_sanity_checks()$data_name,
  target = "data.frame(a = 1:2)",
  info = "Name of the data set is stored"
)
