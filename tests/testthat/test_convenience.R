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
    d[4, , drop = FALSE]
  )
  expect_equivalent(
    get_sanity_checks()[["example"]][[3]],
    d[c(1, 4), , drop = FALSE]
  )
})
# TODO: check more meta-information. see sc_cols_non_NA

test_that("sc_col_elements can set param_name and data_name",  {
  expect_equal(get_sanity_checks()[["param_name"]],
               c("a", "a", "user-param-name"))
  expect_equal(get_sanity_checks()[["data_name"]],
               c("d", "d", "user-data-name"))
})

test_that(
  "sc_col_elements can set description and generates a separate description", {
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
  sc_cols_non_NA(object = x,
                 description = "Check for all cols",
                 counter_meas = "nada")
}
dummy_call(x = d)

test_that("sc_cols_non_NA counts correctly all columns", {
  expect_equivalent(
    get_sanity_checks()[, c("n", "n_fail", "n_na")],
    data.table::data.table(n = 4,
                           n_fail = 0:2,
                           n_na = 0))
})

test_that("sc_cols_non_NA correct meta information", {
  expect_equivalent(
    get_sanity_checks()[, c("description", "additional_desc",
                            "data_name", "counter_meas",
                            "param_name", "call")],
    data.table::data.table(
      description = "Check for all cols",
      additional_desc = paste0("Check that column '",
                               c("id", "a", "b"),
                               "' does not contain NA"),
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
  expect_error(sc_cols_non_NA(object = d, cols = c("d", "e")),
               regexp = msg_regexp)
  expect_warning(
    sc_cols_non_NA(object = d,
                   cols = c("a", "d", "e"),
                   unk_cols_callback = warning),
    regexp = msg_regexp
  )
  expect_equivalent(
    get_sanity_checks()[, c("n", "n_fail", "n_na", "param_name")],
    data.table::data.table(n = 4,
                           n_fail = 1,
                           n_na = 0,
                           param_name = "a")
  )
})



# sc_cols_unique -----------------------------------------------------


clear_sanity_checks()
d <- data.frame(
  col1 = c(1:2, 1:2, NA, NA, NA),
  a = c(NA, 1, NA, 2, NA, NA, NA),
  b = c(NA, letters[2:3], NA, NA, NA, NA))
dummy_call <- function(x) {
  sc_cols_unique(object = x,
                 description = "Check for duplicate entries",
                 counter_meas = "nada")
  sc_cols_unique(object = x, cols = c("col1", "a"), example_size = Inf)
}
dummy_call(x = d)

test_that("sc_cols_unique counts correctly all columns and subset of columns", {
  expect_equivalent(
    get_sanity_checks()[, c("n", "n_fail", "n_na")],
    data.table::data.table(n = 7,
                           n_fail = c(3, 5),
                           n_na = 0))
})

.param_name <- c(sanityTracker:::h_collapse_char_vec(c("col1", "a", "b")),
                sanityTracker:::h_collapse_char_vec(c("col1", "a")))
test_that("sc_cols_unique correct meta information", {
  expect_equivalent(
    get_sanity_checks()[, c("description", "additional_desc",
                            "data_name", "counter_meas", "param_name",
                            "call")],
    data.table::data.table(
      description = c("Check for duplicate entries", "-"),
      additional_desc = paste("The combination of", .param_name, "is unique"),
      data_name = "x",
      counter_meas = c("nada", "-"),
      param_name = .param_name,
      call = "dummy_call(x = d)"
    )
  )
})


# sc_left_join -----------------------------------------------------


clear_sanity_checks()
left <- data.frame(a = 1:4, b = 4:1, c = 11:14)
right <- data.frame(a = c(1:4, 1), b = c(4:1, 1), d = 11:15)
joined <- merge(x = left, y = right, by = c("a", "b"), all.x = TRUE)
desc1 <- "Left join OK"
merge_vars1 <- c("a", "b")
joined2 <- merge(x = left, y = right, by = c("a"), all.x = TRUE)
desc2 <- "Left join with duplicates"
merge_vars2 <- c("a")
dummy_call <- function(x) {
  sc_left_join(joined = joined, left = left, right = right,
               description = desc1, counter_meas = "nada",
               by = merge_vars1)
  sc_left_join(joined = joined2, left = left, right = right,
               description = desc2,
               by = merge_vars2)
}
dummy_call(x = d)

test_that("sc_left_join counts correctly", {
  expect_equivalent(
    get_sanity_checks()[, c("n", "n_fail", "n_na")],
    data.table::data.table(n = c(4, 1, 1,
                                 5, 1, 1),
                           n_fail = c(0, 0, 0,
                                      2, 1, 1),
                           n_na = 0))
})

ex <- get_sanity_checks()[["example"]][[4]]
ex[[".n_col_cmb"]] <- NULL
test_that("sc_cols_unique extract examples correctly", {
  expect_equivalent(
    ex,
    data.table::as.data.table(joined2[1:2, ])
  )
  expect_equivalent(
    get_sanity_checks()[["example"]][[6]],
    data.table::data.table(
      cols = sanityTracker:::h_collapse_char_vec(v = c("b.x", "b.y")))
  )
})


test_that("sc_left_join correct meta information", {
  expect_equivalent(
    get_sanity_checks()[1:3, c("description", "additional_desc",
                               "fail_vec_str", "data_name",
                               "counter_meas", "param_name", "call")],
    data.table::data.table(
      description = desc1,
      additional_desc = c(
        sprintf("The combination of %s is unique",
                sanityTracker:::h_collapse_char_vec(v = merge_vars1)),
        "nrow(joined table) = 4 equals nrow(left table) = 4",
        "No columns were duplicated by the left join"
      ),
      fail_vec_str = c("dt$.n_col_cmb != 1", "n_joined != n_left",
                       "length(duplicated_columns) > 0"),
      data_name = "joined, left, right",
      counter_meas = "nada",
      param_name = sprintf("Merge-vars: %s",
                           sanityTracker:::h_collapse_char_vec(merge_vars1)),
      call = "dummy_call(x = d)"
    )
  )
  expect_equivalent(
    get_sanity_checks()[-c(1:3), c("description", "additional_desc",
                                  "fail_vec_str", "data_name",
                                  "counter_meas", "param_name", "call")],
    data.table::data.table(
      description = desc2,
      additional_desc = c(
        sprintf("The combination of %s is unique",
                sanityTracker:::h_collapse_char_vec(v = merge_vars2)),
        "nrow(joined table) = 5 equals nrow(left table) = 4",
        "No columns were duplicated by the left join"
      ),
      fail_vec_str = c("dt$.n_col_cmb != 1", "n_joined != n_left",
                       "length(duplicated_columns) > 0"),
      data_name = "joined2, left, right",
      counter_meas = "-",
      param_name = sprintf("Merge-vars: %s",
                           sanityTracker:::h_collapse_char_vec(merge_vars2)),
      call = "dummy_call(x = d)"
    )
  )
})

# sc_cols_bounded -----------------------------------------------------

clear_sanity_checks()
d <- data.frame(a = c(-Inf, 0, -3L), b = c(-1, 1, 3L), c = c(1, 2, Inf))

dummy_call <- function(x) {
  sc_cols_bounded(object = x, cols = names(x), rule = "[-Inf, Inf)",
                  description = "all columns", counter_meas = "nada")
  sc_cols_bounded(object = x, cols = "a", rule = "(-Inf, 0)")
  sc_cols_bounded(object = x, cols = "b", rule = "[1, 3]")
  sc_cols_bounded(object = x, cols = "c", rule = "(1, 3)")
}
dummy_call(x = d)
get_sanity_checks()


test_that("sc_cols_bounded counts correctly", {
  expect_equivalent(
    get_sanity_checks()[, c("n", "n_fail", "n_na")],
    data.table::data.table(n = 3,
                           n_fail = c(0, 0, 1,
                                      2,
                                      1,
                                      2),
                           n_na = 0))
})


test_that("sc_cols_bounded correct meta information", {
  expect_equivalent(
    get_sanity_checks()[1:3, c("description", "additional_desc",
                               "data_name", "counter_meas",
                               "param_name", "call")],
    data.table::data.table(
      description = "all columns",
      additional_desc = paste0("Elements in '", letters[1:3],
                               "' should be in [-Inf, Inf)."),
      data_name = "x",
      counter_meas = "nada",
      param_name = letters[1:3],
      call = "dummy_call(x = d)"
    )
  )
  expect_equivalent(
    get_sanity_checks()[-c(1:3), c("description", "additional_desc",
                                  "data_name", "counter_meas",
                                  "param_name", "call")],
    data.table::data.table(
      description = "-",
      additional_desc = paste0("Elements in '", letters[1:3],
                               "' should be in ",
                               c("(-Inf, 0)", "[1, 3]", "(1, 3)"),
                               "."),
      data_name = "x",
      counter_meas = "-",
      param_name = letters[1:3],
      call = "dummy_call(x = d)"
    )
  )
})


# sc_cols_positive -----------------------------------------------------

clear_sanity_checks()
d <- data.frame(a = c(0, 1))

dummy_call <- function(x) {
  sc_cols_positive(object = x, cols = "a", description = "pos")
  sc_cols_positive(object = x, cols = "a", zero_feasible = FALSE,
                   counter_meas = "nada")
}
dummy_call(x = d)
get_sanity_checks()


test_that("sc_cols_positive counts correctly", {
  expect_equivalent(
    get_sanity_checks()[, c("n", "n_fail", "n_na")],
    data.table::data.table(n = 2,
                           n_fail = c(0, 1),
                           n_na = 0))
})


test_that("sc_cols_positive correct meta information", {
  expect_equivalent(
    # additional_desc and param_name - come from sc_cols_bounded and test there
    get_sanity_checks()[, c("description", "data_name",
                            "counter_meas", "call")],
    data.table::data.table(
      description = c("pos", "-"),
      data_name = "x",
      counter_meas = c("-", "nada"),
      call = "dummy_call(x = d)"
    )
  )
})


# sc_cols_bounded_below -----------------------------------------------------

clear_sanity_checks()
d <- data.frame(a = c(0, 1))

dummy_call <- function(x) {
  sc_cols_bounded_below(
    object = x, cols = "a",
    lower_bound = 1, description = "bounded")
  sc_cols_bounded_below(
    object = x, cols = "a",
    lower_bound = 0, include_lower_bound = FALSE,
    counter_meas = "nada")
}
dummy_call(x = d)
get_sanity_checks()


test_that("sc_cols_bounded_below counts correctly", {
  expect_equivalent(
    get_sanity_checks()[, c("n", "n_fail", "n_na")],
    data.table::data.table(n = 2,
                           n_fail = c(1, 1),
                           n_na = 0))
})


test_that("sc_cols_bounded_below correct meta information", {
  expect_equivalent(
    # additional_desc and param_name - come from sc_cols_bounded and test there
    get_sanity_checks()[, c("description", "data_name",
                            "counter_meas", "call")],
    data.table::data.table(
      description = c("bounded", "-"),
      data_name = "x",
      counter_meas = c("-", "nada"),
      call = "dummy_call(x = d)"
    )
  )
})



# sc_cols_bounded_above -----------------------------------------------------

clear_sanity_checks()
d <- data.frame(a = c(0, 1))

dummy_call <- function(x) {
  sc_cols_bounded_above(
    object = x, cols = "a",
    upper_bound = 0, description = "bounded")
  sc_cols_bounded_above(
    object = x, cols = "a",
    upper_bound = 1, include_upper_bound = FALSE,
    counter_meas = "nada")
}
dummy_call(x = d)
get_sanity_checks()


test_that("sc_cols_bounded_above counts correctly", {
  expect_equivalent(
    get_sanity_checks()[, c("n", "n_fail", "n_na")],
    data.table::data.table(n = 2,
                           n_fail = c(1, 1),
                           n_na = 0))
})


test_that("sc_cols_bounded_above correct meta information", {
  expect_equivalent(
    # additional_desc and param_name - come from sc_cols_bounded and test there
    get_sanity_checks()[, c("description", "data_name",
                            "counter_meas", "call")],
    data.table::data.table(
      description = c("bounded", "-"),
      data_name = "x",
      counter_meas = c("-", "nada"),
      call = "dummy_call(x = d)"
    )
  )
})
