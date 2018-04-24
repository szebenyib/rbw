context("Column name operations")

test_that("strip_leading", {
  result <- strip_leading(chars_to_strip = "AB",
                          text = "ABC")
  expect_equal(result,
               "C")

  result <- strip_leading(chars_to_strip = "",
                          text = "ABC")
  expect_equal(result,
               "ABC")

  result <- strip_leading(chars_to_strip = "ABC",
                          text = "ABC")
  expect_equal(result,
               "")

  result <- strip_leading(chars_to_strip = "ABCD",
                          text = "ABC")
  expect_equal(result,
               "ABC")

  result <- strip_leading(chars_to_strip = "ABC",
                          text = c("ABCD", "ABCE"))
  expect_equal(result,
               c("D", "E"))
})

test_that("strip_trailing", {
  result <- strip_trailing(chars_to_strip = "BC",
                           text = "ABC")
  expect_equal(result,
               "A")

  result <- strip_trailing(chars_to_strip = "",
                           text = "ABC")
  expect_equal(result,
               "ABC")

  result <- strip_trailing(chars_to_strip = "ABC",
                           text = "ABC")
  expect_equal(result,
               "")

  result <- strip_trailing(chars_to_strip = "ABCD",
                           text = "ABC")
  expect_equal(result,
               "ABC")

  result <- strip_trailing(chars_to_strip = "D",
                           text = c("ABCD", "BCDD"))
  expect_equal(result,
               c("ABC", "BCD"))
})

test_that("strip_x_bic", {
  result <- strip_x_bic("X.BIC.ABC")
  expect_equal(result,
               "ABC")

  result <- strip_x_bic("ABC")
  expect_equal(result,
               "ABC")

  result <- strip_x_bic("")
  expect_equal(result,
               "")

  result <- strip_x_bic(c("X.BIC.ABC", "X.BIC.DEF"))
  expect_equal(result,
               c("ABC", "DEF"))
})

test_that("strip_x0wm_", {
  result <- strip_x0wm_("X0WM_ABC")
  expect_equal(result,
               "ABC")

  result <- strip_x0wm_("ABC")
  expect_equal(result,
               "ABC")

  result <- strip_x0wm_("")
  expect_equal(result,
               "")

  result <- strip_x0wm_(c("X0WM_ABC", "X0WM_DEF"))
  expect_equal(result,
               c("ABC", "DEF"))
})

test_that("strip_x0", {
  result <- strip_x0("X0ABC")
  expect_equal(result,
               "ABC")

  result <- strip_x0("ABC")
  expect_equal(result,
               "ABC")

  result <- strip_x0("")
  expect_equal(result,
               "")

  result <- strip_x0(c("X0ABC", "X0DEF"))
  expect_equal(result,
               c("ABC", "DEF"))
})

test_that("strip_ending_markers_after_join", {
  result <- strip_ending_markers_after_join("ABC.x")
  expect_equal(result,
               "ABC")

  result <- strip_ending_markers_after_join("ABC")
  expect_equal(result,
               "ABC")

  result <- strip_ending_markers_after_join("")
  expect_equal(result,
               "")

  result <- strip_ending_markers_after_join(c("ABC.x", "DEF.x"))
  expect_equal(result,
               c("ABC", "DEF"))

  result <- strip_ending_markers_after_join(c("ABC.x", "DEF.y"))
  expect_equal(result,
               c("ABC", "DEF"))
})

test_that("conversion_exit_alpha_remove0", {
  result <- conversion_exit_alpha_remove0("000ABC")
  expect_equal(result,
               "ABC")

  result <- conversion_exit_alpha_remove0("000123")
  expect_equal(result,
               "123")

  result <- conversion_exit_alpha_remove0("000ABC0")
  expect_equal(result,
               "ABC0")

  result <- conversion_exit_alpha_remove0("0001230")
  expect_equal(result,
               "1230")

  result <- conversion_exit_alpha_remove0("0")
  expect_equal(result,
               "")

  result <- conversion_exit_alpha_remove0("ABC")
  expect_equal(result,
               "ABC")

  result <- conversion_exit_alpha_remove0("")
  expect_equal(result,
               "")

  result <- conversion_exit_alpha_remove0(c("0ABC", "0DEF"))
  expect_equal(result,
               c("ABC", "DEF"))
})
