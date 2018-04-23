context("Value operations")

test_that("collect_chars_from_string function", {
  collection <- NULL
  collection <- collect_chars_from_string(string = "",
                                          collection = collection)
  expect_equal(collection,
               NULL)

  collection <- NULL
  collection <- collect_chars_from_string(string = "abc",
                                          collection = collection)
  expect_equal(collection,
               c("a", "b", "c"))

  collection <- NULL
  collection <- collect_chars_from_string(string = "#&@ ",
                                          collection = collection)
  expect_equal(collection,
               c("#", "&", "@", " "))
})

test_that("get_special_chars_from_vec", {
  expect_equal(get_special_chars_from_vec("abc"),
               NULL)

  expect_equal(get_special_chars_from_vec("abc_"),
               "_")

  expect_equal(get_special_chars_from_vec("§qwe*rtz%uio_p@$ "),
               c("§", "*", "%", "_", "@", "$", " "))
})
