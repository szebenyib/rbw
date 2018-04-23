context("Global operations")

test_that("rmgc", {
  expect_equal(ls(), character(0))

  foo <<- 1
  expect_true("foo" %in% ls(pos = ".GlobalEnv"))

  rmgc()
  expect_true("foo" %in% ls(pos = ".GlobalEnv"),
              info = "Without explicitly setting RBW_CLEANUP to true,
                      rmgc should not delete")

  RBW_CLEANUP <<- TRUE
  rmgc("foo")
  expect_false("foo" %in% ls(pos = ".GlobalEnv"))

  rmgc("bar")
  expect_false("bar" %in% ls(pos = ".GlobalEnv"),
               "Trying to remove a non-existing variable should not cause
               errors")

  foo <<- 1
  bar <<- 2
  rmgc("foo", "bar")
  expect_false("foo" %in% ls(pos = ".GlobalEnv") &&
              "bar" %in% ls(pos = ".GlobalEnv"),
              "Capable of removing multiple variables")

  foo <<- 1
  bar <<- 2
  rmgc(foo)
  expect_false("foo" %in% ls(pos = ".GlobalEnv") &&
               "bar" %in% ls(pos = ".GlobalEnv"),
               "Capable of removing non-string argument")
})
