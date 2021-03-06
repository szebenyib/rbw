context("Comparison operations")

test_that("compare_same_colnames", {
  x <- mtcars
  y <- mtcars[, 1:ncol(mtcars) - 1]
  expect_error(compare_same_colnames(x = x, y = y,
                                     x_name = "STD", y_name = "LAST_MISSING"),
               paste0("STD and LAST_MISSING do not contain the same number",
                      " of columns, STD has 11, while LAST_MISSING has 10"))

  # Without names
  x <- mtcars
  y <- mtcars[, 1:ncol(mtcars) - 1]
  expect_error(compare_same_colnames(x = x, y = y),
               paste0("x and y do not contain the same number",
                      " of columns, x has 11, while y has 10"))

  # Changing order
  x <- mtcars
  y <- cbind(mtcars[, ncol(mtcars)],
             mtcars[, 1:ncol(mtcars) - 1])
  x_names <- colnames(x)
  colnames(y) <- c(
    x_names[length(x_names)],
    x_names[1:length(x_names) - 1])
  expect_error(compare_same_colnames(x = x, y = y,
                                     x_name = "STD", y_name = "CHANGED_ORDER"),
               "STD and CHANGED_ORDER have their columns in different order")

  # Different column names
  x <- mtcars
  y <- mtcars
  x_names <- colnames(x)
  colnames(y) <- c(
    "foo",
    x_names[1:length(x_names) - 1])
  expect_error(compare_same_colnames(x = x, y = y,
                                     x_name = "STD", y_name = "ALT"),
               "ALT did not contain: carb")

  x <- mtcars
  y <- mtcars
  x_names <- colnames(x)
  colnames(x) <- c(
    "foo", "bar",
    x_names[3:length(x_names) - 1])
  expect_error(compare_same_colnames(x = x, y = y,
                                     x_name = "STD", y_name = "ALT"),
               "ALT did not contain: foo bar")

  # Identical
  x <- mtcars
  y <- mtcars
  expect_true(compare_same_colnames(x = x, y = y,
                                    x_name = "STD", y_name = "CLONE"))
})

test_that("compare_same_coltypes", {
  x <- mtcars
  y <- mtcars
  y$mpg <- as.character(y$mpg)
  expect_error(compare_same_coltypes(x = x, y = y,
                                  x_name = "STD", y_name = "CHR"),
               paste("STD and CHR have different coltypes.",
                    "STD has double double double double double double double",
                    "double double double double while CHR has character",
                    "double double double double double double double",
                    "double double double"))

  x <- mtcars
  y <- mtcars
  expect_true(compare_same_coltypes(x = x, y = y,
                                    x_name = "STD", y_name = "CLONE"))
})

test_that("compare_same_rowcount", {
  x <- mtcars
  y <- mtcars[1:nrow(mtcars)-1,]
  expect_error(compare_same_rowcount(x = x, y = y,
                                     x_name = "STD", y_name = "REDUCED"),
               paste("STD and REDUCED have different number of rows.",
                     "STD has 32 while REDUCED has 31"))

  x <- mtcars
  y <- mtcars
  expect_true(compare_same_rowcount(x = x, y = y,
                                    x_name = "STD", y_name = "CLONE"))
})

test_that("compare_same_structure", {
  # Colnames
  x <- mtcars
  y <- mtcars[, 1:ncol(mtcars) - 1]
  expect_error(compare_same_structure(x = x, y = y,
                                     x_name = "STD", y_name = "LAST_MISSING"),
               paste0("STD and LAST_MISSING do not contain the same number",
                      " of columns, STD has 11, while LAST_MISSING has 10"))

  # Coltypes
  x <- mtcars
  y <- mtcars
  y$mpg <- as.character(y$mpg)
  expect_error(compare_same_structure(x = x, y = y,
                                     x_name = "STD", y_name = "CHR"),
               paste("STD and CHR have different coltypes.",
                     "STD has double double double double double double double",
                     "double double double double while CHR has character",
                     "double double double double double double double",
                     "double double double"))

  # Rowcount
  x <- mtcars
  y <- mtcars[1:nrow(mtcars)-1,]
  expect_error(compare_same_structure(x = x, y = y,
                                      x_name = "STD", y_name = "REDUCED"),
               paste("STD and REDUCED have different number of rows.",
                     "STD has 32 while REDUCED has 31"))
  expect_true(compare_same_structure(x = x, y = y,
                                     x_name = "STD",
                                     y_name = "REDUCED",
                                     check_rowcount = FALSE))

  # Identical
  x <- mtcars
  y <- mtcars
  expect_true(compare_same_structure(x = x, y = y,
                                     x_name = "STD", y_name = "CLONE"))
})

test_that("compare_same_content", {
  # One row differs
  x <- mtcars
  y <- mtcars
  y$mpg[5] <- 35
  expect_error(compare_same_content(x = x, y = y,
                                      x_name = "STD", y_name = "CHANGED"),
               paste("The two dataframes have the same structure but",
                     "they have different content"))

  # Same content in reversed order
  x <- mtcars
  y <- mtcars[seq(dim(mtcars)[1], 1), ]
  expect_true(compare_same_content(x = x, y = y,
                                   x_name = "STD", y_name = "REVERSED"))

  # Same content
  x <- mtcars
  y <- mtcars
  expect_true(compare_same_content(x = x, y = y,
                                   x_name = "STD", y_name = "CLONE"))
})

test_that("get_delta_rows", {
  # Identical dfs
  x <- mtcars
  y <- mtcars
  x$key <- rownames(x)
  y$key <- rownames(y)
  delta_df <- get_delta_rows(x = x, y = y)
  expect_equal(nrow(delta_df), 0)
  delta_df <- get_delta_rows(x = x, y = y, compare_cols = "key")
  expect_equal(nrow(delta_df), 0)
  delta_df <- get_delta_rows(x = x, y = y, compare_cols = "key",
                             sample_size = 5, x_name = "STD", y_name = "CLONE")
  expect_equal(nrow(delta_df), 0)

  # Different structure dfs
  x <- mtcars
  y <- mtcars
  x$key <- rownames(x)
  expect_error(get_delta_rows(x = x, y = y),
               paste("x and y do not contain the same number of columns,",
                     "x has 12, while y has 11"))

  # X Different in first row
  x <- mtcars
  y <- mtcars
  x$key <- rownames(x)
  y$key <- rownames(y)
  x[1, 1] <- 123
  delta_df <- get_delta_rows(x = x, y = y)
  manual_delta_df <- rbind(x[1, ], y[1, ])
  manual_delta_df$source <- c("x", "y")
  manual_delta_df$id <- 1
  manual_delta_df <- manual_delta_df[c(ncol(manual_delta_df),
                                       ncol(manual_delta_df) - 1,
                                       1:(ncol(manual_delta_df) - 2))]
  rownames(manual_delta_df) <- NULL
  expect_equal(delta_df, manual_delta_df)

  # X Different in last row
  x <- mtcars
  y <- mtcars
  x$key <- rownames(x)
  y$key <- rownames(y)
  x[nrow(x), ncol(x)] <- "ABC"
  delta_df <- get_delta_rows(x = x, y = y)
  manual_delta_df <- rbind(x[nrow(x), ], y[nrow(y), ])
  manual_delta_df$source <- c("x", "y")
  manual_delta_df$id <- 1
  manual_delta_df <- manual_delta_df[c(ncol(manual_delta_df),
                                       ncol(manual_delta_df) - 1,
                                       1:(ncol(manual_delta_df) - 2))]
  rownames(manual_delta_df) <- NULL
  expect_equal(delta_df, manual_delta_df)

  # X Different in first and last rows
  x <- mtcars
  y <- mtcars
  x$key <- rownames(x)
  y$key <- rownames(y)
  x[1, 1] <- 123
  x[nrow(x), ncol(x)] <- "ABC"
  delta_df <- get_delta_rows(x = x, y = y)
  manual_delta_df <- rbind(x[1, ], y[1, ], x[nrow(x), ], y[nrow(y), ])
  manual_delta_df$source <- c("x", "y")
  manual_delta_df$id <- c(1, 1, 2, 2)
  manual_delta_df <- manual_delta_df[c(ncol(manual_delta_df),
                                       ncol(manual_delta_df) - 1,
                                       1:(ncol(manual_delta_df) - 2))]
  rownames(delta_df) <- NULL
  rownames(manual_delta_df) <- NULL
  expect_equal(nrow(anti_join(delta_df,
                              manual_delta_df,
                              by = "key")),
               0)

  # X Different in first row, in the column supplied for comparison
  x <- mtcars
  y <- mtcars
  x$key <- rownames(x)
  y$key <- rownames(y)
  x[1, 1] <- 123
  delta_df <- get_delta_rows(x = x, y = y, compare_cols = c("cyl", "disp"))
  expect_equal(nrow(delta_df), 0)

  # X Different in first row, but not in the column supplied for comparison
  # Also only x will be present since y has a match via the comparison column
  x <- mtcars
  y <- mtcars
  x$key <- rownames(x)
  y$key <- rownames(y)
  x[1, 1] <- 123
  delta_df <- get_delta_rows(x = x, y = y, compare_cols = c("mpg", "disp"))
  manual_delta_df <- rbind(x[1, ])
  manual_delta_df$source <- "x"
  manual_delta_df$id <- 1
  manual_delta_df <- manual_delta_df[c(ncol(manual_delta_df),
                                       ncol(manual_delta_df) - 1,
                                       1:(ncol(manual_delta_df) - 2))]
  rownames(manual_delta_df) <- NULL
  expect_equal(delta_df, manual_delta_df)

  # X Different in two rows, but only one is requested via sample_size
  x <- mtcars
  y <- mtcars
  x$key <- rownames(x)
  y$key <- rownames(y)
  x[1, 1] <- 123
  x[nrow(x), ncol(x)] <- "ABC"
  delta_df <- get_delta_rows(x = x, y = y, sample_size = 1)

})
