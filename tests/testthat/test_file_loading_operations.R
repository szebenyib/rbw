context("File loading operations")

test_that("initialization can be performed", {
  init_file_loading(input_files_dir = "a",
                    system_landscape = "b")
  expect_equal(RBW_INPUT_FILES_DIR, "a")
  expect_equal(RBW_SYSTEM_LANDSCAPE, "b")
  rm("RBW_INPUT_FILES_DIR", "RBW_SYSTEM_LANDSCAPE",
     pos = ".GlobalEnv")
})

test_that("load_csv_local_unconverted", {
  expect_false(exists("RBW_INPUT_FILES_DIR"))
  expect_error(load_csv_local_unconverted(
    filename = file.path("tests", "testdata", "gh_mm_2000_unconverted.txt"),
    "RBW_INPUT_FILES_DIR must be set before using this function, call init_file_loading"))

  RBW_INPUT_FILES_DIR <<- file.path("..", "testdata")
  df <- load_csv_local_unconverted(filename = "gh_mm_2000_unconverted.txt")
  expect_equal(nrow(df), 10)
  expect_equal(ncol(df), 17)
  expect_false(any(grepl("BIC", colnames(df))),
               "Column names contain BIC, they were not cleaned")

  RBW_SYSTEM_LANDSCAPE <<- "sap"
  df <- load_csv_local_unconverted(filename = "gh_mm_2000_unconverted.txt")
  expect_equal(nrow(df), 5)
  expect_equal(ncol(df), 17)
  expect_false(any(grepl("BIC", colnames(df))),
               "Column names contain BIC, they were not cleaned")
  rm("RBW_INPUT_FILES_DIR", "RBW_SYSTEM_LANDSCAPE",
     pos = ".GlobalEnv")
})

test_that("load_csv_tab_separated", {
  expect_false(exists("RBW_INPUT_FILES_DIR"))
  expect_error(load_csv_tab_separated(
    filename = file.path("tests", "testdata", "bic_agh_mm_1000_all_tab.txt"),
    "RBW_INPUT_FILES_DIR must be set before using this function, call init_file_loading"))

  RBW_INPUT_FILES_DIR <<- file.path("..", "testdata")
  df <- load_csv_tab_separated(filename = "bic_agh_mm_1000_all_tab.txt")
  expect_equal(nrow(df), 5)
  expect_equal(ncol(df), 13)
  expect_false(any(grepl("BIC", colnames(df))),
               "Column names contain BIC, they were not cleaned")

  RBW_SYSTEM_LANDSCAPE <<- "sap"
  df <- load_csv_tab_separated(filename = "bic_agh_mm_1000_all_tab.txt")
  expect_equal(nrow(df), 3)
  expect_equal(ncol(df), 13)
  expect_false(any(grepl("BIC", colnames(df))),
               "Column names contain BIC, they were not cleaned")
  rm("RBW_INPUT_FILES_DIR", "RBW_SYSTEM_LANDSCAPE",
     pos = ".GlobalEnv")
})
