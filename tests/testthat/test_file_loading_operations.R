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
    filename = "tests/testdata/sap_bic_agh_mm_1000_all_tab.txt"),
    "RBW_INPUT_FILES_DIR must be set before using this function, call init_file_loading")

  RBW_INPUT_FILES_DIR <<- file.path("..", "testdata")
  df <- load_csv_local_unconverted(filename = "gh_mm_2000_unconverted.txt")
  expect_equal(nrow(df), 10)
  expect_equal(ncol(df), 17)
  expect_false(any(grepl("BIC", colnames(df))),
               "Column names contain BIC, they were not cleaned")
})

