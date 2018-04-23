context("File loading")

test_that("initialization can be performed", {
  init_file_loading(input_files_dir = "a",
                    system_landscape = "b")
  expect_equal(RBW_INPUT_FILES_DIR, "a")
  expect_equal(RBW_SYSTEM_LANDSCAPE, "b")
})
