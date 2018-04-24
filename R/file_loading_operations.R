library(dplyr)

# Initializes variables that the file loading functions
# rely on.
#
# @param input_files_dir path to the input files
# @param system_landscape SAP system landscape id (SID)
#        that is used to prefix the file name automatically
init_file_loading <- function(input_files_dir,
                              system_landscape) {
  RBW_INPUT_FILES_DIR <<- input_files_dir
  RBW_SYSTEM_LANDSCAPE <<- system_landscape
}

# Reads a csv file that has been saved from a SAP table
# as a "local file" with the "unconverted" setting.
#
# In this case the file is full of | chars and there is
# a header like that at the top of the file.
#
# @param filename the name of the file with extension
# @return tbl_df
# @examples
# collection <- NULL
# collect_chars_from_string("abc", collection)
load_csv_local_unconverted <- function(filename) {
  if (!exists("RBW_INPUT_FILES_DIR")) {
    stop("RBW_INPUT_FILES_DIR must be set before using this function, call init_file_loading")
  }
  if (exists("RBW_SYSTEM_LANDSCAPE") && !is.na(RBW_SYSTEM_LANDSCAPE)) {
    filename <- paste0(RBW_SYSTEM_LANDSCAPE,
                      "_",
                      filename)
  }
  df <- read.csv(file = file.path(RBW_INPUT_FILES_DIR,
                                  filename),
                         skip = 3,
                         sep = "|",
                         dec = ",",
                         na.strings = "",
                         stringsAsFactors = FALSE)
  df <- tbl_df(df)
  # Removing unnecessarily created rows and columns
  df <- df[3:nrow(df)-1, 3:ncol(df)-1]
  # Cleaning column names
  colnames(df) <- strip_x_bic(colnames(df))
  return(df)
}

# Reads a csv file that has been saved from a SAP table
# as a "local file" with the "unconverted" setting.
#
# In this case the file is full of | chars and there is
# a header like that at the top of the file.
#
# @param filename the name of the file with extension
# @return tbl_df
# @examples
# collection <- NULL
# collect_chars_from_string("abc", collection)
load_csv_tab_separated <- function(filename) {
  if (!exists("RBW_INPUT_FILES_DIR")) {
    stop("RBW_INPUT_FILES_DIR must be set before using this function, call init_file_loading")
  }
  if (exists("RBW_SYSTEM_LANDSCAPE") && !is.na(RBW_SYSTEM_LANDSCAPE)) {
    filename <- paste0(RBW_SYSTEM_LANDSCAPE,
                       "_",
                       filename)
  }
  df <- read.csv(file = file.path(RBW_INPUT_FILES_DIR,
                                  filename),
                 sep = "\t",
                 dec = ",",
                 na.strings = "",
                 stringsAsFactors = FALSE)
  df <- tbl_df(df)
  # Removing unnecessarily created rows and columns
  # df <- df[3:nrow(df)-1, 3:ncol(df)-1]
  # Cleaning column names
  colnames(df) <- strip_x_bic(colnames(df))
  return(df)
}

# Collects special characters uniquely from a vector
#
# This can be used to observe which (special) characters
# hinder further processing.
# Every character that is not a-z, A-Z, 0-9 is collected
#
# @param vec a vector of values to collect from
# @return a vector of collected special characters
# @examples
# vec <- c("ab_", "de%")
# get_special_chars_from_vec(vec)
#
# @export
get_special_chars_from_vec <- function(vec) {
  spec_values <- gsub("([a-z]|[A-Z]|[0-9])",
                      "",
                      vec)
  collection <- NULL
  loc_collect_special_chars <- function(value) {
    return(collect_chars_from_string(string = value,
                                     collection = collection))
  }
  for (i in 1:length(spec_values)) {
    collection <- loc_collect_special_chars(spec_values)
  }
  return(collection)
}
