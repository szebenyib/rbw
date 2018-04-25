library(dplyr)

# Compares whether the two data frames have the same column names and
# in the same order
#
# @param x the first dataframe
# @param y the second dataframe
# @param x_name optional name of the first dataframe, used for messages
# @param y_name optional name of the second dataframe, used for messages
# @return TRUE if the dataframes have the same column names
# @examples
# compare_same_colnames(x = x,
#                       y = y,
#                       x_name = "first",
#                       y_name = "second")
compare_same_colnames <- function(x, y, x_name = "x", y_name = "y") {
  x_colnames <- colnames(x)
  y_colnames <- colnames(y)
  if (length(x_colnames) != length(y_colnames)) {
    stop(paste0(x_name, " and ", y_name,
        " do not contain the same number of columns, ",
        x_name, " has ", length(x_colnames), ", while ",
        y_name, " has ", length(y_colnames)))
  }
  if (!all(x_colnames %in% y_colnames)) {
    stop(paste0(y_name, " did not contain: ",
                paste(x_colnames[!x_colnames %in% y_colnames],
                      collapse = " ")))
  }
  if (!all(y_colnames %in% x_colnames)) {
    stop(paste0(x_name, " did not contain: ",
                paste(y_colnames[!y_colnames %in% x_colnames],
                      collapse = " ")))
  }
  if (all(x_colnames == y_colnames)) {
    result <- TRUE
  } else {
    stop(paste0(x_name, " and ", y_name,
                " have their columns in different order"))
  }
}

# Compares whether the two data frames have the same column types
# (in the same order)
#
# @param x the first dataframe
# @param y the second dataframe
# @param x_name optional name of the first dataframe, used for messages
# @param y_name optional name of the second dataframe, used for messages
# @return TRUE if the dataframes have the same column types
# @examples
# compare_same_coltypes(x = x,
#                       y = y,
#                       x_name = "first",
#                       y_name = "second")
compare_same_coltypes <- function(x, y, x_name = "x", y_name = "y") {
  if (paste(sapply(x, typeof), collapse = " ") ==
      paste(sapply(y, typeof), collapse = " ")) {
    return(TRUE)
  } else {
    stop(paste(x_name, "and", y_name, "have different coltypes.",
               x_name, "has", paste(sapply(x, typeof),
                                    collapse = " "), "while",
               y_name, "has", paste(sapply(y, typeof),
                                    collapse = " ")))
  }
}

# Compares whether the two data frames have the same number of rows
#
# @param x the first dataframe
# @param y the second dataframe
# @param x_name optional name of the first dataframe, used for messages
# @param y_name optional name of the second dataframe, used for messages
# @return TRUE if the dataframes have the same number of rows
# @examples
# compare_same_rowcount(x = x,
#                       y = y,
#                       x_name = "first",
#                       y_name = "second")
compare_same_rowcount <- function(x, y, x_name = "x", y_name = "y") {
  if (nrow(x) == nrow(y)) {
    return(TRUE)
  } else {
    stop(paste(x_name, "and", y_name, "have different number of rows.",
               x_name, "has", nrow(x), "while", y_name, "has", nrow(y)))
  }
}

# Compares whether the two data frames have:
# - the same column names in the same order
# - the same column types
# - the same number of rows
#
# @param x the first dataframe
# @param y the second dataframe
# @param x_name optional name of the first dataframe, used for messages
# @param y_name optional name of the second dataframe, used for messages
# @param check_rowcount optional, default TRUE
# @return TRUE if the dataframes have the same structure
# @examples
# compare_same_structure(x = x,
#                        y = y,
#                        x_name = "first",
#                        y_name = "second")
compare_same_structure <- function(x, y, x_name = "x", y_name = "y",
                                   check_rowcount = TRUE) {
  compare_same_colnames(x = x, y = y, x_name = x_name, y_name = y_name)
  compare_same_coltypes(x = x, y = y, x_name = x_name, y_name = y_name)
  if (check_rowcount) {
    compare_same_rowcount(x = x, y = y, x_name = x_name, y_name = y_name)
  }
  return(TRUE)
}

# Compares whether the two data frames are identical regarding structure
# and content, excluding ordering
#
# @param x the first dataframe
# @param y the second dataframe
# @param x_name optional name of the first dataframe, used for messages
# @param y_name optional name of the second dataframe, used for messages
# @return TRUE if the dataframes are identical
# @examples
# compare_same_content(x = x,
#                        y = y,
#                        x_name = "first",
#                        y_name = "second")
compare_same_content <- function(x, y, x_name = "x", y_name = "y") {
  compare_same_structure(x = x, y = y, x_name = x_name, y_name = y_name)
  cols <- colnames(x)
  delta_df <- anti_join(x = x,
                        y = y,
                        by = cols)
  if (nrow(delta_df) == 0) {
    return(TRUE)
  } else {
    stop(paste("The two dataframes have the same structure but",
               "they have different content"))
  }
}
