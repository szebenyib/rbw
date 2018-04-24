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
