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

# Gets the differing rows of the two data frames in a way that
# differing rows will be placed next to each other. Sorting
# and difference making is done key-wise.
#
# @param x the first dataframe
# @param y the second dataframe
# @param compare_cols optional character vector of "key" columns,
#        exclue columns from this vector if those do not matter,
#        e.g.: RECORDMODE
# @param sample_size optional if supplied only maximum this amount of rows
#        will be returned from both dataframes
#        (returned tbl_df will be max two times this). If not supplied then
#        all rows will be returned.
# @param x_name optional name of the first dataframe, used for messages
# @param y_name optional name of the second dataframe, used for messages
# @return tbl_df of differing rows
# @examples
# compare_same_structure(x = x,
#                        y = y,
#                        key_cols = c("a", "b"),
#                        sample_size = 5,
#                        x_name = "first",
#                        y_name = "second")
get_delta_rows <- function(x, y,
                           compare_cols = NULL,
                           sample_size = NULL,
                           x_name = "x",
                           y_name = "y") {
  compare_same_structure(x, y,
                         x_name = x_name, y_name = y_name,
                         check_rowcount = FALSE)
  if (missing(compare_cols)) {
    compare_cols = colnames(x)
  }
  delta_rows_in_x <- anti_join(x = x,
                               y = y,
                               by = compare_cols)
  if (nrow(delta_rows_in_x) > 0) {
    delta_rows_in_x$source <- x_name
    delta_rows_in_x$id <- 1:nrow(delta_rows_in_x)
  }
  delta_rows_in_y <- anti_join(x = y,
                               y = x,
                               by = compare_cols)
  if (nrow(delta_rows_in_y) > 0) {
    delta_rows_in_y$source <- y_name
    delta_rows_in_y$id <- 1:nrow(delta_rows_in_y)
  }
  if (nrow(delta_rows_in_x) > 0 ||
      nrow(delta_rows_in_y) > 0) {
    delta_df <- rbind(delta_rows_in_x,
                    delta_rows_in_y)
    delta_df <- delta_df[order(delta_df$id, delta_df$source), ]
    if (!missing(sample_size)) {
      delta_df <- delta_df[1:(sample_size * 2), ]
    }
    # Putting source and id first
    delta_df <- delta_df[c(ncol(delta_df),
                           ncol(delta_df) - 1,
                           1:(ncol(delta_df) - 2))]
  } else {
    return(data_frame())
  }
}
