# Removing leading characters from the beginning of characters in a vector.
#
# The {\code{chars_to_strip}} characters are removed from the beginning of every {\code{text}} in
# a vector received. If no such characters are found then the text is left
# intact.
#
# @param text characters
# @return characters without the {\code{chars_to_strip}} if found.
# @examples
# strip_leading("ABC", "ABCDEF")
#
# @export
strip_leading <- function(chars_to_strip, text) {
  num_chars <- nchar(chars_to_strip)
  first_num_chars <- substr(x = text,
                            start = 1,
                            stop = num_chars)
  if (first_num_chars == chars_to_strip) {
    result <- substr(x = text,
                     start = num_chars + 1,
                     stop = nchar(text))
  } else {
    result <- text
  }
}

# Removing trailing characters from the end of characters in a vector.
#
# The {\code{chars_to_strip}} characters are removed from the end of every {\code{text}} in
# a vector received. If no such characters are found then the text is left
# intact.
#
# @param text characters
# @return characters without the {\code{chars_to_strip}} if found.
# @examples
# strip_trailing("DEF", "ABCDEF")
#
# @export
strip_trailing <- function(chars_to_strip, text) {
  num_chars <- nchar(chars_to_strip)
  last_num_chars <- substr(x = text,
                          start = nchar(text) - (num_chars - 1),
                          stop = nchar(text))
  if (last_num_chars == chars_to_strip) {
    result <- substr(x = text,
                     start = 1,
                     stop = nchar(text) - num_chars)
  } else {
    result <- text
  }
}

# Removing "X.BIC." characters from the beginning of characters in a vector.
#
# The "X.BIC." characters are removed from the beginning of every item in
# a vector received. If no such characters are found then the item is left
# intact.
#
# @param name Columnames of an imported dataframe.
# @return Columnames without "X.BIC." if found.
# @examples
# strip_x_bic("X.BIC.MATERIAL")
#
# @export
strip_x_bic <- function(text) {
  strip_leading(text = text,
                chars_to_strip = "X.BIC.")
}

# Removing "X0WM_" characters from the beginning of characters in a vector.
#
# The "X0WM_" characters are removed from the beginning of every item in
# a vector received. If no such characters are found then the item is left
# intact.
#
# @param name Columnames of an imported dataframe.
# @return Columnames without "X0WM_" if found.
# @examples
# strip_x0wm_("X0WM_MATERIAL")
#
# @export
strip_x0wm_ <- function(text) {
  strip_leading(text = text,
                chars_to_strip = "X0WM_")
}

# Removing "X0" characters from the beginning of characters in a vector.
#
# The "X0_" characters are removed from the beginning of every item in
# a vector received. If no such characters are found then the item is left
# intact.
#
# @param name Columnames of an imported dataframe.
# @return Columnames without "X0" if found.
# @examples
# strip_x0wm_("X0ABC")
#
# @export
strip_x0 <- function(text) {
  strip_leading(text = text,
                chars_to_strip = "X0")
}

# Removing trailing ".x" and ".y" that are added after a dplyr join.
#
# Only either ".x" or ".y" are removed not both at the same time, but
# such characters are not expected at the same columnname after dplyr joins.
#
# @param text characters
# @return characters without the ".x" or ".y" if found.
# @examples
# strip_ending_markers_after_join("element.x")
#
# @export
strip_ending_markers_after_join <- function(text) {
  text <- strip_trailing(text = text,
                        chars_to_strip = ".x")
  text <- strip_trailing(text = text,
                        chars_to_strip = ".y")
}

# Removing conversion exit zeros from characters
#
# SAP uses alpha exit conversion to switch between
# user and database friendly representation of data. This function
# removes the zeros that are at the beginning of data.
#
# @param number as characters
# @return number as characters without leading zeros
# @examples
# conversion_exit_alpha_remove0("000123")
#
# @export
conversion_exit_alpha_remove0 <- function(number) {
  return(sub("^(0*)", "", number))
}
