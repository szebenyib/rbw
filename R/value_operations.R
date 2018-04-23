# Collects characters from a string and adds them
# to the collection uniquely
#
# This can be used to observe which (special) characters
# hinder further processing.
#
# @param string of characters to collect
# @param collection a vector of collected characters
# @return collection
# @examples
# collection <- NULL
# collect_chars_from_string("abc", collection)
collect_chars_from_string <- function(string, collection) {
  if (string != "") {
    for(i in 1:nchar(string)) {
      if (!substr(string, i, i) %in% collection) {
        collection <- c(collection, substr(string, i, i))
      }
    }
  }
  return(collection)
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
