# Removing objects from the environment and also calling gc
#
# This leads to not only the removal of objects but also
# frees up memory.
# The execution is conditional and depends on the variable
# RBW_CLEANUP being set.
#
# @param objects to be removed
# @examples
# rmgc("a_data_frame")
#
# @export
rmgc <- function(...) {
  if (exists("RBW_CLEANUP") && RBW_CLEANUP) {
    names_ <- sapply(match.call(expand.dots = FALSE)$...,
                    as.character)
    names_ <- names_[names_ %in% ls(pos = ".GlobalEnv")]
    if (length(names_) > 0) {
      rm(list = as.vector(names_),
         pos = ".GlobalEnv")
    }
    output <- gc()
  }
}
