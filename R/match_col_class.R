# ---------------------------------------------------------------------------- #
# match_col_class
# ---------------------------------------------------------------------------- #
#' Get column indexes of a given class in a data frame
#'
#' For a specified class such as "numeric" or "character", gets the column
#' indexes in a data frame for columns matching the class.
#'
#' @param df data.frame: data frame for which columns should be matched.
#' @param class_type character: type of class to match (e.g. "numeric",
#'   "integer", "logical", "character", "factor").
#'
#' @return integer: the vector of column indexes whose classes match the
#'   specified class.
#'
#' @examples
#' id    <- letters[1:3]
#' size  <- c(1.2, 3.5, 4.1)
#' num   <- c(1, 2, 3)
#' intgr <- c(1L, 2L, 3L)
#'
#' df_no_fac <- data.frame(id, size, num, intgr, stringsAsFactors = FALSE)
#' match_col_class(df_no_fac, "character")
#'
#' df_with_fac <- data.frame(id, size, num, intgr)
#' match_col_class(df_with_fac, "character")
#' match_col_class(df_with_fac, "factor")
#' match_col_class(df_with_fac, "numeric")
#' match_col_class(df_with_fac, "integer")
#'
#' @export
match_col_class <- function(df, class_type) {
  which(vapply(df, class, FUN.VALUE = character(1)) == class_type)
}

# ---------------------------------------------------------------------------- #
