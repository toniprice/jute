# ---------------------------------------------------------------------------- #
# stringify_vec
# ---------------------------------------------------------------------------- #
#' Stringify vector
#'
#' Converts a vector (named or unnamed) into a single string representation.
#'
#' @param vec vector: the vector to be converted into a character string.
#' @param item_sep character: separator to use between vector elements when
#'   converting the vector into a string.
#' @param name_sep character: separator to use between names and values when
#'   converting the vector into a string (if names exist).
#' @param fmt character: format specification for the vector values, or NULL to
#'   use defaults (which would be as returned by \code{\link{format}}).
#'
#' @return character: a single character string representation of the vector.
#'
#' @examples
#' stringify_vec(c(1, 2, 3))
#'
#' stringify_vec(c(one = 1, two = 2, three = 3))
#'
#' @export
stringify_vec <- function(vec, item_sep = ", ", name_sep = " = ", fmt = NULL) {
  elements <- sprintf(fmt %||% "%s", vec)
  if (!is.null(names(vec))) {
    elements <- sprintf("%s%s%s", names(vec), name_sep, elements)
  }
  paste(elements, collapse = item_sep)
}

# ---------------------------------------------------------------------------- #
