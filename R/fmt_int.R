# ---------------------------------------------------------------------------- #
# fmt_int
# ---------------------------------------------------------------------------- #
#' Convert integers into strings
#'
#' Converts an integer value or vector of integer values into string(s) without
#' any decimal point, padding them with leading zeroes to ensure they are all
#' the same length.
#'
#' @param num integer vector of values to be converted (these do not need to be
#'   of class \code{integer} but they do need to \emph{be} integer, i.e. without
#'   any digits after the decimal points).
#'
#' @return character: vector of formatted integer values, padded to the length
#'   of the largest value with leading spaces.
#'
#' @examples
#' fmt_int(1.0)
#' fmt_int(2)
#' fmt_int(c(2, 25, 467))
#'
#' @export
fmt_int <- function(num) {
  lapply(num, function(n_val) {
    if (!is_int_val(n_val)) {
      stop(sprintf("Expected integer value but received '%s'", n_val))
    }
  })
  format(num, drop0trailing = TRUE)
}

# ---------------------------------------------------------------------------- #
