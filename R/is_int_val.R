# ---------------------------------------------------------------------------- #
# is_int_val
# ---------------------------------------------------------------------------- #
#' Test if a number is integer value
#'
#' Tests if the supplied number is an integer value (or could be coerced to
#' one).
#'
#' @param num numeric: the number to be tested for whether it is an integer
#'   value.
#' @param tol numeric: a small value to be used as the mathematical tolerance
#'   when assessing whether the number is an integer.\cr
#'   The default of \code{.Machine$double.eps ^ 0.5} is approximately equal to
#'   1.5e-08 on a 64-bit machine.
#'
#' @details
#' \code{is_int_val} differs from \code{\link{is.integer}} in that
#' \code{is.integer} tests for objects of type \code{"integer"}, not for whether
#' the numbers themselves are integers.\cr\cr
#'
#' For example, the following two calls return FALSE: \code{is.integer(5.0)} or
#' \code{is.integer(5)}, though this call returns TRUE:
#' \code{is_int_val(5.0)}.\cr\cr
#'
#' Note that both \code{is.integer(5L)} and \code{is_int_val(5L)} return TRUE.
#'
#' @return logical: TRUE if the supplied number is an integer value, FALSE
#'   otherwise.
#'
#' @examples
#' is_int_val(100)     # => TRUE
#' is_int_val(100.0)   # => TRUE
#' is_int_val(100L)    # => TRUE
#'
#' is_int_val(100.001) # => FALSE
#'
#' is_int_val(TRUE)    # => TRUE
#'
#' @seealso \code{\link{is.integer}}
#'
#' @export
is_int_val <- function(num, tol = .Machine$double.eps ^ 0.5) {
  if (is.logical(num)) return(TRUE)
  abs(num - round(num)) < tol
}

# ---------------------------------------------------------------------------- #
