# ============================================================================ #
# math_utils.R
# ============================================================================ #

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
# is_sym_pos_def
# ---------------------------------------------------------------------------- #
#' Check if a matrix is symmetric positive-definite
#'
#' Checks a matrix to determine if it is symmetric positive-definite.
#'
#' @param x matrix: numeric matrix which should be checked to see if it is
#'   symmetric positive-definite.
#'
#' @return logical: TRUE if the supplied matrix is symmetric positive-definite,
#' FALSE otherwise.
#'
#' @examples
#' is_sym_pos_def(matrix(c(2.51, 2.01, 2.01, 1.74), 2, 2))  # => TRUE
#' is_sym_pos_def(matrix(c(1, 1, 1, 1), 2, 2))              # => FALSE
#'
#' @export
is_sym_pos_def <- function(x) {

  if (!isSymmetric(x)) {
    stop("Matrix is not symmetric")
  }

  # if a matrix is *symmetric* and positive-definite, then all of its
  # eigenvalues are positive
  if (length(which(eigen(x)$values <= 0.0)) > 0) {
    FALSE
  } else {
    TRUE
  }
}

# ---------------------------------------------------------------------------- #
# is_invertible
# ---------------------------------------------------------------------------- #
#' Check if a matrix is invertible
#'
#' Checks a matrix to see if it is invertible.
#'
#' @param x matrix: numeric matrix which should be checked to see if it is
#'   invertible.
#'
#' @return logical: TRUE if the supplied matrix is invertible, FALSE otherwise.
#'
#' @examples
#' is_invertible(matrix(c(1, 2, 2, 1), 2, 2))  # => TRUE
#'
#' is_invertible(matrix(c(1, 1, 1, 1), 2, 2))  # => FALSE
#'
#' @export
is_invertible <- function(x) {

  res <- tryCatch({
    solve(x)
  },
  error = function(e) {
    e
  })

  if (inherits(res, "error")) {
    FALSE
  } else {
    TRUE
  }
}

# ---------------------------------------------------------------------------- #
