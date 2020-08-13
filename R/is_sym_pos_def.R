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
