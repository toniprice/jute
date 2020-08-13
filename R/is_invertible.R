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
