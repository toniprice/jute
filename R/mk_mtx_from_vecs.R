# ---------------------------------------------------------------------------- #
# mk_mtx_from_vecs
# ---------------------------------------------------------------------------- #
#' Create matrix from vectors
#'
#' Binds the supplied vectors into columns of a matrix consisting with vector
#' names as column names (if the vectors are named).
#'
#' @param ... one or more named vectors which should be combined into the
#'   columns of a matrix. If the vectors are named, these names will become the
#'   column names. Note that the vectors should all be of the same length and
#'   type.
#'
#' @return a matrix constructed from the named vectors.
#'
#' @examples
#' mk_mtx_from_vecs(col_1 = c("a", "b"), col_2 = c("alpha", "bravo"))
#'
#' mk_mtx_from_vecs(A = c(1, 10, 100))
#'
#' mk_mtx_from_vecs(seq(10, 30, 10), seq(70, 90, 10))
#'
#' @export
mk_mtx_from_vecs <- function(...) {
  do.call(cbind, list(...))
}

# ---------------------------------------------------------------------------- #
