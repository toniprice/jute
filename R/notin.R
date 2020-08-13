# ---------------------------------------------------------------------------- #
# function `%notin%`
# ---------------------------------------------------------------------------- #
#' Find non-matching values
#'
#' Returns non-matching values for its left operand when compared to the right
#' operand.
#'
#' @name notin
#'
#' @param x vector or NULL: values to be compared.
#' @param y vector or NULL: values for which \emph{non}-matches should be
#'   returned.
#'
#' @return a logical vector indicating if there is \emph{no} match for its left
#'   operand.
#'
#' @examples
#' foo <- letters[1:6]
#' foo[foo %notin% c("a", "c", "e")]
#' # => [1] "b" "d" "f"
#'
#' lst <- list(a = 1, b = 2, c = 3, d = 4)
#' lst[names(lst) %notin% c("a", "c", "d")]
#' # =>
#' # $b
#' # [1] 2
#'
#' NULL %notin% c(1, 2)
#' # => logical(0)
#'
#' c("a", "b", "c") %notin% NULL
#' # => [1] TRUE TRUE TRUE
#'
#' @export
`%notin%` <- function(x, y) {
  # nolint start
  # see \url{http://stackoverflow.com/questions/7494848/standard-way-to-remove-multiple-elements-from-a-dataframe}
  # [retrieved 09 Jan 2020]
  # nolint end
  !(x %in% y)
}

# ---------------------------------------------------------------------------- #
