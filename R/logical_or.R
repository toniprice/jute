# ---------------------------------------------------------------------------- #
# function `%||%`
# ---------------------------------------------------------------------------- #
#' Logical \code{OR} operator
#'
#' Sets default values using a logical \code{OR} operator.
#'
#' @name logical_OR
#'
#' @param a NULL, or a particular value.
#' @param b the default value to return if the first operand is NULL.
#'
#' @return a non-NULL value. This is is the value of the first operand if it is
#'   non-NULL, or the value of the second operand if the first operand is NULL.
#'
#' @details
#' This infix function is inspired by Ruby's "logical OR" operator (\code{||})
#' for conveniently setting default values.\cr
#'
#' usage:\cr
#' \code{function_that_might_return_null() \%||\% <default value>}
#'
#' See Advanced R (Hadley Wickham) Section 6.5.1 p. 91.
#'
#' @references
#'   Wickham, Hadley (2015). Advanced R, CRC Press, Boca Raton FL.
#' @examples
#' x <- NULL
#' x %||% 10              # => 10
#'
#' x <- "Madiba"
#' y <- "Nelson Mandela"
#' x %||% y               # => 'Madiba'
#'
#' @export

# nolint start
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}
# nolint end

# ---------------------------------------------------------------------------- #
