# ============================================================================ #
# misc_utils.R
# ============================================================================ #

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
# mk_mtx
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
#' mk_mtx(col_1 = c("a", "b"), col_2 = c("alpha", "bravo"))
#'
#' mk_mtx(A = c(1, 10, 100))
#'
#' mk_mtx(seq(10, 30, 10), seq(70, 90, 10))
#'
#' @export
mk_mtx <- function(...) {
  do.call(cbind, list(...))
}

# ---------------------------------------------------------------------------- #
# simplify_lst_lst
# ---------------------------------------------------------------------------- #
#' Simplify list of lists
#'
#' Simplify a list of lists by eliminating the top-level list.
#'
#' @param lst a list of lists, each of which should be moved up one level in the
#'   list hierarchy.

#' @param unnamed_prefix character: string to use as a prefix for naming unnamed
#'   list items in the event that any list items are unnamed.

#'
#' @return list: the items in \code{lst} but each moved one level up in the list
#'   hierarchy, resulting in a list of single elements.
#'
#' @details
#' List names are handled as follows: if any list items are named, these names
#' will be set in the returned list. If any list items are unnamed, their names
#' will be set to the default value prefixed by \code{unnamed_prefix} with an
#' integer suffix starting at 1 and incremented for each unnamed item.
#'
#' @examples
#' a_1 <- "pineapple"
#' a_2 <- list(uno = 1, due = 2, tre = 3)
#' a_3 <- c(1, 4, 9, 16, 25)
#' lst <- list(list(z_1 = a_1), list(z_2 = a_2), list(z_3 = a_3))
#' simplify_lst_lst(lst)
#'
#' a_1 <- "pineapple"
#' a_2 <- list(uno = 1, due = 2, tre = 3)
#' a_3 <- c(1, 4, 9, 16, 25)
#' lst <- list(list(a_1), list(z = a_2), list(a_3))
#' simplify_lst_lst(lst)
#'
#' @export
simplify_lst_lst <- function(lst, unnamed_prefix = "name") {

  cls_chk <- unlist(lapply(lst, class))
  if (length(unique(cls_chk)) != 1 || unique(cls_chk) != "list") {
    fmt <- "Argument 'lst' should be a list of lists but found classes: %s"
    stop(sprintf(fmt, paste(cls_chk, collapse = ", ")))
  }

  lst_items <- lapply(lst, "[[", 1)

  lst_names <- lapply(lst, names)

  # set default names for any unnamed items
  nulls <- which(vapply(lst_names, is.null, FUN.VALUE = logical(1)))
  lst_names[nulls] <- get_incremental_names(length(nulls), unnamed_prefix)

  setNames(lst_items, lst_names)
}

# ---------------------------------------------------------------------------- #
