# ---------------------------------------------------------------------------- #
# get_incremental_names
# ---------------------------------------------------------------------------- #
#' Construct sequence of incremental names
#'
#' Constructs a sequence of names, each of which has an increasing integer
#' appended to it.
#'
#' @param n_items integer: the number of names to generate.
#'
#' @param prefix character: an optional name prefix to use. E.g. if
#'   \code{prefix} is "xyz" and \code{n_items} is 3 this will result in names
#'   \code{xyz_1}, \code{xyz_2} and \code{xyz_3}. If NULL the returned names
#'   will simply be a sequence of integers which are formatted as strings.
#'
#' @param sep character: separator to use between \code{prefix} and the
#'   incremental numbers (default is an underscore). Ignored if \code{prefix =
#'   NULL}.
#'
#' @return a character vector of names with successively incremented integer
#'   suffixes.
#'
#' @examples
#' get_incremental_names(2, "item")
#' get_incremental_names(3, "item", sep = "")
#' get_incremental_names(11, "z")
#' get_incremental_names(12)
#'
#' @export
get_incremental_names <- function(n_items, prefix = NULL, sep = "_") {
  len <- nchar(sprintf("%d", n_items))
  fmt <- sprintf("%%0%dd", len)
  if (!is.null(prefix)) fmt <- sprintf("%s%s%s", prefix, sep, fmt)
  sprintf(fmt, seq_len(n_items))
}

# ---------------------------------------------------------------------------- #
