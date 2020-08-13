# ---------------------------------------------------------------------------- #
# get_max_int_len
# ---------------------------------------------------------------------------- #
#' Compute maximum length of formatted integers
#'
#' Computes the maximum character length of a vector of integers when they are
#' converted to strings.
#'
#' @param num integer: vector of the integer values.
#'
#' @return integer: maximum character length of the integers provided, if they
#'   were formatted as strings.
#'
#' @examples
#' get_max_int_len(1.0)
#' get_max_int_len(2)
#' get_max_int_len(c(2, 25, 467))
#'
#' @export
get_max_int_len <- function(num) {
  lens <- vapply(fmt_int(num), stringr::str_length, integer(1),
                 USE.NAMES = FALSE)

  # the expected behaviour is that all the strings are formatted to be of equal
  # length so this should be equivalent to lens[1]; however, doing it this way
  # is insurance against a future change
  unique(max(lens))
}

# ---------------------------------------------------------------------------- #
