# ============================================================================ #
# stat_utils.R
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# get_stat_mode
# ---------------------------------------------------------------------------- #
#' Compute statistical mode of sample data
#'
#' Computes the statistical mode of the supplied vector under some simplifying
#' assumptions (see 'Details' for more information).
#'
#' @param x vector: integer, numeric, logical, character or factor vector for
#'   which the mode should be computed.
#'
#' @details
#' If x is numeric and assumes integer values then the mode is computed by
#' tabulating its frequencies: the maximum value (or, if there are ties, maximum
#' values) would then be the mode (or modes). If all values are unique then
#' there is no mode and \code{get_stat_mode} returns \code{NA}.\cr
#'
#' If x is numeric and assumes real values, it is assumed to be a sample from a
#' continuous random variable. In this case the mode is estimated by computing
#' the kernel density function of x and returning the (single) value which
#' corresponds to the maximum of the estimated kernel density. Note that in this
#' case only one modal value - i.e. the first - will be returned.
#'
#' @return numeric: the statistical mode computed from the supplied vector.
#'
#' @examples
#' get_stat_mode(c(1, 1, 3, 5, 1, 3, 1, 2))              # => 1
#' get_stat_mode(c(1, 3, 5, 2))                          # => NA
#' get_stat_mode(c(63, 62, 66, 67, 63, 70, 67, 68, 61))  # => 63, 67
#'
#' set.seed(10)
#' get_stat_mode(rnorm(100))  # => -0.1795327
#'
#' set.seed(100)
#' mean(vapply(1:5, function(x) get_stat_mode(rnorm(100)),
#'             FUN.VALUE = numeric(1)))                    # => -0.0069
#'
#' @export
get_stat_mode <- function(x) {

  # nolint start
  # see \url{https://stats.stackexchange.com/questions/176112/how-to-find-the-mode-of-a-probability-density-function}
  # [retrieved 09 Jan 2020]
  # nolint end

  valid_types <- c("factor", "character", "integer", "logical", "numeric")

  if (mode(x) %notin% valid_types) {
    types <- stringify_vec(valid_types, item_sep = " | ")
    stop(sprintf("Supplied values need to be atomic of type: %s", types))
  }

  is_all <- function(x, type) {
    type <- match.arg(type, valid_types)
    switch(type,
           factor    = !isTRUE(any(!is.factor(x))),
           character = !isTRUE(any(!is.character(x))),
           integer   = !isTRUE(any(!is_int_val(x)))
    )
  }

  if (is_all(x, "factor") || is_all(x, "character") || is_all(x, "integer")) {
    uniq_x <- unique(x)
    if (length(uniq_x) == length(x)) {
      return(NA)
    }
    freq <- tabulate(match(x, uniq_x))
    mode_val <- uniq_x[which(freq == max(freq))]
    if (is.factor(mode_val)) {
      levels(x)[mode_val]
    } else {
      mode_val
    }

  } else {
    kern_den <- stats::density(x = x, bw = stats::bw.SJ(x))
    kern_den$x[which(kern_den$y == max(kern_den$y)[1])]
  }
}

# ---------------------------------------------------------------------------- #
