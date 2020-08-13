# ---------------------------------------------------------------------------- #
# get_random_seed_val
# ---------------------------------------------------------------------------- #
#' Generate random seed value
#'
#' Generates a random seed value for setting the RNG seed by sampling a random
#' number (between 1 and .Machine$integer.max).
#'
#' @return integer: a random value (sampled from the numbers between 1 and
#' .Machine$integer.max).
#'
#' @seealso \code{\link{set_init_seed}}
#'
#' @examples
#' get_random_seed_val()
#'
#' @export
get_random_seed_val <- function() {
  sample(.Machine$integer.max, size = 1)
}

# ---------------------------------------------------------------------------- #
