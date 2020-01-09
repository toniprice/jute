# ============================================================================ #
# rng_utils.R
# ============================================================================ #

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
# set_init_seed
# ---------------------------------------------------------------------------- #
#' Set RNG seed
#'
#' Sets the RNG (random number generator) seed.
#'
#' @param seed_val one of the following:\itemize{
#'   \item{a single character value (which can be interpreted as an integer) to
#'   set the RNG;}
#'   \item{the character literal "random" (to set a random seed);}
#'   \item{\code{NULL}, to re-initialize the RNG as if no seed had yet been set
#'   (see 'Details' in \code{\link{set.seed}}); or}
#'   \item{\code{NA}, in which case the function will return without any change
#'   to the seed.}
#'   }
#'
#' @return NULL, invisibly
#'
#' @keywords random seed, RNG
#' @seealso \code{\link{set.seed}}, \code{\link{get_random_seed_val}}
#'
#' @examples
#' \dontrun{
#' set_init_seed(1)        # => set seed to a specific value
#' set_init_seed("random") # => set seed to a randomly-generated value
#' set_init_seed(NULL)     # => re-initialize seed as if none had yet been set
#' set_init_seed(NA)       # => return immediately without changing seed
#' }
#'
#' @export
set_init_seed <- function(seed_val) {

  if (!is.atomic(seed_val) || (length(seed_val) > 1)) {
    msg <- paste("Argument 'seed_val' should be an integer, the keyword",
                 "'random', NULL or NA")
    stop(msg)
  }

  if (!is.null(seed_val) && is.na(seed_val)) {
    # requested seed is NA; return without setting seed
    return(NULL)
  }

  if (!is.null(seed_val) && identical(seed_val, "random")) {
    seed_val <- get_random_seed_val()
  }

  # --- seed_handler --- --- --- --- --- --- --- --- --- --- --- --- --- --- --#
  seed_handler <- function(e_or_w) {
    fmt <- paste("Argument 'seed_val' is '%s' (note that this must be",
                 "interpretable as an integer to set the seed)",
                 "\nWarning or Error whilst setting the RNG seed was:",
                 e_or_w$message)
    stop(sprintf(fmt, seed_val))
  }
  # --- END: seed_handler --- --- --- --- --- --- --- --- --- --- --- --- --- -#

  tryCatch({
    # if seed_val is NULL, RNG seed will be re-initialized (i.e. set to NULL);
    # otherwise it will be set to the value of seed_val
    set.seed(seed_val)
  },
  error = seed_handler, warning = seed_handler)

  invisible(seed_val)
}

# ---------------------------------------------------------------------------- #
