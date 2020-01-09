# ============================================================================ #
# misc_utils.R
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# place_in_env
# ---------------------------------------------------------------------------- #
#' Place variables into an environment
#'
#' Place the specified variables into the environment supplied.
#'
#' @param ... the variables to place into the environment (can be named or
#'   unnamed).
#' @param envir the environment object into which to place the variables. The
#'   default is the environment of the function itself (which would not be very
#'   useful in practice but this is by design to avoid any unintended additions
#'   to an environment). So \code{envir} should usually be explicitly specified.
#' @param var_names an optional character vector of names for the variables
#'   within the environment. See 'Details' for information about default naming.
#'
#' @return the environment supplied, updated to include variables specified in
#'   the ellipsis argument (invisibly)
#'
#' @details
#' If \code{envir} is NULL, a new environment will be created with
#' \code{\link{new.env}} and an empty parent, i.e.,
#' \code{new.env(parent = emptyenv())}.\cr
#'
#' Names for the environment variables are assigned as follows:\cr
#' \itemize{
#'   \item{If names are specified in \code{var_names}, these are used;}
#'   \item{if \code{var_names} is NULL and the ellipsis arguments are named,
#'         these names are used;}
#'   \item{if \code{var_names} is NULL and the ellipsis arguments are unnamed,
#'         names are set to sequential letters of the alphabet (i.e. the first
#'         list item would be named "a", the second "b" and so forth).}
#' }
#'
#' @seealso \code{\link{new.env}}, \code{\link{emptyenv}}
#' @examples
#' an_env <- new.env(parent = emptyenv())
#' place_in_env(x = 1, y = 2, envir = an_env)
#' # now it is possible to access these by, e.g. an_env$y etc.
#'
#' # the following would all be equivalent in terms of naming, i.e. they would
#' # all result in the names "a" and "b" being assigned:
#' place_in_env("one", "two")
#' place_in_env(a = "one", b = "two")
#' place_in_env("one", "two", var_names = c("a", "b"))
#'
#' # run from within a function, the following would have the effect of placing
#' # the variables v and w into the function's local environment:
#' place_in_env(v = "one", w = "two", envir = environment())
#'
#' # run from within the global environment, the following would have the effect
#' # of placing the variables v and w into the global environment:
#' \dontrun{place_in_env(v = "one", w = "two", envir = environment())}
#'
#' @export
place_in_env <- function(..., envir = NULL, var_names = NULL) {
  args <- list(...)
  names(args) <- var_names %||% names(args) %||% letters[seq_along(args)]
  envir <- envir %||% new.env(parent = emptyenv())
  # convert the elements of args into variables in the specified environment
  invisible(list2env(args, envir = envir))
}

# ---------------------------------------------------------------------------- #
