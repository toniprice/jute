# ---------------------------------------------------------------------------- #
# mk_lookup_utils
# ---------------------------------------------------------------------------- #
#' Create lookup utility functions
#'
#' \code{mk_lookup_utils} is a closure which returns a list of lookup utility
#' functions.
#'
#' @param lookups a named list of named character vectors. The names are
#'   available types and the elements (each a named character vector) are
#'   key/description pairs for the corresponding lookup type.
#'
#' @return a list of lookup utility functions with the following elements
#'   \strong{(see 'Returned Functions' for details)}:
#'   \describe{
#'     \item{\code{get_all}}{function to retrieve all available lookups.}
#'     \item{\code{get}}{function to retrieve the lookup options for the given
#'          key.}
#'     \item{\code{descrip}}{function to return the description for a key from
#'          the specified lookup.}
#'     \item{\code{get_types}}{function to retrieve the names of all available
#'          lookup types.}
#'     \item{\code{get_choices}}{function to retrieve the names of all available
#'          choices for the given type.}
#'     \item{\code{make}}{function to create a lookup function for the given
#'          type.}
#'     \item{\code{make_all}}{function to create lookup functions for all
#'          available types.}
#'     \item{\code{add_to_env}}{function to add the given lookup functions to
#'          the specified environment.}
#'   }
#'
#' @section Returned Functions:
#'   The returned functions are utilities for managing a lookup list. They are:
#'
#' \preformatted{
#' get_all()
#' }
#' function to retrieve all available lookups\cr
#' \emph{Value}\cr
#' a list of all available lookups.
#'
#' \preformatted{
#' get(type)
#' }
#' function to retrieve the lookup options for the given key\cr
#' \emph{Arguments}\cr
#' \describe{
#'   \item{\code{type}}{character: name of the type for which to retrieve lookup
#'     options.}
#' }
#' \emph{Value}\cr
#' a named character vector of lookup options for the given type. The character
#'   vector consists of key/value pairs where names are keys and values are
#'   descriptions for the keys.
#'
#' \preformatted{
#' descrip(key, type = NULL, lookup = NULL, tf = FALSE, strip_name = TRUE)
#' }
#' function to return the description for a key from the specified lookup\cr
#' \emph{Arguments}\cr
#' \describe{
#'    \item{\code{key}}{character: key for which to retrieve a description from
#'      the specified lookup.}
#'    \item{\code{type}}{character: type for which to retrieve the lookup. If
#'      NULL, \code{lookup} must be supplied instead.}
#'    \item{\code{lookup}}{list: the lookup (a named list of named character
#'      vectors). If NULL, \code{type} must be supplied instead.}
#'    \item{\code{tf}}{logical (stands for "titleize first"): should the first
#'      letter of the description be in title case or lower case?}
#'    \item{\code{strip_name}}{logical: should the name (i.e. the key) be
#'      removed from the description before it is returned?}
#' }
#' \emph{Value}\cr
#' description of the specified key within the specified lookup.
#'
#' \preformatted{
#' get_types()
#' }
#' function to retrieve the names of all available lookup types\cr
#' \emph{Value}\cr
#' a character vector comprising names of all available lookup types.
#'
#' \preformatted{
#' get_choices(type)
#' }
#' function to retrieve the names of all available choices for the given type.
#' \cr
#' \emph{Arguments}\cr
#' \describe{
#'    \item{\code{type}}{character: type for which to retrieve all available
#'      choices.}
#' }
#' \emph{Value}\cr
#' a character vector comprising names of all available choices for the given
#'   type.
#'
#' \preformatted{
#' make(type)
#' }
#' function to create a lookup function for the given type\cr
#' \emph{Arguments}\cr
#' \describe{
#'    \item{\code{type}}{character: type for which to create a lookup function.}
#' }
#' \emph{Value}\cr
#' a lookup function for the given type.
#'
#' \preformatted{
#' make_all()
#' }
#' function to create lookup functions for all available types\cr
#' \emph{Value}\cr
#' a list of all available lookup functions.
#'
#' \preformatted{
#' add_to_env(envir = NULL, lst = NULL)
#' }
#' function to add the given lookup functions to the specified environment\cr
#' \emph{Arguments}\cr
#' \describe{
#'    \item{\code{envir}}{environment to which the lookup functions should be
#'      added. Note that the default is to create a new environment from within
#'      this function, which would have no effect on its return; this is by
#'      design to avoid any unintended additions to an environment.}
#'    \item{\code{lst}}{a list of functions to add to the specified environment
#'      (or NULL to add all available lookup functions).}
#' }
#' \emph{Value}\cr
#' NULL
#'
#' @examples
#' stuff <- list(a = c(upper = "red", middle = "yellow", lower = "green"),
#'               b = c(first = "platinum level", second = "silver level"))
#' lookup <- mk_lookup_utils(stuff)
#'
#' lookup$get_all()
#'
#' lookup$get("b")
#'
#' lookup$descrip("lower", lookup = lookup$get("a"))
#' lookup$descrip("lower", "a")
#'
#' lookup$descrip("lower", "a", tf = TRUE)
#'
#' lookup$descrip("lower", "a", strip_name = FALSE)
#'
#' lookup$get_types()
#'
#' lookup$get_choices("b")
#'
#' lookup_a <- lookup$make("a")
#' lookup_a("upper")
#'
#' lookup_lst <- lookup$make_all()
#' lookup_lst$lookup_b("second")
#'
#' an_env <- new.env(parent = emptyenv())
#' lookup$add_to_env(an_env)
#'
#' @export
mk_lookup_utils <- function(lookups) {

  # ---------------------------------- #
  # get
  # ---------------------------------- #
  get <- function(type) {

    if (missing(type)) stop("Argument 'type' cannot be missing")

    if (is.null(lookups[[type]])) {
      stop(sprintf("Unknown lookup type '%s'", type))
    }

    lookups[[type]]
  }

  # ---------------------------------- #
  # descrip
  # ---------------------------------- #
  descrip <- function(key, type = NULL, lookup = NULL, tf = FALSE,
                      strip_name = TRUE) {

    if (missing(key)) stop("Argument 'key' cannot be missing")

    if (is.null(type) && is.null(lookup)) {
      msg <- "Arguments 'type' and 'lookup' cannot both be NULL"
      stop(msg)
    }

    lookup <- lookup %||% get(type)

    descrip <- lookup[key]
    if (is.na(unname(descrip))) {
      stop(sprintf("Key '%s' not found in lookup", key))
    }
    if (strip_name) descrip <- unname(descrip)
    if (tf) descrip <- titleize_first(descrip)
    descrip
  }

  # ---------------------------------- #
  # get_types
  # ---------------------------------- #
  get_types <- function() {
    names(lookups)
  }

  # ---------------------------------- #
  # get_choices
  # ---------------------------------- #
  get_choices <- function(type) {
    names(get(type))
  }

  # ---------------------------------- #
  # make
  # ---------------------------------- #
  make <- function(type) {
    lookup <- get(type)
    function(key, tf = FALSE, strip_name = TRUE) {
      descrip(key, lookup = lookup, tf = tf, strip_name = strip_name)
    }
  }

  # ---------------------------------- #
  # make_all
  # ---------------------------------- #
  make_all <- function() {

    nms <- get_types()

    funcs <- lapply(nms, function(nm) {
      make(nm)
    })
    names(funcs) <- sprintf("lookup_%s", nms)

    funcs
  }

  # ---------------------------------- #
  # add_to_env
  # ---------------------------------- #
  add_to_env <- function(envir = NULL, lst = NULL) {
    envir <- envir %||% new.env(parent = emptyenv())
    lst   <- lst %||% make_all()
    # define funcs in the specified environment
    list2env(lst, envir = envir)
    NULL
  }

  # ---------------------------------- #
  list(
    get_all     = function() lookups,
    get         = get,
    descrip     = descrip,
    get_types   = get_types,
    get_choices = get_choices,
    make        = make,
    make_all    = make_all,
    add_to_env  = add_to_env
  )
}

# ---------------------------------------------------------------------------- #
