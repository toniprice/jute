# ---------------------------------------------------------------------------- #
# pl
# ---------------------------------------------------------------------------- #
#' Pluralise word
#'
#' Converts a word into a plural (English).
#'
#' @param singular character: singular form of the word.
#' @param num integer: number of items. \code{num = 1} will give the singular
#'   form; \code{num = 0} or \code{num > 1} will give the plural form.
#' @param irregular character: plural form of the word if it is irregular and
#'   needs to be explicitly supplied.
#'
#' @details
#' This function would usually be used in a context where the number of items
#'   \code{num} is a variable and not known at run-time.
#'
#' @return character: singular or plural form of the word, depending on the
#'   value of \code{num}.
#'
#' @examples
#' pl("shoe", 2)
#' pl("shoe", 1)
#'
#' n <- 0
#' pl("thing", n)
#'
#' pl("sheep", 5)           # => incorrect (gives "sheeps")
#' pl("sheep", 5, "sheep")  # => correct   (gives "sheep")
#'
#' @export
pl <- function(singular, num, irregular = NULL) {
  if (num == 1) return(singular)
  irregular %||% sprintf("%ss", singular)
}

# ---------------------------------------------------------------------------- #
