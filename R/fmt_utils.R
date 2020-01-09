# ============================================================================ #
# fmt_utils.R
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# pad_c
# ---------------------------------------------------------------------------- #
#' Pad and join multiple strings within character vectors
#'
#' Joins one or more character vectors as in \code{\link[stringr]{str_c}} but
#' with the character strings in each vector padded to make them equal length.
#' The last vector is by default left unpadded but can optionally be padded too.
#'
#' @param ... one or more character vectors as for \code{\link[stringr]{str_c}}.
#' @param pad_last logical: pad the last vector in addition to the others or
#'   leave it without padding?
#' @param sep character: separator between vector elements.
#' @param collapse character: optional string used to combine input vectors into
#'   a single string.
#'
#' @return character: a single string constructed in a similar way as with
#'   \code{\link[stringr]{str_c}} but where vectors are padded to a uniform
#'   width. If collapse is NULL (the default) the returned value is a character
#'   vector with length equal to the length of the first input vector. If
#'   collapse is non-NULL, the returned value is a character vector of length 1.
#'
#' @seealso \code{\link[stringr]{str_c}}
#' @examples
#' vec1 <- c("Avocado:", "Blueberry:", "Coconut:", "Dill:")
#' vec2 <- c(rep("fruit", 3), "herb")
#' vec3 <- c("green", "purple", "white", "green")
#'
#' pad_c(vec1, pad_last = TRUE)
#'
#' cat(pad_c(vec1, vec2, collapse = "\n"), "\n")
#'
#' cat(pad_c(vec1, vec2, vec3, collapse = "\n"), "\n")
#' @export
pad_c <- function(..., pad_last = FALSE, sep = " ", collapse = NULL) {
  vecs <- list(...)
  n_vecs <- length(vecs)

  max_n <- ifelse(pad_last, n_vecs, n_vecs - 1)

  padded <- lapply(seq(1, max_n, length.out = max_n), function(i) {
    vec <- vecs[[i]]
    stringr::str_pad(vec, max(nchar(vec)), side = "right", pad = " ")
  })
  if (max_n < n_vecs) padded <- c(padded, vecs[n_vecs])

  cat_pad <- function(...) stringr::str_c(..., sep = sep, collapse = collapse)
  do.call(cat_pad, args = padded)
}

# ---------------------------------------------------------------------------- #
# fmt_int
# ---------------------------------------------------------------------------- #
#' Convert integers into strings
#'
#' Converts an integer value or vector of integer values into string(s) without
#' any decimal point, padding them with leading zeroes to ensure they are all
#' the same length.
#'
#' @param num integer vector of values to be converted (these do not need to be
#'   of class \code{integer} but they do need to \emph{be} integer, i.e. without
#'   any digits after the decimal points).
#'
#' @return character: vector of formatted integer values, padded to the length
#'   of the largest value with leading spaces.
#'
#' @examples
#' fmt_int(1.0)
#' fmt_int(2)
#' fmt_int(c(2, 25, 467))
#'
#' @export
fmt_int <- function(num) {
  lapply(num, function(n_val) {
    if (!is_int_val(n_val)) {
      stop(sprintf("Expected integer value but received '%s'", n_val))
    }
  })
  format(num, drop0trailing = TRUE)
}

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
# titleize_first
# ---------------------------------------------------------------------------- #
#' Make first word title case
#'
#' Formats the first word in the given text to have title case.
#'
#' @param txt character: text to be formatted in title case.
#'
#' @return character: the supplied text with its first word in title case.
#'
#' @examples
#' titleize_first("hello")
#'
#' titleize_first("hello world")
#'
#' @export
titleize_first <- function(txt) {
  splits <- c(stringr::str_sub(txt, end = 1L),
              stringr::str_sub(txt, start = 2L))
  sprintf("%s%s", stringr::str_to_upper(splits[1]), splits[2])
}

# ---------------------------------------------------------------------------- #
# stringify_vec
# ---------------------------------------------------------------------------- #
#' Stringify vector
#'
#' Converts a vector (named or unnamed) into a single string representation.
#'
#' @param vec vector: the vector to be converted into a character string.
#' @param item_sep character: separator to use between vector elements when
#'   converting the vector into a string.
#' @param name_sep character: separator to use between names and values when
#'   converting the vector into a string (if names exist).
#' @param fmt character: format specification for the vector values, or NULL to
#'   use defaults (which would be as returned by \code{\link{format}}).
#'
#' @return character: a single character string representation of the vector.
#'
#' @examples
#' stringify_vec(c(1, 2, 3))
#'
#' stringify_vec(c(one = 1, two = 2, three = 3))
#'
#' @export
stringify_vec <- function(vec, item_sep = ", ", name_sep = " = ", fmt = NULL) {
  elements <- sprintf(fmt %||% "%s", vec)
  if (!is.null(names(vec))) {
    elements <- sprintf("%s%s%s", names(vec), name_sep, elements)
  }
  paste(elements, collapse = item_sep)
}

# ---------------------------------------------------------------------------- #
# pl
# ---------------------------------------------------------------------------- #
#' Pluralize word
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
# lgl_str
# ---------------------------------------------------------------------------- #
#' Convert logical variable to string
#'
#' Converts a logical variable into a string representation.
#'
#' @param lgl logical: the value to be converted to a string.
#' @param case character: case of the output ("title", "lower" or "upper").
#'
#' @return character: the logical value as a string, i.e. "yes" or "no",
#'   formatted according to the specified \code{case} option.
#'
#' @examples
#' lgl_str(FALSE)                 # => "No"
#' lgl_str(TRUE, case = "upper")  # => "YES"
#' lgl_str(FALSE, case = "lower") # => "no"
#'
#' @export
lgl_str <- function(lgl, case = NULL) {
  case <- match.arg(case, c("title", "lower", "upper"))
  val <- ifelse(lgl, "yes", "no")
  switch(
    case,
    title = stringr::str_to_title(val),
    lower = val,
    upper = stringr::str_to_upper(val)
  )
}

# ---------------------------------------------------------------------------- #
