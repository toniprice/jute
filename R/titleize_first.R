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
