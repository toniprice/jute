# ---------------------------------------------------------------------------- #
# logical_to_str
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
#' logical_to_str(FALSE)                 # => "No"
#' logical_to_str(TRUE, case = "upper")  # => "YES"
#' logical_to_str(FALSE, case = "lower") # => "no"
#'
#' @export
logical_to_str <- function(lgl, case = NULL) {
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
