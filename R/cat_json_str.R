# ---------------------------------------------------------------------------- #
# cat_json_str
# ---------------------------------------------------------------------------- #
#' Concatenate stringified JSON
#'
#' Strips 'stringified' JSON of newlines and spaces following colons or commas.
#'
#' @param txt character: the stringified JSON to be concatenated.
#'
#' @return character: a string (stringified JSON), stripped of newlines and
#'   spaces following colons or commas.
#'
#' @examples
#' cat_json_str("{\n    'v1': [20],\n    'v2': [1.5]\n  }")
#'
#' @export
cat_json_str <- function(txt) {
  pattern_repl <- c("\n *" = "", ": " = ":", ", +\"" = ",\"")
  stringr::str_replace_all(txt, pattern_repl)
}

# ---------------------------------------------------------------------------- #
