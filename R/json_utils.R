# ============================================================================ #
# json_utils.R
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# compact_json_str
# ---------------------------------------------------------------------------- #
#' Compact stringified JSON
#'
#' Strips 'stringified' JSON of newlines and spaces following colons or commas.
#'
#' @param txt character: the stringified JSON to be compacted.
#'
#' @return character: a string (stringified JSON), stripped of newlines and
#'   spaces following colons or commas.
#'
#' @examples
#' compact_json_str("{\n    'v1': [20],\n    'v2': [1.5]\n  }")
#'
#' @export
compact_json_str <- function(txt) {
  pattern_repl <- c("\n *" = "", ": " = ":", ", +\"" = ",\"")
  stringr::str_replace_all(txt, pattern_repl)
}

# ---------------------------------------------------------------------------- #
