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
#'
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
