# ---------------------------------------------------------------------------- #
# simplify_lst_lst
# ---------------------------------------------------------------------------- #
#' Simplify list of lists
#'
#' Simplify a list of lists by eliminating the top-level list.
#'
#' @param lst a list of lists, each of which should be moved up one level in the
#'   list hierarchy.

#' @param unnamed_prefix character: string to use as a prefix for naming unnamed
#'   list items in the event that any list items are unnamed.

#'
#' @return list: the items in \code{lst} but each moved one level up in the list
#'   hierarchy, resulting in a list of single elements.
#'
#' @details
#' List names are handled as follows: if any list items are named, these names
#' will be set in the returned list. If any list items are unnamed, their names
#' will be set to the default value prefixed by \code{unnamed_prefix} with an
#' integer suffix starting at 1 and incremented for each unnamed item.
#'
#' @examples
#' a_1 <- "pineapple"
#' a_2 <- list(uno = 1, due = 2, tre = 3)
#' a_3 <- c(1, 4, 9, 16, 25)
#' lst <- list(list(z_1 = a_1), list(z_2 = a_2), list(z_3 = a_3))
#' simplify_lst_lst(lst)
#'
#' a_1 <- "pineapple"
#' a_2 <- list(uno = 1, due = 2, tre = 3)
#' a_3 <- c(1, 4, 9, 16, 25)
#' lst <- list(list(a_1), list(z = a_2), list(a_3))
#' simplify_lst_lst(lst)
#'
#' @export
simplify_lst_lst <- function(lst, unnamed_prefix = "name") {

  cls_chk <- unlist(lapply(lst, class))
  if (length(unique(cls_chk)) != 1 || unique(cls_chk) != "list") {
    fmt <- "Argument 'lst' should be a list of lists but found classes: %s"
    stop(sprintf(fmt, paste(cls_chk, collapse = ", ")))
  }

  lst_items <- lapply(lst, "[[", 1)

  lst_names <- lapply(lst, names)

  # set default names for any unnamed items
  nulls <- which(vapply(lst_names, is.null, FUN.VALUE = logical(1)))
  lst_names[nulls] <- get_incremental_names(length(nulls), unnamed_prefix)

  setNames(lst_items, lst_names)
}

# ---------------------------------------------------------------------------- #
