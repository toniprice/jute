# ============================================================================ #
# rc_utils.R
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# get_rc_fld_vals
# ---------------------------------------------------------------------------- #
#' Get field values in an RC object (with optional replacement)
#'
#' Retrieves a list of field values from an RC object, replacing any existing
#' field values with those supplied.
#'
#' @param rc_obj an RC object.
#' @param vals list: fields to replace in \code{rc_obj}.
#'
#' @return list: existing fields in \code{rc_obj} with values replaced by any
#'   field values supplied in \code{vals}.
#'
#' @examples
#' thing_factory <- setRefClass(
#'   "thing", fields = list(name  = "character", value = "numeric")
#' )
#' flibgib <- thing_factory$new(name = "Flibberty Gibbet", value = 1e6)
#' get_rc_fld_vals(flibgib)
#' get_rc_fld_vals(flibgib, list(value = 25.6))
#'
#' @export
get_rc_fld_vals <- function(rc_obj, vals = NULL) {

  # nolint start
  # adapted from \url{http://stackoverflow.com/questions/18713847/return-a-list-of-fields-of-a-reference-class}
  # [retrieved 09 Jan 2020]
  # nolint end

  # retrieve list of fields and set all elements to NULL
  flds <- rc_obj$getRefClass()$fields()
  flds <- lapply(flds, function(x) NULL)

  # set field values to those in rc_obj
  for (nm in names(flds)) flds[[nm]] <- rc_obj$field(nm)
  # replace any values with those supplied in vals
  if (!is.null(vals)) flds[names(flds) %in% names(vals)] <- vals
  flds
}

# ---------------------------------------------------------------------------- #
