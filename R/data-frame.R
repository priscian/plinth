#' @import data.table

#' @export
dataframe <- function (..., row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE, stringsAsFactors = FALSE)
{
  data.frame(..., row.names = row.names, check.rows = check.rows, check.names = check.names, fix.empty.names = fix.empty.names, stringsAsFactors = stringsAsFactors)
}


#' @export
renumber  <- function(x, ...)
  UseMethod("renumber")


#' @export
renumber.pointer <- function(x, ...)
{
  if (inherits(..(x), "data.frame"))
    renumber.data.frame(x, ...)
  else
    stop("Pointer does not reference a relevant object type.")
}


#' @export
renumber.data.frame <- function(x, ...)
{
  row.names(..(x)) <- NULL
}
