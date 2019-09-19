## Compare internal representations of R objects:
## http://stackoverflow.com/questions/9278715/value-reference-equality-for-same-named-function-in-package-namespace-environmen
#' @export
are_same <- function(x, y)
{
  f <- function(x) utils::capture.output(.Internal(inspect(x)))
  all(f(x) == f(y))
}


## These fuzzy equalities are necessary or useful sometimes.
#' @export
all_equal <- function(...) Vectorize(all.equal, "target", SIMPLIFY = FALSE)(...)


#' @export
is_equal <- function(..., simplify = TRUE) sapply(all_equal(...), function(x) is.logical(x) && x, simplify = simplify)


#' @export
is_invalid <- function(x, ...)
{
  if (missing(x) || is.null(x) || length(x) == 0L)
    return (TRUE)

  if (is.list(x))
    return (all(sapply(x, is_invalid)))
  else if (is.vector(x))
    return (all(is.na(x)))
  else
    return (FALSE)
}
