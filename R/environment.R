#' @export
copy_env <- function(e1)
{
  e2 <- new.env()
  for (n in ls(e1, all.names = TRUE)) assign(n, get(n, e1), e2)

  e2
}
