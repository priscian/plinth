### Constants

#' @export
no_yes_levels <- list(no = "FALSE", yes = "TRUE")
#' @export
yes_no_levels <- list(yes = "TRUE", no = "FALSE")


#' @export
unfactor  <- function(x, ...)
  UseMethod("unfactor")


#' @export
unfactor.factor <- function(x, ...)
{
  return (levels(x)[x])
}


#' @export
unfactor.data.frame <- function(x, columns = NULL, ...)
{
  isPointer <- FALSE
  if (inherits(x, "pointer")) isPointer <- TRUE

  if (!isPointer) {
    if (is.null(columns))
      columns <- seq(ncol(x))

    for (i in columns) {
      if (inherits(x[, i], "factor"))
        x[, i] <- unfactor(x[, i], ...)
    }

    return (x)
  }
  else {
    if (is.null(columns))
      columns <- seq(ncol(..(x)))

    for (i in columns) {
      if (inherits(..(x)[, i], "factor"))
        ..(x)[, i] <- unfactor(..(x)[, i], ...)
    }

    return (nop())
  }
}


#' @export
unfactor.pointer <- function(x, ...)
{
  if (inherits(..(x), "data.frame"))
    unfactor.data.frame(x, ...)
  else if (inherits(..(x), "factor"))
    return (unfactor.factor(..(x), ...))
  else
    stop("Pointer does not reference a relevant object type.")
}


#' @export
summary_f <- function(x, as.factor = FALSE, ..., factor... = NULL)
{
  if (is.null(factor...))
    factor... <- list()

  factorArgs <- list(
    x <- x
  )
  factorArgs <- utils::modifyList(factorArgs, factor...)

  if (as.factor)
    f <- as.factor(x)
  else
    f <- do.call("factor", factorArgs)

  summary(f, ...)
}


#' @export
drop_levels <- function(x, reorder = FALSE, ...)
  UseMethod("drop_levels")


#' @export
drop_levels.factor <- function(x, reorder = FALSE, ...)
{
  x <- x[, drop = TRUE]
  if(reorder) x <- stats::reorder(x, ...)

  return (x)
}


#' @export
drop_levels.data.frame <- function(x, reorder = FALSE, ...)
{
  isPointer <- FALSE
  if (inherits(x, "pointer")) isPointer <- TRUE

  if (!isPointer) {
    for (i in seq(ncol(x))) {
      if (inherits(x[, i], "factor"))
        x[, i] <- drop_levels(x[, i], reorder)
    }

    return (x)
  }
  else {
    for (i in seq(ncol(..(x)))) {
      if (inherits(..(x)[, i], "factor"))
        ..(x)[, i] <- drop_levels(..(x)[, i], reorder)
    }
  }
}


#' @export
drop_levels.pointer <- function(x, reorder = FALSE, ...)
{
  if (inherits(..(x), "data.frame"))
    drop_levels.data.frame(x, reorder, ...)
  else if (inherits(..(x), "factor"))
    return (drop_levels.factor(..(x), reorder, ...))
  else
    stop("Pointer does not reference a relevant object type.")
}

## usage:
# x <- data.frame(a = 1:5, b = letters[1:5], c = rep(3.14, 5), d = as.factor(c(1,2,1,3,4)))
# Hmisc::units(x$d) <- "mg^2"
# Hmisc::label(x$d) <- "duh"
# x1 <- x[1:4, ]
# x1$d
# x2 <- drop_levels(x1)
# x2$d


# Wrapper for 'addNA()' function that keeps factor attributes.
#' @export
add_na <- function(x, ...)
{
  x <- as.factor(x)

  classes <- class(x)
  att <- attributes(x); att <- att[names(att) %nin% c("levels")] # Save object attributes.

  x <- addNA(x, ...)

  attributes(x) <- utils::modifyList(attributes(x), att) # Restore object attributes.
  class(x) <- unique(classes, c(class(x)))

  return (x)
}


#' @export
order_levels <- function(x, ...,
  index = FALSE,
  frequency = TRUE,
  decreasing = TRUE,
  other = "other",
  other_last = TRUE,
  exclude = NA,
  add_na = FALSE,
  ifany = TRUE,
  by_name = FALSE)
{
  # 'x' is a factor variable or a variable that can be coerced into a factor.
  # '...' takes the names of factor levels to be first in order, in the order given.
  # If 'index' is true, factor levels named in '...' are considered indices for 'levels(x)'.
  # If 'frequency' is true, factor levels not named in '...' are sorted by frequency. See 'decreasing' below.
  # If frequency = TRUE and 'decreasing' is true, factor levels are sorted by decreasing frequency, increasing if false.
  # 'other' is a catch-all level for small-frequency categories, often in factors created by function 'merge_levels()'.
  # If other_last = FALSE, 'other' is ignored; if it's true, the category named by 'other' is made the last level.
  # 'exclude' is passed to function 'factor()'. See '?factor' for details.
  # If 'add_na' is true, the function 'addNA()' is called on 'x' to turn NA into an extra level at the end.
  # 'ifany': only add an NA level if it is used, i.e. if any(is.na(x)).
  # If 'by_name' is true, it overrides all other named arguments and calls 'gdata::reorder.factor(x, ...)'.

  if (by_name) return (gdata::reorder.factor(x, ...))

  classes <- class(x)
  att <- attributes(x); att <- att[names(att) %nin% c("levels")] # Save object attributes.

  l <- levels(x)
  ll <- as.list(l); names(ll) <- l
  hasNaLevel <- FALSE; if (any(is.na(l))) hasNaLevel <- TRUE
  ll <- ll[names(ll) %nin% exclude]
  levels(x) <- ll
  l <- levels(x)
  dots <- get_dots(...)
  s <- dots$as_character
  if (index)
    s <- l[as.integer(s)]

  tab <- table(x)

  o <- s
  temp <- l
  if (frequency)
    temp <- names(tab[order(tab, decreasing = decreasing)])
  keepLevels <- temp %nin% s
  if (other_last)
    keepLevels <- keepLevels & temp %nin% other
  temp <- temp[keepLevels]
  o <- c(o, temp)
  if (any(grepl("^" %_% other %_% "$", l)) && other_last)
    o <- c(o, other)

  x <- factor(x, levels = o)
  if (add_na || hasNaLevel)
    x <- addNA(x, ifany = ifany)

  attributes(x) <- utils::modifyList(att, attributes(x)) # Restore object attributes.
  class(x) <- unique(classes, c(class(x)))

  return (x)
}

## usage:
# save.seed <- get(".Random.seed", .GlobalEnv); set.seed(100)
# x <- sample(c(LETTERS[1:4], "other"), 50, TRUE); is.na(x) <- sample(seq_along(x), 5); x <- factor(x); summary(x)
# assign(".Random.seed", save.seed, .GlobalEnv)
# order_levels(x)
# order_levels(x, decreasing = FALSE)
# order_levels(x, other_last = FALSE)
# order_levels(x, decreasing = FALSE, other_last = FALSE)
# order_levels(x, B, D)
# order_levels(x, "B", "D")
# order_levels(x, 2, 4, index = TRUE) # Order by given level indices.
# order_levels(x, exclude=c(NA, "C"))
# order_levels(x, exclude=c(NA, "C"), add_na = TRUE)


#' @export
merge_levels <- function(x,
  new_levels,
  ...,
  other_level = "other",
  drop_levels = FALSE,
  label = NULL,
  sort = FALSE)
{
  # 'x' is a factor variable or a variable that can be coerced into a factor.
  # 'new_levels' is a named vector or list of new factor levels created by grouping the existing levels of 'x'. If a list item is numeric, it will be used as a vector index on 'levels(x)' to extract the corresponding level names.
  # '...' is further arguments to be passed to function 'order_levels()'.
  # 'other_level' is the name of a new level that will hold any levels of 'x' not taken up in 'new_levels'. If other_level = FALSE, remaining levels are left intact.
  # If 'drop_levels' is true, remove unused factor levels from returned factor.
  # 'label' is a character string used to add or change the 'label' attribute of the returned factor.
  # If 'sort' is true, call 'order_levels()' on the returned factor using arguments '...'.

  x <- as.factor(x)
  l <- levels(x)

  otherLevel <- other_level
  newLevels <- new_levels
  if (is.null(newLevels))
    newLevels <- seq(nlevels(x))
  newLevels <- sapply(newLevels, function(s) { if (is.numeric(s)) l[s] else s }, simplify = FALSE)
  if (is.null(names(newLevels)))
    names(newLevels) <- rep("", length(newLevels))
  names(newLevels)[names(newLevels) == ""] <- newLevels[names(newLevels) == ""]

  if (is.character(newLevels)) {
    names(newLevels) <- newLevels
    newLevels <- as.list(newLevels)
  }

  newFactor <- x
  otherList <- list()
  remainder <- setdiff(levels(x), unlist(newLevels))
  if (length(remainder) == 0L) remainder <- NULL
  if (is.logical(otherLevel)) {
    if (!otherLevel) {
      otherList <- as.list(remainder)
      names(otherList) <- otherList
    }
  }
  else
    otherList[[otherLevel]] <- remainder
  newLevels <- c(newLevels, otherList)
  levels(newFactor) <- newLevels

  if (!is.null(label))
    label(newFactor) <- label

  dots <- get_dots(...)
  if (!is_invalid(dots$arguments)) # I.e. if there are non-formal parameters in function call.
    sort <- TRUE
  if (sort)
    newFactor <- order_levels(newFactor, ...)

  if (drop_levels)
    newFactor <- droplevels(newFactor)

  return (newFactor)
}

## usage:
# save.seed <- get(".Random.seed", .GlobalEnv); set.seed(100)
# x <- sample(c(LETTERS[1:5]), 50, TRUE); is.na(x) <- sample(seq_along(x), 5); x <- factor(x); summary(x)
# assign(".Random.seed", save.seed, .GlobalEnv)
# summary(merge_levels(x, list(first = "A", second = c("B", "C"))))
# summary(merge_levels(add_na(x), seq(nlevels(x)))) # Merge NAs into "other" level.
# summary(merge_levels(x, list(first = 1, second = 2:3))) # Use indices instead.
# summary(merge_levels(x, list(first = "A", second = 2:3))) # Use indices instead.
# summary(merge_levels(x, as.list(1:3))) # Merge all but the levels given as indices into "other" level.
# summary(merge_levels(add_na(x), as.list(1:3))) # Merge NAs and all but the levels given as indices into "other" level.
# summary(merge_levels(add_na(x), list(first = c("A", NA), second = c("B", "C")))) # Merge NAs with an existing level.
# summary(merge_levels(x, list(first = "A", second = c("B", "C")), sort = TRUE)) # Call 'order_levels()' with default paramaters.
# summary(merge_levels(x, list(first = "A", second = c("B", "C")), other_last = FALSE))
# summary(merge_levels(x, list(first = "A", second = c("B", "C")), first, other_last = FALSE))
# summary(merge_levels(x, list(first = "A", second = c("B", "C")), first, other_last = FALSE, add_na = TRUE))
# summary(merge_levels(x, exclude = c(NA, "D"), list(first = "A", second = c("B", "C")), other_level = FALSE, first, second, E)) # Merge levels; order them (through 'order_levels()') as "first", "second", "E", but without renaming the "other" levels, and including "D" with the NAs.

#' @export
mlevels <- function(x, new_levels, ...)
{
  merge_levels(x, new_levels, ..., other_level = FALSE)
}

## usage:
# save.seed <- get(".Random.seed", .GlobalEnv); set.seed(100)
# x <- sample(c(LETTERS[1:5]), 50, TRUE); is.na(x) <- sample(seq_along(x), 5); x <- factor(x); summary(x)
# assign(".Random.seed", save.seed, .GlobalEnv)
# summary(merge_levels(x, list(first = "A", second = c("B", "C"))))
# summary(merge_levels(x, list(first = "A", second = c("B", "C")), other_level = FALSE))
# summary(mlevels(x, list(first = "A", second = c("B", "C"))))
# summary(mlevels(x, list(first = "A", second = c("C", "D"))))


#' @export
rename_levels <- function(x, ...,
  in_order = FALSE,
  merge... = NULL,
  order... = NULL,
  label = NULL)
{
  # 'x' is a factor variable or a variable that can be coerced into a factor.
  # '...' takes as arguments values for new levels of 'x' named for the original levels of 'x'. Unnamed arguments are processed after named ones and applied as new level names to levels not identified by the named arguments, starting with the first element of 'levels(x)'. Arguments for levels using unsyntactic identifiers (i.e. those not among letters, digits, the period, and the underscore; see also '?Quotes') should be enclosed by backticks (`).
  # If 'in_order' is true, the new factor's levels are ordered according to the order of the new level names in '...', by a call to 'order_levels()'.
  # 'merge...' takes a list of arguments to be passed to function 'merge_levels()'.
  # 'order...' takes a list of arguments to be passed to function 'order_levels()'.

  x <- as.factor(x)
  ol <- levels(x)

  dots <- get_dots(...)
  if (is_invalid(dots$arguments)) # I.e. if there are non-formal parameters in function call.
    return (x)

  nl <- ol
  names(nl) <- ol

  named <- dots$as_character[dots$named_dots != ""]
  if (!is_invalid(named)) {
    named <- named[names(named) %in% ol]
    null <- sapply(names(named), function (s) names(nl)[names(nl) %in% s] <<- named[s]); null <- NULL
  }

  unnamed <- dots$as_character[dots$named_dots == ""]
  if (!is_invalid(unnamed)) {
    if (length(unnamed) > length(nl) - length(named))
      unnamed <- rep(unnamed, length.out = length(nl) - length(named))
    repl <- names(nl)[names(nl) %in% ol]
    length(repl) <- length(unnamed)
    for (i in seq_along(repl))
      names(nl)[names(nl) == repl[i]] <- unnamed[i]
  }

  if (any(duplicated(names(nl))))
    warning("Duplicate levels merged.")

  levels(x) <- as.list(nl)

  if (in_order) {
    inOrderArgs <- list(
      x = x,
      frequency = FALSE,
      other_last = FALSE
    )
    dotsAsSymbol <- dots$arguments
    names(dotsAsSymbol) <- NULL
    inOrderArgs <- c(inOrderArgs, dotsAsSymbol)

    x <- do.call("order_levels", inOrderArgs)
  }

  if (!is.null(merge...)) {
    mergeArgs <- list(
      x = x
    )
    mergeArgs <- utils::modifyList(mergeArgs, merge...)

    x <- do.call("merge_levels", mergeArgs)
  }

  if (is.logical(order...)) {
    if (order...)
      order... <- list()
    else
      order... <- NULL
  }

  if (!is.null(order...)) {
    orderArgs <- list(
      x = x
    )
    orderArgs <- utils::modifyList(orderArgs, order...)

    x <- do.call("order_levels", orderArgs)
  }

  if (!is.null(label))
    label(x) <- label

  return (x)
}

## usage:
# save.seed <- get(".Random.seed", .GlobalEnv); set.seed(100)
# x <- sample(c(LETTERS[1:5]), 50, TRUE); is.na(x) <- sample(seq_along(x), 5); x <- factor(x); summary(x)
# assign(".Random.seed", save.seed, .GlobalEnv)
# summary(rename_levels(x)) # Return without change.
# summary(rename_levels(x, a, b, c))
# summary(rename_levels(x, a, B = b, c))
# summary(rename_levels(x, a, B = `my goodness`, c)) # Using backticks around unsyntactic level names.
# summary(rename_levels(x, D = d, B = b, c))
# summary(rename_levels(x, D = d, B = b, c, order... = TRUE)) # Same as 'order... = list()'.
# summary(rename_levels(x, D = d, B = b, c, order... = list()))
# summary(rename_levels(x, D = d, B = b, c, order... = list(add_na = TRUE)))
# summary(rename_levels(x, D = d, B = b, c, order... = list(3, 1, 2, index = TRUE)))
# summary(rename_levels(x, D = d, B = b, C)) # Warning for merge of duplicate level names.
# summary(rename_levels(x, a, E = e, c, R = duh, B = b, f, g))
# summary(rename_levels(x, D = d, B = b, c, merge... = list(list(duh = c("d", "E")))))
# summary(rename_levels(x, B = b, C = c, A = a, in_order = TRUE))
# summary(rename_levels(x, B = b, C = c, A = a, order... = list(frequency = FALSE, exclude = c(NA, "D")))) # Don't sort, make "D" into NA.


## Factorize continuous variable by quantile or other cutpoints.
#' @export
cut2q <- function(v, cuts, trim = FALSE, fmt = "%1.1f", dash = "--", ...)
{
  att <- attributes(v); att <- att[names(att) %nin% c("levels")] # Save object attributes.
  att$class <- c(att$class, "factor") # Note that 'c(NULL, "factor")' is the same as 'c("factor")'.

  if (missing(cuts)) {
    cuts <- stats::quantile(v, na.rm = TRUE)
    trim <- TRUE
  }
  if (trim)
    cuts <- head(tail(cuts, -1L), -1L)

  x <- sprintf(fmt, cuts)
  ranges <- character()
  for (i in seq_along(x)) {
    if (i == 1L) ranges <- c(ranges, "< " %_% x[i])
    if (i == length(x)) ranges <- c(ranges, ">= " %_% x[i])
    else ranges <- c(ranges, x[i] %_% dash %_% x[i + 1L])
  }

  f <- Hmisc::cut2(v, cuts, ...)
  levels(f) <- ranges

  attributes(f) <- utils::modifyList(attributes(f), att) # Restore object attributes.

  return (f)
}
