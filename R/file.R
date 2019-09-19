## Allow renaming of objects before 'save()'ing them.
## Expands on https://stackoverflow.com/questions/21248065/r-rename-r-object-while-save-ing-it/21248218#21248218
#' @export
saver <- function(..., file = stop("'file' must be specified"), save... = list(), evaluate_dots = TRUE)
{
  d <- get_dots(..., evaluate = evaluate_dots)
  l <- d$arguments
  if (evaluate_dots)
    l <- d$evaluated
  if (is_invalid(names(l)))
    names(l) <- ""
  names(l)[names(l) == ""] <- sapply(d$arguments, toString)[names(l) == ""]

  if (!is_invalid(save...$list)) {
    e <- new.env()
    dev_null <- sapply(save...$list, function(s) assign(s, get(s), envir = e))
    dev_null <- NULL

    l <- modifyList(l, as.list(e))
    save...$list <- NULL
  }

  saveArgs <- list(
    list = names(l),
    file = file,
    #eval.promises = TRUE,
    envir = list2env(l)
  )
  saveArgs <- utils::modifyList(saveArgs, save...)

  do.call("save", saveArgs)
}
## usage:
# foo <- list(beast = 666, "test", 3.14)
# frog <- "fish"
# saver(bar = foo, frig = frog, file = "hi.RData")
## Save object using the 'list' parameter of 'save()':
# saver(moo = frog, save... = list(list = c("foo")), file = "hi2.RData")


#' @export
append_rda <- function(file_path, objects = character(), envir = parent.frame(), remove = FALSE, ...)
{
  # 'file' is the name of the file where the data will be saved; if 'file' doesn't exist it will be created.
  # 'objects' is a character vector containing the names of objects to be appended to 'file'.
  # 'envir' is the environment to search for objects to be saved.
  # If 'remove' is true, remove all other objects from 'file' besides those named in 'objects'.

  e <- new.env()
  .null <- NULL # 'save()' requires storage of at least one variable, so fake it.

  if (!file.exists(file_path))
    save(.null, file = file_path)

  load(file_path, envir = e)

  if (!remove) {
    for (o in objects)
      assign(o, get(o, envir = envir), envir = e)
    variables <- ls(e, all.names = TRUE)
  }
  else
    variables <- setdiff(ls(e, all.names = TRUE), objects)

  if (length(variables) == 0)
    variables <- ".null"

  save(list = variables, file = file_path, envir = e, ...)

  return (nop())
}


#' @export
clipwd <- function(use_dirname = TRUE, dir, source_files = TRUE, verbose = TRUE, ...)
{
  if (missing(dir))
    dir <- utils::readClipboard() # Windows only, try 'scan("clipboard", what="")[1]' otherwise.

  if (use_dirname)
    dir <- dirname(dir)

  setwd(dir, ...)

  if (source_files) {
    files <- choose.files(filters = Filters[c("R"), ])
    sourceCommands <- NULL
    for (f in files) {
      #sourceCommand <- "source(\"./" %_% basename(f) %_% "\", keep.source = FALSE)"
      sourceCommand <- "source(\"" %_% normalizePath(f, "/") %_% "\", keep.source = FALSE)"
      sourceCommands <- c(sourceCommands, sourceCommand)
      if (verbose)
        cat("Running command '" %_% sourceCommand %_% "'.... ")
      ## N.B. 'writeClipboard()' automatically ends character strings with '\n'; convert to raw to prevent this.
      tryCatch(source(f, keep.source = FALSE), # Need to add extra "raw" to raw string to prevent deletion of last character.
        finally = {
          b <- charToRaw(paste(sourceCommands, collapse = "\n"))
          b[length(b) + 1L] <- as.raw(0)
          utils::writeClipboard(b, format = 1L)
          ## Write command directly to history:
          timestamp(stamp = paste(sourceCommands, collapse = "\n"), prefix = "", suffix = "", quiet = verbose)
        }
      )
      if (verbose) { cat("Done.", fill = TRUE); flush.console() }
    }
  }
}


#' @export
clip_wd <- function(..., source_files = FALSE)
{
  clipwd(..., source_files = source_files)
}
