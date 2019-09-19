#' @export
get_dots <- function(..., evaluate = FALSE)
{
  caller <- sys.function(which = -1L)
  formalArguments <- NULL
  if (!is.null(caller)) {
    callerName <- as.list(sys.call(-1L))[[1L]]
    formalArguments <- names(formals(caller))
  }
  #unevaluated <- substitute(...()) # List of '...' name-value pairs.
  unevaluated <- eval(substitute(alist(...)))
  dotsAsCharacter <- unlist(sapply(unevaluated, deparse, simplify = TRUE))
  dotsNames <- names(dotsAsCharacter)
  if (is.null(dotsNames))
    dotsNames <- rep("", length(dotsAsCharacter))

  rv <- list()
  if (!is.null(sys.call(-2L)))
    rv$calling_function <- as.list(sys.call(-2L))[[1L]]
  rv$current_function <- callerName
  rv$current_formals <- formalArguments
  #rv$... <- environment()$`...`
  rv$arguments <- as.list(unevaluated)
  if (evaluate)
    rv$evaluated <- list(...)
  rv$as_character <- dotsAsCharacter
  rv$named_dots <- dotsNames
  whichDots <- which(formalArguments == "...")
  if (length(whichDots) == 0L)
    whichDots <- ifelse(length(formalArguments) == 0L, 1L, length(formalArguments))
  temp <- append(formalArguments, dotsNames[dotsNames != ""], after = whichDots)
  rv$all_named_args <- temp[temp != "..."]

  return (rv)
}


## https://stackoverflow.com/a/47955845/931941
#' @export
get_all_args <- function(defaults = FALSE) {
  ## Get formals of parent function.
  parentFormals <- formals(sys.function(sys.parent(n = 1)))

  ## Get names of assumed arguments.
  hasDots <- FALSE
  fnames <- names(parentFormals)
  if (any(fnames == "...")) {
    hasDots <- TRUE
    ## Remove '...' from list of parameter names.
    fnames <- fnames[-which(fnames == "...")]
  }

  ## Get current values for named variables in the parent frame.
  a <- evalq(as.list(environment()), envir = parent.frame())
  a <- a[fnames]

  ## Get the list of variables in '...'.
  if (hasDots)
    # a <- c(a, evalq(list(...), envir=parent.frame()))
    a <- c(a, evalq(get_dots(...)$arguments, envir = parent.frame()))

  if (defaults) {
    ## Get default values.
    defArgs <- as.list(parentFormals)
    defArgs <- defArgs[unlist(lapply(defArgs, FUN = function(x) class(x) != "name"))]
    a[names(defArgs)] <- defArgs
    setArgs <- evalq(as.list(match.call())[-1], envir = parent.frame())
    a[names(setArgs)] <- setArgs
  }

  a
}


#' Evaluate Function inside an Environment and Extract and Save Any Useful Variables from Its Body
#'
#' Puts a function's body and its arguments into an environment for evaluation, and afterwards allows extraction of any variables from the body, not just a return value.
#'
#' @param fun The function to be evaluated.
#' @param ... Arguments to be passed into \code{fun}.
#' @param arguments A list of additional arguments for passing into \code{fun}; can be used e.g. when the formal arguments of \code{fun} conflict with those of the current function.
#' @param envir Environment where \code{variables} will be copied after \code{fun} has been evaluated. For \code{action = "save"}, also names what variables in the evaluation environment will be \code{save()}d to an external file.
#' @param file_path For \code{action = c("save", "load")}, the path to the file to which the \code{variables} in \code{envir} will be written, or from which objects will be extracted to \code{envir}. If \code{timestamp = TRUE}, the file name provides a base name to which a timestamp is appended.
#' @param variables A character string naming variables among the arguments to, or in the body of, \code{fun} that will be extracted from the evaluation environment. If any of the strings are named, those names with carry the variables' values in \code{envir}.
#' @param copy_args Logical: Should all named arguments to \code{fun} also be extracted from the evaluation environment (and for \code{action = "save"}, saved)?
#' @param timestamp A logical value deciding whether a current timestamp (default format \code{\%Y-\%m-\%d+[seconds after midnight]}) should be appended to the base file name given as part of \code{file_path}.
#' @param action A character string denoting the purpose of calling \code{cordon()} in the first place:
#' \tabular{ll}{
#'   run \tab Evaluate \code{fun} and extract variables, but don't load or save them. \cr
#'   save \tab Evaluate \code{fun}, extract variables, and save them to an external file. \cr
#'   load \tab Load saved data from \code{file_path}. If \code{timestamp = TRUE}, load the most recent version according to the timestamped file name. \cr
#'   skip \tab Do nothing, i.e. prevent \code{fun} from being evaluated at all. \cr
#'   archive \tab Not implemented.
#' }
#'
#' @return The environment in which the body of \code{fun} was evaluated.
#'
#' @examples
#' \dontrun{
#' f <- function(x = "frog", ...) { args <- get_dots(...)$arguments; nines <- args$beast + 333; bite <- args$bite; return (nop()) }
#' e <- cordon(f, bite = "me", 3.14, beast = 666, TRUE, envir = globalenv(), variables = "nines")
#' get("nines", envir = globalenv())
#' e$bite
#' ls(e, all = TRUE)
#' }
#'
#' @export
cordon <- function(fun, ...,
  arguments = list(),
  envir = environment(),
  file_path = NULL,
  variables = NULL,
  copy_args = FALSE,
  timestamp = TRUE, timestamp... = list(),
  action = c("run", "save", "load", "skip", "archive"),
  evaluate_dots = TRUE,
  create_path_dir = TRUE,
  verbose = TRUE)
{
  action <- match.arg(action)
  run_ <- action == "run" || action == "save" || action == "load"
  save_ <- action == "save"
  load_ <- action == "load"
  archive_ <- action == "archive"

  timestampArgs <- list(
    use_seconds = TRUE,
    seconds_sep = '+'
  )
  timestampArgs <- utils::modifyList(timestampArgs, timestamp...)

  if (archive_) {
    if (is.null(file_path))
      stop("Archive file path must be specified.")
    if (!(file.info(file_path)$isdir)) file_path <- dirname(file_path)

    if (verbose) cat("Loading archive file \"" %_% filePath %_% "\".... ")
    archive("load", file_path) # 'archive()' not implemented yet.
    if (verbose) { cat("Done.", fill = TRUE); flush.console() }
  }
  else if (load_) {
    filePath <- file_path
    if (timestamp) {
      ## Get list of files in directory of 'file_path'.
      fileExt <- tools::file_ext(file_path)
      dirName <- dirname(file_path)
      timestampRe <- "_\\d{4}-\\d{2}-\\d{2}(?:\\" %_% timestampArgs$seconds_sep %_% "\\d{5})?"
      ## Find all versions of the file according to their timestamp extensions.
      filePaths <- sort(grep("^.*?" %_% timestampRe %_% "\\." %_% fileExt %_% "$", list.files(dirName, pattern = "^" %_% Hmisc::escapeRegex(tools::file_path_sans_ext(basename(file_path))) %_% timestampRe %_% "\\." %_% fileExt %_% "$", full.names = FALSE), perl = TRUE, value = TRUE), decreasing = TRUE)
      filePaths <- paste(dirName, filePaths, sep = "/")
      if (length(filePaths) > 0L)
        ## Use the most recent version of the file according to its timestamp extension.
        filePath <- filePaths[1L]
    }

    if (verbose) cat("Loading data file \"" %_% filePath %_% "\".... ")
    load(file = filePath, envir = envir)
    if (verbose) { cat("Done.", fill = TRUE); flush.console() }
  }
  else if (run_) {
    evalEnv <- new.env()

    ## Add default arguments of 'fun' to argument list.
    argList <- as.list(formals(fun))
    hasDots <- FALSE
    if (!is.null(argList[["..."]])) hasDots <- TRUE
    argList[["..."]] <- NULL
    dots <- get_dots(..., evaluate = evaluate_dots)
    ## Add '...' arguments to argument list.
    dotsArguments <- dots$arguments
    if (evaluate_dots) dotsArguments <- dots$evaluated
    argList <- utils::modifyList(argList, dotsArguments[dots$named_dots != ""]) # Replace duplicate named arguments with those from '...' and add new named arguments.
    argList <- c(argList, dotsArguments[dots$named_dots == ""]) # Tack on unnamed arguments from '...'.
    ## Add 'arguments' to 'argList'.
    argList <- utils::modifyList(argList, arguments[names(arguments) != ""]) # Replace duplicate named arguments with those from 'arguments' and add new named arguments.
    argList <- c(argList, arguments[names(arguments) == ""]) # Tack on unnamed arguments from 'arguments'.

    temp <- fun
    body(temp) <- as.call(c(as.name("{"), expression(return (environment()))))
    ## Return environment containing complete set of new arguments, including '...', for 'fun()'.
    evalEnv <- do.call(temp, argList)

    ## Evaluate the body of 'fun()' in the environment created.
    eval(body(fun), envir = evalEnv)

    ## Pick out the variables to keep.
    if (is.null(variables))
      variables <- setdiff(ls(evalEnv, all = TRUE), c(names(formals(fun))))

    ## N.B. Not used yet.
    variableNames <- variables
    if (!is.null(names(variables)))
      variableNames[names(variables) != ""] <- names(variables)[names(variables) != ""]

    argEnv <- as.environment(argList[names(argList) != ""]) # Can only save named arguments.
    if (!is.null(file_path)) {
      if (save_) {
        filePath <- file_path

        if (create_path_dir && !dir.exists(dirname(file_path)))
          dir.create(dirname(file_path), recursive = TRUE)

        if (timestamp)
          filePath <- paste(tools::file_path_sans_ext(file_path), do.call("make_current_timestamp", timestampArgs), sep='_') %_% '.' %_% file_ext(file_path)

        if (verbose) cat("Saving data file \"" %_% filePath %_% "\".... ")
        save(list = variables, file = filePath, envir = evalEnv)
        if (copy_args)
          append_rda(filePath, objects = ls(argEnv, all = TRUE), envir = argEnv)
        if (verbose) { cat("Done.", fill = TRUE); flush.console() }
      }
    }

    for (v in variables)
      assign(v, get(v, envir = evalEnv), envir = envir)
    if (copy_args) {
      for (a in ls(argEnv, all = TRUE))
        assign(a, get(a, envir = argEnv), envir = envir)
    }

    return (invisible(evalEnv))
  }
}


#' @export
eval_js <- function(..., envir = parent.frame(), enclos = if(is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv())
{
  dots <- get_dots(..., evaluate = TRUE)
  expr <- unlist(dots$evaluated)

  if (is.list(expr)) {
    if (is.function(expr[[1L]])) # If first '...' argument is a function, execute it with other '...' arguments as its own.
      return (do.call(expr[[1L]], tail(expr, -1L)))

    for (i in expr) {
      if (is.expression(i) || is.language(i)) {
        return (eval(i, envir, enclos)) # Returns only the first expression found.
      }
    }
  }

  expr <- paste(expr, collapse = " ")

  if (typeof(expr) != "character")
    return (expr)

  expr <- parse(text = expr)
  eval(expr, envir, enclos)
}



## Allow 'what' argument of 'do.call()' to include package path.
## V. https://stackoverflow.com/questions/10022436/do-call-in-combination-with/10037475#10037475
#' @export
do_call <- function(what, args, ...)
{
  if (is.function(what)) {
    what <- deparse(as.list(match.call())$what)
  }
  myFunCall <- parse(text = what)[[1]]
  myCall <- as.call(c(list(myFunCall), args))

  return (eval(myCall, ...))
}


## N.B. This may not work always (with 'by.x' and 'by.y'?). I have the following note elsewhere:
## m2 <- merge_fun_factory(all = FALSE, by.x = "Unique State ID Number", by.y = "sample")(m, quality) # Doesn't work; need to debug!
#' @export
merge_fun_factory <- function(FUN = base::merge, SETDIFF = TRUE, ...)
{
  if (SETDIFF)
    ## N.B. Note how '...' is NOT in 'function(x, y)'.
    function(x, y) FUN(x, y[, c(eval(get_dots(..., evaluate = TRUE)$evaluated$by), setdiff(colnames(y), colnames(x)))], ...)
  else
    function(x, y) FUN(x, y, ...)
}


#' @export
nop <- function(x=NULL)
{
  return (invisible(x))
}
