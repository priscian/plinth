### N.B. The counter is assigned in "zzz.R".

#' @export
get_latex_plot_counter <- function()
{
  .pkgenv[["latex_plot_counter"]]
}

#' @export
increment_latex_plot_counter <- function()
{
  .pkgenv[["latex_plot_counter"]] <- .pkgenv[["latex_plot_counter"]] + 1L
}

#' @export
reset_latex_plot_counter <- function()
{
  .pkgenv[["latex_plot_counter"]] <- 1L
}


#' @export
latex_plot <- function(x, ...)
  UseMethod("latex_plot")


#' @export
latex_plot.default <- function(x, ...,
  lp_fun = plot,
  devices = list(`grDevices::pdf` = list(height = 8.27, width = 11.69), `devEMF::emf` = list(ext = "emf"), `grDevices::png` = list(height = 8.27, width = 11.69)),
  cap = "", short_caption = cap,
  path_base = "image", graphics_path = "./images/",
  latex,
  translate_latex = TRUE,
  create_plots = TRUE,
  float_placement = "!ht",
  graphics_width = "\\linewidth",
  graphics_height = NULL,
  set_par = NULL,
  print_after = FALSE,
  callback = NULL)
{
  caption_ <- cap

  if (missing(latex)) { # If not missing, make sure replacement uses the same "@@...@@" placeholders as below.
    latex <- "  %%
  \\begin{figure}[" %_% float_placement %_% "]
  \\begin{center}
    \\includegraphics[width=" %_% graphics_width %_% ifelse(is.null(graphics_height), "", ", height=" %_% graphics_height) %_% ", keepaspectratio]{@@FILEPATH@@}
  \\end{center}
  \\caption[@@SHORTCAPTION@@]{@@CAPTION@@\\label{fig:@@LABEL@@}}
  \\end{figure}"
  }

  figNum <- sprintf("%04d", get_latex_plot_counter())
  pathName <- graphics_path %_% path_base %_% "-" %_% figNum

  if (create_plots) {
    for (d in names(devices)) {
      argList <- devices[[d]]
      ext <- argList$ext
      if (is.null(ext)) ext <- d
      argList$ext <- NULL
      filePath <- paste(pathName, ext, sep = ".")
      argList$file <- filePath

      do_call(d, argList)
      if (!is.null(set_par)) eval_js(set_par, envir = parent.frame())
      if (!is.null(lp_fun)) { if (!print_after) lp_fun(x, ...) else print(lp_fun(x, ...)) }
      if (!is.null(callback)) eval_js(callback, envir = parent.frame())
      dev.off()
    }
  }

  ## Create LaTeX output.
  temp <- latex
  temp <- sub("@@CAPTION@@", gsub("\\\\", "\\\\\\\\", ifelse(translate_latex, Hmisc::latexTranslate(caption_, greek = TRUE), caption_)), temp)
  temp <- sub("@@SHORTCAPTION@@", gsub("\\\\", "\\\\\\\\", ifelse(translate_latex, Hmisc::latexTranslate(short_caption, greek = TRUE), caption_)), temp)
  temp <- sub("@@FILEPATH@@", basename(tools::file_path_sans_ext(pathName)), temp)
  temp <- sub("@@LABEL@@", basename(tools::file_path_sans_ext(pathName)), temp)

  cat(temp, "\n\n", sep = "")

  increment_latex_plot_counter()

  return (nop())
}
