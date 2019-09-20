#' @export
only_selected_series <- function(x, series, common_columns, sort = FALSE, range = NULL, x_var = NULL, ...)
{
  if (missing(series))
    series <- NULL

  colNames <- c(intersect(colnames(x), c(common_columns, series)))
  if (!sort)
    colNames <- char_sort(colNames, series)
  r <- x[, colNames, ...]

  if (!is.null(range)) {
    if (!is.null(x_var))
      xVar <- x_var
    else if ("yr_part" %in% colNames)
      xVar <- "yr_part"
    else if ("year" %in% colNames)
      xVar <- "year"

    r <- r[r[[xVar]] >= ifelse(is.na(range[1]), min(r[[xVar]], na.rm = TRUE), range[1]) & r[[xVar]] <= ifelse(is.na(range[2]), max(r[[xVar]]), range[2]), ]
  }

  r
}

#' @export
oss <- only_selected_series


#' @export
view_only_selected_series <- function(..., fun = View)
{
  fun(only_selected_series(...))
}

#' @export
vss <- view_only_selected_series


#' @export
plot_series <- function(
  x, # This should be a data frame.
  series,
  x_var,
  start = NULL, end = NULL,
  ma = NULL, ma_sides = 1L,
  plot_type = c("single", "multiple"),
  type = "l",
  main = "", xlab = "", ylab = "",
  unit = NULL,
  col = NULL, col_fun = colorspace::rainbow_hcl, col_fun... = list(l = 65), alpha = 0.5, lwd = 2,
  legend... = list(),
  trend = FALSE, trend_lwd = lwd, trend_legend_inset = c(0.2, 0.2),
  add = FALSE,
  segmented = FALSE, segmented... = list(),
  loess = FALSE, loess... = list(),
  plot.segmented... = list(),
  mark_segments = FALSE, vline... = list(),
  start_callback = NULL, end_callback = NULL,
  save_png = FALSE, save_png_dir = ".",
  png... = list(),
  ...
)
{
  plot_type <- match.arg(plot_type)

  ## This is to avoid an roxygen error described here: https://github.com/klutometis/roxygen/issues/592
  if (is.null(unit))
    unit <- "\u00b0C"

  y <- zoo::zoo(x[, c(x_var, series)], order.by = x[[x_var]])
  y <- subset(y, na_unwrap(as.matrix(y[, series]))) # Remove trailing NAs.
  w <- interpNA(y, "linear", unwrap = TRUE)

  ## Create moving-average variables if requested.
  w[, series] <- MA(w[, series], ma, sides = ma_sides); w <- zoo::zoo(w, order.by = zoo::index(y))
  maText <- ""
  if (!is.null(ma))
    maText <- "(" %_% ma %_% "-month moving average)"

  y <- stats::window(y, start = start, end = end, extend = TRUE) # N.B. This can't be 'extend()'ed.
  w <- stats::window(w, start = start, end = end, extend = TRUE)
  #wz <- zoo::as.zoo(w) # Unnecessary, but for legacy's sake.
  wz <- w

  ## Series colors.
  if (is.null(col)) {
    col <- seq_along(series)
    col_funArgs <- list(
      n = length(col)
    )
    col_funArgs <- utils::modifyList(col_funArgs, col_fun...)
    col <- suppressWarnings(do.call(col_fun, col_funArgs))
    ## Some other possible function calls:
    #col <- grDevices::rainbow(length(col))
    #col <- grDevices::terrain.colors(length(col))
    #col <- grDevices::topo.colors(length(col))
    #col <- suppressWarnings(RColorBrewer::brewer.pal(length(col), "Spectral")) # Or "Paired".
    #col <- colorRamps::matlab.like2(length(col)) # From package "colorRamps".
  }
  col <- rep(col, length.out = length(series))
  col <- scales::alpha(col, alpha)
  names(col) <- series

  ## Arguments for saving the plot.
  imageDir <- save_png_dir

  if (save_png) {
    pngArgs <- list(
      filename = paste(imageDir, filename, sep = "/"),
      width = 12.5,
      height = 7.3,
      units = "in",
      res = 600
    )
    pngArgs <- utils::modifyList(pngArgs, png...)
    do.call(grDevices::png, pngArgs)
  }

  ## Arguments for plotting.
  xaxt <- "n"
  if (dev.cur() == 1L) # If a graphics device is active, plot there instead of opening a new device.
    dev.new(width = 12.5, height = 7.3) # New default device of 1200 × 700 px at 96 DPI.
    #dev.new(width = 7.3, height = 7.3) # New default device of 700 × 700 px at 96 DPI.

  if (!add)
    plot(w[, series], plot.type = plot_type, type = "n", xaxs = "r", xaxt = "s", xlab = xlab, ylab = ylab, main = main, ...)
  if (maText != "") graphics::mtext(maText, 3L)

  if (!add) {
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"))
  }

  ## Evaluate expression after creating graphics device but before plotting series.
  if (!is.null(start_callback))
    eval(start_callback)

  par(new = TRUE)
  if (add) {
    plotSeries <- series
    for (i in seq_along(plotSeries))
      lines(wz[, plotSeries[i]], type = type, col = col[i], lwd = lwd, bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", ...) # I.e. 'plot.zoo()'.
  }
  else
    plot(wz[, series], screens = 1L, plot.type = plot_type, type = type, col = col, lwd = lwd, bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", ...) # I.e. 'plot.zoo()'.

  legendArgs <- list(
    x = "topleft",
    legend = series %_% ifelse(loess, " (+ LOESS)", ""),
    col = col,
    lwd = lwd,
    bty = "n",
    cex = 0.8
  )
  legendArgs <- utils::modifyList(legendArgs, legend...)
  do.call(graphics::legend, legendArgs)

  ## LOESS smooth.
  if (loess) local({
    for (s in series) {
      loessArgs = list(
        formula = eval(substitute(s ~ x, list(s = as.name(s), x = as.name(x_var)))),
        data = y[, c(x_var, series)],
        span = 0.2
      )
      loessArgs <- utils::modifyList(loessArgs, loess...)

      l <- do.call(stats::loess, loessArgs)

      lwd <- 2
      if (!is.null(loessArgs$lwd))
        lwd <- loessArgs$lwd
      lines(l$x, l$fit, col = col[s], lwd = lwd)
    }
  })

  r <- list()

  ## Linear trends.
  if (trend) local({
    m <- list()
    m$series <- series
    m$range <- list(start = start(wz), end = end(wz))
    m$col <- col
    m$data <- y
    for (s in m$series) {
      m[[s]]$lm <- stats::lm(eval(substitute(b ~ x, list(b = as.name(s), x = as.name(x_var)))), data = m$data, x = TRUE)
      m[[s]]$warming <- stats::coef(m[[s]]$lm)[2] * diff(range(m[[s]]$lm$model[, 2]))
      m[[s]]$rate <- stats::coef(m[[s]]$lm)[2] * 10
      m[[s]]$rateText <- eval(substitute(expression(paste(Delta, " = ", r, phantom(l), unit, "/dec.", sep = "")), list(r = sprintf(m[[s]]$rate, fmt = "%+1.3f"), unit = unit)))
      m[[s]]$col <- m$col[s]
    }

    legendText <- NULL
    for (s in m$series) {
      ## Set clipping region for 'abline()'.
      xRange <- range(wz[!is.na(wz[, s]), x_var], na.rm = TRUE)
      yRange <- range(wz[, s], na.rm = TRUE)
      usr <- par("usr")
      clip(xRange[1], xRange[2], yRange[1], yRange[2])

      abline(m[[s]]$lm, col = m[[s]]$col, lwd = trend_lwd, untf = TRUE)

      ## Reset clipping to plot region.
      do.call(graphics::clip, as.list(usr))

      legendText <- c(legendText, m[[s]]$rateText)
    }

    if (!is.null(trend_legend_inset))
      #legend("bottomright", inset = trend_legend_inset, legend = legendText, col = m$col, lwd = trend_lwd, bty = "n", cex = 0.8)
      legend("topright", legend = legendText, col = m$col, lwd = trend_lwd, bty = "n", cex = 0.8)

    r$trend <<- m
  })

  ## Segmented linear regression.
  if (segmented) local({
    segmentedArgs <- list(
      x = x,
      series = series,
      col = col,
      start = start,
      end = end,
      x_var = x_var
    )
    segmentedArgs <- utils::modifyList(segmentedArgs, segmented...)
    sm <- do.call("fit_segmented_model", segmentedArgs)

    for (i in names(sm$piecewise)) {
      ## Set clipping region for 'plot.segmented()' and 'abline()'.
      xRange <- range(wz[!is.na(wz[, i]), x_var], na.rm = TRUE)
      yRange <- range(wz[, i], na.rm = TRUE)
      usr <- graphics::par("usr")
      clip(xRange[1], xRange[2], yRange[1], yRange[2])

      x <- sm$piecewise[[i]]$sm

      if (!is.null(x) && inherits(x, "segmented")) {
        plot.segmentedArgs <- list(
          x = x,
          add = TRUE,
          rug = FALSE,
          lwd = 2,
          #lty = "longdash",
          col = col[i],
          alpha = alpha
        )
        plot.segmentedArgs <- utils::modifyList(plot.segmentedArgs, plot.segmented...)
        dev_null <- do.call("plot", plot.segmentedArgs)

        if (mark_segments) {
          vlineArgs <- list(
            mark_x = sprintf(sm$piecewise[[i]]$sm$psi[, 2], fmt = "%1.1f")
          )
          vlineArgs <- utils::modifyList(vlineArgs, vline...)
          do.call("vline", vlineArgs)
        }
      } else {
        lwd <- ifelse(is.null(plot.segmented...$lwd), 2, plot.segmented...$lwd)
        abline(sm$piecewise[[i]]$lm, col = col[i], lwd = lwd, untf = TRUE)
      }

      ## Reset clipping to plot region.
      do.call(graphics::clip, as.list(usr))
    }

    r$segmented <<- sm
  })

  if (!is.null(end_callback))
    eval(end_callback)

  if (length(r) > 0L)
    return (invisible(r))

  return (nop())
}
