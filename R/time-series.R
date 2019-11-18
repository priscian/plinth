#' @export
only_selected_series <- function(x, series, common_columns = NULL, sort = FALSE, range = NULL, x_var = NULL, ...)
{
  if (missing(series))
    series <- NULL

  colNames <- c(intersect(colnames(x), c(common_columns, series)))
  if (!sort)
    colNames <- char_sort(colNames, series)
  r <- x[, colNames, ...]

  if (!is.null(range)) {
    xVar <- x_var
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
  col = NULL, col_fun = colorspace::rainbow_hcl, col_fun... = list(l = 65), alpha = 0.5, lwd = 2,
  main = "", xlab = "", ylab = "",
  unit = NULL,
  add = FALSE,
  conf_int = FALSE, conf_int_suffix = "_uncertainty", ci_alpha = 0.3, polygon... = list(),
  trend = FALSE, trend_lwd = lwd, trend_legend_inset = c(0.2, 0.2), trend... = list(), extra_trends = list(),
  loess = FALSE, loess... = list(), loess_series = NULL, lines.loess... = list(),
  segmented = FALSE, segmented... = list(),
  plot.segmented... = list(),
  mark_segments = c("none", "lines", "points"), vline... = list(), points.segmented... = list(),
  legend... = list(),
  start_callback = NULL, end_callback = NULL,
  save_png = FALSE, save_png_dir = ".", save_png_filename = "image.png",
  png... = list(),
  ...
)
{
  plot_type <- match.arg(plot_type)
  mark_segments <- match.arg(mark_segments)

  ## This is to avoid an roxygen error described here: https://github.com/klutometis/roxygen/issues/592
  if (is.null(unit))
    unit <- "\u00b0C"

  ci_series <- NULL
  if (conf_int)
    ci_series <- grep(conf_int_suffix %_% "$", colnames(x), perl = TRUE, value = TRUE)

  y <- zoo::zoo(x[, c(x_var, series, ci_series)], order.by = x[[x_var]])
  y <- subset(y, na_unwrap(as.matrix(y[, c(series, ci_series)]))) # Remove trailing NAs.
  w <- interpNA(y, "linear", unwrap = TRUE)

  ## Create moving-average variables if requested.
  w[, c(series, ci_series)] <- MA(w[, c(series, ci_series)], ma, sides = ma_sides); w <- zoo::zoo(w, order.by = zoo::index(y))
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
  if (save_png) {
    pngArgs <- list(
      filename = paste(save_png_dir, save_png_filename, sep = "/"),
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

  ## N.B. I need more control over the grid here:
  if (!add) {
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"))
  }

  ## Evaluate expression after creating graphics device but before plotting series.
  if (!is.null(start_callback))
    eval(start_callback)

  if (conf_int) { # Plot confidence bands for series that have them.
    confintNames <- intersect(series %_% conf_int_suffix, colnames(w))
    if (length(confintNames) != 0L) {
      seriesNames <- stringr::str_match(confintNames, "^(.*?)" %_% conf_int_suffix %_% "$")[, 2L]
      for (i in seq_along(confintNames)) {
        value <- w[, seriesNames[i]]
        ci <- w[, confintNames[i]]
        upper <- value + ci/2; lower <- value - ci/2
        ciCol <- scales::alpha(col[seriesNames[i]], ci_alpha)
        cidf <- dataframe(x = y[, x_var], lower = lower, upper = upper) %>% tidyr::drop_na()

        polygonArgs <- list(
          x = c(cidf$x, rev(cidf$x)),
          y = c(cidf$upper, rev(cidf$lower)),
          col = ciCol,
          border = NA
        )
        polygonArgs <- utils::modifyList(polygonArgs, polygon...)
        do.call(graphics::polygon, polygonArgs)
      }
    }
  }

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
    loessSeries <- series
    if (!is_invalid(loess_series))
      loessSeries <- loess_series
    for (s in loessSeries) {
      loessArgs = list(
        formula = eval(substitute(s ~ x, list(s = as.name(s), x = as.name(x_var)))),
        data = y[, c(x_var, series)],
        span = 0.2
      )
      loessArgs <- utils::modifyList(loessArgs, loess...)

      l <- do.call(stats::loess, loessArgs)

      lines.loessArgs <- list(
        x = drop(l$x),
        y = l$fit,
        lwd = 2
      )
      lines.loessArgs <- modifyList(lines.loessArgs, lines.loess...)
      if (is_invalid(lines.loessArgs$col))
        lines.loessArgs$col = col[s]

      do.call(graphics::lines, lines.loessArgs)
    }
  })

  r <- list()

  ## Linear trends.
  if (trend) local({
    trendArgs <- list(
      data = as.data.frame(y[, c(x_var, series)]),
      range = range(as.data.frame(w)[, x_var], na.rm = TRUE),
      lwd = trend_lwd,
      legend_inset = trend_legend_inset,
      fmt = "%+1.2f",
      unit = unit,
      trend_multiplier = 0,
      denom_text = "/" %_% x_var,
      keep_default_trends = TRUE,
      sort_by_name = FALSE
    )
    if (!is_invalid(trend...$keep_default_trends))
      trendArgs$keep_default_trends <- trend...$keep_default_trends
    if (trendArgs$keep_default_trends) {
      if (is_invalid(trendArgs$m))
        trendArgs$m <- list()
      length(trendArgs$m) <- length(series); names(trendArgs$m) <- series
    }
    trendArgs <- utils::modifyList(trendArgs, trend...)
    if (!is_invalid(extra_trends))
      trendArgs$m <- append(trendArgs$m, extra_trends)

    for (i in seq_along(trendArgs$m)) {
      if (is_invalid(trendArgs$m[[i]]$range))
        trendArgs$m[[i]]$range <- trendArgs$range
      if (is_invalid(trendArgs$m[[i]]$col))
        trendArgs$m[[i]]$col <- col[names(trendArgs$m)[i]]
      if (is_invalid(trendArgs$m[[i]]$lwd))
        trendArgs$m[[i]]$lwd <- trendArgs$lwd

      trendArgs$m[[i]]$sdata <- trendArgs$data %>%
        dplyr::select(c(intersect(common_columns, colnames(trendArgs$data)), names(trendArgs$m)[i])) %>%
        dplyr::filter(!!as.name(x_var) >= trendArgs$m[[i]]$range[1] & !!as.name(x_var) <= trendArgs$m[[i]]$range[2])
      trendArgs$m[[i]]$lm <- lm(eval(substitute(b ~ x, list(b = as.name(names(trendArgs$m)[i]), x = as.name(x_var)))), data = trendArgs$m[[i]]$sdata, x = TRUE)
      trendArgs$m[[i]]$change <- coef(trendArgs$m[[i]]$lm)[2] * diff(range(trendArgs$m[[i]]$lm$model[, 2]))
      trendArgs$m[[i]]$rate <- coef(trendArgs$m[[i]]$lm)[2] * trendArgs$trend_multiplier
      trendArgs$m[[i]]$rateText <- eval(substitute(expression(paste(Delta, " = ", r, phantom(l), unit, denom_text, sep = "")), list(r = sprintf(trendArgs$m[[i]]$rate, fmt = trendArgs$fmt), unit = trendArgs$unit, denom_text = trendArgs$denom_text)))
    }
    if (trendArgs$sort_by_name)
      trendArgs$m <- trendArgs$m[sort(names(trendArgs$m))]

    legendText <- NULL
    for (i in seq_along(trendArgs$m)) {
      ## Set clipping region for 'abline()'.
      xRange <- range(trendArgs$m[[i]]$sdata[!is.na(trendArgs$m[[i]]$sdata[, names(trendArgs$m)[i]]), x_var], na.rm = TRUE)
      yRange <- range(trendArgs$m[[i]]$sdata[, names(trendArgs$m)[i]], na.rm = TRUE)

      usr <- par("usr")
      graphics::clip(xRange[1], xRange[2], yRange[1], yRange[2])

      graphics::abline(trendArgs$m[[i]]$lm, col = trendArgs$m[[i]]$col, lwd = trendArgs$m[[i]]$lwd)

      ## Reset clipping to plot region.
      do.call(graphics::clip, as.list(usr))

      legendText <- c(legendText, trendArgs$m[[i]]$rateText)
    }

    if (!is.null(trendArgs$legend_inset))
      legend("bottomright", inset = trendArgs$legend_inset, legend = legendText, col = sapply(trendArgs$m, function(a) a$col), lwd = sapply(trendArgs$m, function(a) a$lwd), bty = "n", cex = 0.8)

    r$trend <<- trendArgs$m
  })

  ## Segmented linear regression.
  if (segmented) local({
    segmentedArgs <- list(
      x = x,
      series = series,
      start = start,
      end = end,
      x_var = x_var
    )
    if (!is_invalid(segmented...$x) && is.list(segmented...$x)) { # V. "?utils::modifyList" for why this is necessary.
      segmentedArgs$x <- segmented...$x
      segmented...$x <- NULL
    }
    segmentedArgs <- utils::modifyList(segmentedArgs, segmented...)
    sm <- do.call("fit_segmented_model", segmentedArgs)

    for (i in names(sm$piecewise)) {
      ## Set clipping region for 'plot.segmented()' and 'abline()'.
      xRange <- range(wz[!is.na(wz[, i]), x_var], na.rm = TRUE)
      yRange <- range(wz[, i], na.rm = TRUE)
      usr <- graphics::par("usr")
      graphics::clip(xRange[1], xRange[2], yRange[1], yRange[2])

      x <- sm$piecewise[[i]]$sm

      plot.segmentedArgs <- list(
        x = x,
        add = TRUE,
        rug = FALSE,
        #lty = "longdash",
        lwd = 2,
        pch = NA_integer_, # Not necessary
        col = col[i]
      )
      plot.segmentedArgs <- utils::modifyList(plot.segmentedArgs, plot.segmented...)

      if (!is.null(x) && inherits(x, "segmented")) {
        dev_null <- do.call("plot", plot.segmentedArgs)

        if (mark_segments != "none") {
          ## Reset clipping to whole plot region.
          do.call(graphics::clip, as.list(usr))

          if (mark_segments == "lines") {
            vlineArgs <- list(
              mark_x = sprintf(sm$piecewise[[i]]$sm$psi[, 2], fmt = "%1.1f")
            )
            vlineArgs <- utils::modifyList(vlineArgs, vline...)
            do.call("vline", vlineArgs)
          } else if (mark_segments == "points") {
            points.segmentedArgs <- list(
              x = sm$piecewise[[i]]$sm,
              col = plot.segmentedArgs$col,
              pch = 4 # Like '×'
            )
            points.segmentedArgs <- utils::modifyList(points.segmentedArgs, points.segmented...)
            do.call(graphics::points, points.segmentedArgs)
          }

          ## Turn clipping back on for any further plotting.
          graphics::clip(xRange[1], xRange[2], yRange[1], yRange[2])
        }
      } else {
        abline(sm$piecewise[[i]]$lm, col = plot.segmentedArgs$col, lwd = plot.segmentedArgs$lwd)
      }

      ## Reset clipping to plot region.
      do.call(graphics::clip, as.list(usr))
    }

    r$segmented <<- sm
  })

  if (!is.null(end_callback))
    eval(end_callback)

  if (save_png)
    dev.off()

  if (length(r) > 0L)
    return (invisible(r))

  return (nop())
}


## Style suggested by https://tamino.wordpress.com/2014/12/04/a-pause-or-not-a-pause-that-is-the-question/
#' @export
plot_sequential_trend <- function(
  x, # A data frame
  series,
  x_var,
  start = NULL, end = NULL,
  trend_multiplier = 1,
  main = NULL, xlab = NULL, ylab = NULL,
  use_polygon = FALSE,
  mark_xs = NULL,
  baseline = FALSE,
  plot... = list(),
  ci... = list(),
  vline... = list(),
  m_text = NULL,
  ...
)
{
  d <- x
  if (!is.null(start))
    d <- x[x[[x_var]] >= start, ]

  series <- series[1L]

  means <- unclass(by(d[[series]], d[[x_var]], mean, na.rm = TRUE))
  xNames <- names(means)
  means <- as.vector(means); names(means) <- xNames
  means <- means[na_unwrap(means)]
  rates <- rep(NA, length(means))
  rates <- data.frame(rate = rates, lwr = rates, upr = rates)
  rownames(rates) <- names(means)
  xs <- as.numeric(rownames(rates))

  local({
    for (i in seq_along(means)) {
      y <- means[i:length(means)]
      x <- as.numeric(names(y))

      m <- stats::lm(y ~ x)
      r <- stats::coef(m)[2L] * trend_multiplier
      ci <- suppressWarnings(t(stats::confint(m, "x"))) * trend_multiplier
      rates[i, ] <<- c(r, ci)
    }
  })

  if (is.null(start)) start <- head(xs, 1L)
  if (is.null(end)) end <- tail(xs, 1L)

  attr(rates, "series") <- series
  attr(rates, "trend_start") <- start
  attr(rates, "trend_end") <- tail(xs, 1L)

  ccRates <- rates[xs <= end & stats::complete.cases(rates), ]
  ccXs <- as.numeric(rownames(ccRates))[xs <= end & stats::complete.cases(rates)]

  plotArgs <- list(
    x = ccXs,
    y = ccRates[, "rate"],
    type = "o",
    pch = 16,
    #ylim = c(-1.0, 1.0),
    ylim = c(min(ccRates[, "lwr"]), max(ccRates[, "upr"])),
    lwd = 2,
    col = "black",
    panel.first = quote(abline(h = 0.0, col = "darkgray", lty = "dashed")),
    xlab = ifelse(!is_invalid(xlab), xlab, "Start value of trend"),
    ylab = ifelse(!is_invalid(ylab), ylab, "Trend"),
    main = ifelse(!is_invalid(main), main, "Linear Trend + 95% CIs")
  )
  plotArgs <- utils::modifyList(plotArgs, plot...)

  if (is.language(plotArgs$main))
    plotArgs$main <- eval(plotArgs$main)

  if (is.null(m_text))
    m_text <- quote("Trend from start value to " %_% tail(names(means), 1))

  if (dev.cur() == 1) # If a graphics device is active, plot there instead of opening a new device.
    dev.new(width = 12.5, height = 7.3) # New default device of 1200 × 700 px at 96 DPI.
  do.call(plot, plotArgs)
  mtext(eval(m_text))

  if (use_polygon) {
    ciArgs <- list(
      x = c(ccXs, rev(ccXs)),
      y = c(ccRates[, "lwr"], rev(ccRates[, "upr"])),
      col = scales::alpha("gray", 0.6),
      border = NA # Border color; NULL means use par("fg"), NA omits borders.
    )
    ciArgs <- modifyList(ciArgs, ci...)

    do.call(graphics::polygon, ciArgs)
  }
  else { # Use error bars to show confidence intervals.
    ciArgs <- list(
      x0 = ccXs,
      y0 = ccRates[, "lwr"],
      x1 = ccXs,
      y1 = ccRates[, "upr"],
      length = 0.03,
      angle = 90,
      code = 3
    )
    ciArgs <- utils::modifyList(ciArgs, ci...)

    do.call(graphics::arrows, ciArgs)
  }

  if (!is.null(mark_xs)) {
    vlineArgs <- list(
      mark_x = mark_xs,
      abline... = list(abline... = list(col = scales::alpha("red", 0.4)))
    )
    vlineArgs <- utils::modifyList(vlineArgs, vline...)

    do.call(vline, vlineArgs)
  }

  return (rates)
}


#' @export
create_smooth_variables <- function(
  x,
  series = NULL,
  x_var,
  unwrap = TRUE,
  force_names = TRUE,
  pad_by = NULL,
  seq... = list(),
  interpNA... = list(),
  loess_suffix = " (LOESS fit)",
  loess... = list(),
  interpolated_suffix = " (interpolated)",
  keep_interpolated = FALSE,
  deriv = NULL, # 0, 1, or 2; or some combo of them
  frfast... = list(),
  deriv_suffix_template = " (derivative %d)",
  lower_ci_suffix = "_lower", upper_ci_suffix = "_upper",
  interpolated_derivative = FALSE,
  plot_series_friendly = TRUE,
  verbose = FALSE,
  ...
)
{
  if (is.null(series))
    series <- colnames(x)[colnames(x) != x_var]

  d <- tibble::as_tibble(x)
  if (unwrap)
    d <- subset(d, na_unwrap(d[, series]))

  seqArgs <- list(
    from = min(d[[x_var]], na.rm = TRUE),
    to = max(d[[x_var]], na.rm = TRUE),
    by = pad_by
  )
  seqArgs <- utils::modifyList(seqArgs, seq...)

  if (!is.null(pad_by)) {
    attrs <- attributes(d)

    seq_vals <- do.call(seq, seqArgs)
    temp <- sort(unique(c(d[[x_var]], seq_vals)))
    tbl <- dataframe(temp); names(tbl) <- x_var
    d <- dplyr::right_join(d, tbl, by = x_var)

    ## dplyr joins remove attributes, so:
    missingAttrs <- dplyr::setdiff(names(attrs), names(attributes(d)))
    plyr::l_ply(missingAttrs,
      function(i)
      {
        attr(d, i) <<- attrs[[i]]
      })
  }

  interpNAArgs <- list(
    method = "fmm"
  )

  if (is.numeric(series) && force_names)
    series <- colnames(d)[series]

  attr(d, "frfast") <- list()

  for (i in series) {
    if (verbose) cat("  Processing column \"" %_% i %_% "\".... ")

    interpNAArgs$x <- d[, i]
    interpNAArgs <- utils::modifyList(interpNAArgs, interpNA...)
    d[[i %_% interpolated_suffix]] <- drop(do.call(interpNA, interpNAArgs))

    loessArgs = list(
      formula = eval(substitute(s ~ x_var, list(s = as.name(i %_% interpolated_suffix), x_var = as.name(x_var)))),
      data = d,
      span = 0.2
    )
    loessArgs <- utils::modifyList(loessArgs, loess...)

    l <- do.call(stats::loess, loessArgs)
    d[[i %_% loess_suffix]] <- stats::predict(l, newdata = d[[x_var]]) # Use 'newdata' here to include NAs.

    ## Create an estimated derivative variable using 'npregfast::frfast()'.
    if (interpolated_derivative)
      gam_data <- dataframe(d[, x_var, drop = FALSE], y = d[[i %_% interpolated_suffix]]) %>% tidyr::drop_na() # Complete cases only
    else
      gam_data <- dataframe(x[, x_var, drop = FALSE], y = x[[i]]) %>% tidyr::drop_na() # Complete cases only

    temp <- if (!is.null(pad_by)) seq_vals else x[[x_var]]
    kbin <- sum(temp >= min(gam_data[[x_var]], na.rm = TRUE) & temp <= max(gam_data[[x_var]], na.rm = TRUE))

    frfastArgs <- list(
      formula = eval(substitute(y ~ s(x_var, k = knots), list(x_var = as.name(x_var), knots = ifelse(NROW(gam_data) < 10, NROW(gam_data), 10)))),
      data = gam_data,
      #h0 = -1,
      kbin = kbin,
      nboot = 100,
      smooth = "splines",
      seed = 666,
      cluster = FALSE,
      ncores = NULL
    )
    frfastArgs <- utils::modifyList(frfastArgs, frfast...)

    if (frfastArgs$smooth != "splines")
      frfastArgs$formula <- eval(substitute(y ~ x_var, list(x_var = as.name(x_var))))

    if (!is.null(deriv)) local({
      deriv <- sort(deriv)

      z <- do.call(npregfast::frfast, frfastArgs)
      zz <- dataframe(z$x, z$p[, 1, 1], z$pl[, deriv + 1, 1], z$p[, deriv + 1, 1], z$pu[, deriv + 1, 1])
      colnames(zz) <- c(x_var, i, unlist(sapply(c(lower_ci_suffix, "", upper_ci_suffix), function(a) paste0(i %_% sapply(deriv, sprintf, fmt = deriv_suffix_template), a), simplify = FALSE)))

      # interpNAArgs$x <- zz
      # interpNAArgs <- utils::modifyList(interpNAArgs, interpNA...)
      # zz <- drop(do.call(interpNA, interpNAArgs))

      zzz <- zz
      if (plot_series_friendly) {
        ## Make a data set compatible with 'plinth::plot_series()'.
        dv <- sapply(deriv, sprintf, fmt = deriv_suffix_template)
        ll <- lower_ci_suffix
        ul <- upper_ci_suffix

        reCis <- "(" %_% paste(Hmisc::escapeRegex(trimws(c(dv %_% ll, dv %_% ul))), collapse = "|") %_% ")"
        ciCols <- grep(reCis, colnames(zz), value = TRUE)
        ciVars <- sapply(c(ll, ul), function(a) paste0(i %_% dv, a), simplify = FALSE)
        ciPairs <- chunk(combine_groups(list(i %_% dv, c(ll, ul)), sep = ""), 2)

        uncVars <- i %_% dv %_% "_uncertainty"
        unc <- dplyr::bind_cols(mapply(uncVars, ciPairs,
          FUN = function(a, b)
          {
            zz %>% dplyr::select(b) %>% dplyr::mutate(!! a := .[[b[2]]] - .[[b[1]]])
          }, SIMPLIFY = FALSE))
        zzz <- dplyr::bind_cols(list(dplyr::select(zz, c(x_var, i, i %_% dv)), unc))
      }

      attr(d, "frfast")[[i]] <<- zzz
    })

    if (!keep_interpolated)
      d[[i %_% interpolated_suffix]] <- NULL

    if (verbose) { cat("Done.", fill = TRUE); flush.console() }
  }

  plyr::l_ply(names(attr(d, "frfast")),
    function(a)
    {
      attr(d, "frfast")[[a]] <<- dplyr::arrange(attr(d, "frfast")[[a]], !! rlang::sym(c(x_var)))
    })

  d
}
