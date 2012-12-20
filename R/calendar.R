#' Create a calendar heat map with a set number of breaks
#'
#' This function creates a calendar heat map with custom break values,
#' allowing for comparisions between multiple time series.
#'
#' @param dates Vector of dates.
#' @param values Numeric vector of values per day.
#' @param breaks Vector specifying values to breaks colors at (optional).
#' @param ncolors Number of colors to use.
#' @param pal Palette from RColorBrewer
#' @param varname Name of variable for plot title.
#' @param date_form Date format. Defaults to "\%Y-\%m-\%d"
#'
#' @export
#'
#' @examples
#' date <- seq(from=as.Date("2010-01-01"),
#'              to=as.Date("2012-12-31"),
#'             by='day')
#' value <- rnorm(length(date), mean = 10, sd=1)
#' library("hiR")
#' calendar(dates=date, values=value)

calendar <- function(dates,
                    values,
                    breaks,
                    ncolors = 9,
                    pal = "Spectral",
                    varname = "Values",
                    date_form = "%Y-%m-%d"
                    ) {

####################################################################
####################################################################
####################################################################
####################################################################

levelplot.forumula <- function (x, data = NULL, allow.multiple = is.null(groups) ||
    outer, outer = TRUE, aspect = "fill", panel = if (useRaster) lattice.getOption("panel.levelplot.raster") else lattice.getOption("panel.levelplot"),
    prepanel = NULL, scales = list(), strip = TRUE, groups = NULL,
    xlab, xlim, ylab, ylim, at, cuts = 15, pretty = TRUE, region = TRUE,
    drop.unused.levels = lattice.getOption("drop.unused.levels"),
    ..., useRaster = FALSE, lattice.options = NULL, default.scales = list(),
    default.prepanel = lattice.getOption("prepanel.default.levelplot"),
    colorkey = region, col.regions, alpha.regions, subset = TRUE)
{
    formula <- x
    dots <- list(...)
    groups <- eval(substitute(groups), data, environment(formula))
    subset <- eval(substitute(subset), data, environment(formula))
    if (!is.null(lattice.options)) {
        oopt <- lattice.options(lattice.options)
        on.exit(lattice.options(oopt), add = TRUE)
    }
    form <- latticeParseFormula(formula, data, dimension = 3,
        subset = subset, groups = groups, multiple = allow.multiple,
        outer = outer, subscripts = TRUE, drop = drop.unused.levels)
    if (!is.null(form$groups))
        groups <- if (is.matrix(form$groups))
            as.vector(form$groups)[form$subscr]
        else if (is.data.frame(form$groups))
            as.vector(as.matrix(form$groups))[form$subscr]
        else form$groups[form$subscr]
    subscr <- seq_len(length(form$left))
    cond <- form$condition
    z <- form$left
    x <- form$right.x
    y <- form$right.y
    if (useRaster) {
        devRaster <- dev.capabilities("rasterImage")$rasterImage
        if (is.na(devRaster)) {
            warning("device support for raster images unknown, ignoring 'raster=TRUE'")
            useRaster <- FALSE
        }
        else if (devRaster == "no") {
            warning("device has no raster support, ignoring 'raster=TRUE'")
            useRaster <- FALSE
        }
        else if (devRaster == "non-missing" && any(is.na(z))) {
            warning("device does not support raster images with NA, ignoring 'raster=TRUE'")
            useRaster <- FALSE
        }
    }
    if (!is.function(panel))
        panel <- eval(panel)
    if (!is.function(strip))
        strip <- eval(strip)
    if (length(cond) == 0) {
        strip <- FALSE
        cond <- list(gl(1, length(x)))
    }
    if (missing(xlab))
        xlab <- form$right.x.name
    if (missing(ylab))
        ylab <- form$right.y.name

    zrng <- extend.limits(range(as.numeric(z), finite = TRUE))
    if (missing(at))
        at <- if (pretty)
            seq(zrng[1], zrng[2], length.out = cuts + 2)

    foo <- do.call("trellis.skeleton", c(list(formula = formula,
        cond = cond, aspect = aspect, strip = strip, panel = panel,
        xlab = xlab, ylab = ylab, xlab.default = form$right.x.name,
        ylab.default = form$right.y.name, lattice.options = lattice.options),
        dots))
    dots <- foo$dots
    foo <- foo$foo
    foo$call <- sys.call(sys.parent())
    foo$call[[1]] <- quote(levelplot)
    if (is.character(scales))
        scales <- list(relation = scales)
    scales <- updateList(default.scales, scales)
    foo <- c(foo, do.call("construct.scales", scales))
    have.xlim <- !missing(xlim)
    if (!is.null(foo$x.scales$limits)) {
        have.xlim <- TRUE
        xlim <- foo$x.scales$limits
    }
    have.ylim <- !missing(ylim)
    if (!is.null(foo$y.scales$limits)) {
        have.ylim <- TRUE
        ylim <- foo$y.scales$limits
    }
    have.xlog <- !is.logical(foo$x.scales$log) || foo$x.scales$log
    have.ylog <- !is.logical(foo$y.scales$log) || foo$y.scales$log
    if (have.xlog) {
        xlog <- foo$x.scales$log
        xbase <- if (is.logical(xlog))
            10
        else if (is.numeric(xlog))
            xlog
        else if (xlog == "e")
            exp(1)
        x <- log(x, xbase)
        if (have.xlim)
            xlim <- logLimits(xlim, xbase)
    }
    if (have.ylog) {
        ylog <- foo$y.scales$log
        ybase <- if (is.logical(ylog))
            10
        else if (is.numeric(ylog))
            ylog
        else if (ylog == "e")
            exp(1)
        y <- log(y, ybase)
        if (have.ylim)
            ylim <- logLimits(ylim, ybase)
    }
    cond.max.level <- unlist(lapply(cond, nlevels))
    if (is.logical(colorkey)) {
        if (colorkey) {
            colorkey <- list(at = at, space = "right")
            if (useRaster)
                colorkey$raster <- TRUE
            if (!missing(col.regions))
                colorkey$col <- col.regions
            if (!missing(alpha.regions))
                colorkey$alpha <- alpha.regions
        }
        else colorkey <- NULL
    }
    else if (is.list(colorkey)) {
        tmp <- list(space = if (any(c("x", "y", "corner") %in%
            names(colorkey))) "inside" else "right", at = at)
        if (!missing(col.regions))
            tmp$col <- col.regions
        if (!missing(alpha.regions))
            tmp$alpha <- alpha.regions
        if (useRaster)
            tmp$raster <- TRUE
        colorkey <- updateList(tmp, colorkey)
    }
    foo$legend <- construct.legend(foo$legend, colorkey, fun = "draw.colorkey")
    foo$panel.args.common <- c(list(x = x, y = y, z = z, at = at,
        region = region), dots)
    if (!missing(col.regions))
        foo$panel.args.common$col.regions <- col.regions
    if (!missing(alpha.regions))
        foo$panel.args.common$alpha.regions <- alpha.regions
    if (!is.null(groups))
        foo$panel.args.common$groups <- groups
    npackets <- prod(cond.max.level)
    if (npackets != prod(sapply(foo$condlevels, length)))
        stop("mismatch in number of packets")
    foo$panel.args <- vector(mode = "list", length = npackets)
    foo$packet.sizes <- numeric(npackets)
    if (npackets > 1) {
        dim(foo$packet.sizes) <- sapply(foo$condlevels, length)
        dimnames(foo$packet.sizes) <- lapply(foo$condlevels,
            as.character)
    }
    cond.current.level <- rep(1, length(cond))
    for (packet.number in seq_len(npackets)) {
        id <- compute.packet(cond, cond.current.level)
        foo$packet.sizes[packet.number] <- sum(id)
        foo$panel.args[[packet.number]] <- list(subscripts = subscr[id])
        cond.current.level <- cupdate(cond.current.level, cond.max.level)
    }
    more.comp <- c(limits.and.aspect(default.prepanel, prepanel = prepanel,
        have.xlim = have.xlim, xlim = xlim, have.ylim = have.ylim,
        ylim = ylim, x.relation = foo$x.scales$relation, y.relation = foo$y.scales$relation,
        panel.args.common = foo$panel.args.common, panel.args = foo$panel.args,
        aspect = aspect, npackets = npackets, x.axs = foo$x.scales$axs,
        y.axs = foo$y.scales$axs), cond.orders(foo))
    foo[names(more.comp)] <- more.comp
    class(foo) <- "trellis"
    foo
}

####################################################################
####################################################################
####################################################################
####################################################################
    require(lattice)
    require(grid)
    require(chron)
    if (class(dates) == "character" | class(dates) == "factor") {
        dates <- strptime(dates, date_form)
    }
    caldat <- data.frame(value = values, dates = dates)
    min.date <- as.Date(paste(format(min(dates), "%Y"), "-1-1",
        sep = ""))
    max.date <- as.Date(paste(format(max(dates), "%Y"), "-12-31",
        sep = ""))
    dates.f <- data.frame(date.seq = seq(min.date, max.date,
        by = "days"))
    caldat <- data.frame(date.seq = seq(min.date, max.date, by = "days"),
        value = NA)
    dates <- as.Date(dates)
    caldat$value[match(dates, caldat$date.seq)] <- values
    caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
    caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) +
        1
    caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
    caldat$month <- as.numeric(format(caldat$date.seq, "%m"))
    yrs <- as.character(unique(caldat$yr))
    d.loc <- as.numeric()
    for (m in min(yrs):max(yrs)) {
        d.subset <- which(caldat$yr == m)
        sub.seq <- seq(1, length(d.subset))
        d.loc <- c(d.loc, sub.seq)
    }
    caldat <- cbind(caldat, seq = d.loc)

    #INCLUDE R COLOR BREWER PALETTE FUNCTIONALITY

 Blues<-c("#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1","#6BAED6","#4292C6","#2171B5","#08519C","#08519C","#08306B")
 BuGn<-c("#F7FCFD","#E5F5F9","#CCECE6","#99D8C9","#66C2A4","#41AE76","#238B45","#006D2C","#006D2C","#00441B")
 BuPu<-c("#F7FCFD","#E0ECF4","#BFD3E6","#9EBCDA","#8C96C6","#8C6BB1","#88419D","#810F7C","#810F7C","#4D004B")
 GnBu<-c("#F7FCF0","#E0F3DB","#CCEBC5","#A8DDB5","#7BCCC4","#4EB3D3","#2B8CBE","#0868AC","#0868AC","#084081")
 Greens<-c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#006D2C","#00441B")
 Greys<-c("#FFFFFF","#F0F0F0","#D9D9D9","#BDBDBD","#969696","#737373","#525252","#252525","#252525","#000000")
 Oranges<-c("#FFF5EB","#FEE6CE","#FDD0A2","#FDAE6B","#FD8D3C","#F16913","#D94801","#A63603","#A63603","#7F2704")
 OrRd<-c("#FFF7EC","#FEE8C8","#FDD49E","#FDBB84","#FC8D59","#EF6548","#D7301F","#B30000","#B30000","#7F0000")
 PuBu<-c("#FFF7FB","#ECE7F2","#D0D1E6","#A6BDDB","#74A9CF","#3690C0","#0570B0","#045A8D","#045A8D","#023858")
 PuBuGn<-c("#FFF7FB","#ECE2F0","#D0D1E6","#A6BDDB","#67A9CF","#3690C0","#02818A","#016C59","#016C59","#014636")
 PuRd<-c("#F7F4F9","#E7E1EF","#D4B9DA","#C994C7","#DF65B0","#E7298A","#CE1256","#980043","#980043","#67001F")
 Purples<-c("#FCFBFD","#EFEDF5","#DADAEB","#BCBDDC","#9E9AC8","#807DBA","#6A51A3","#54278F","#54278F","#3F007D")
 RdPu<-c("#FFF7F3","#FDE0DD","#FCC5C0","#FA9FB5","#F768A1","#DD3497","#AE017E","#7A0177","#7A0177","#49006A")
 Reds<-c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#A50F15","#67000D")
 YlGn<-c("#FFFFE5","#F7FCB9","#D9F0A3","#ADDD8E","#78C679","#41AB5D","#238443","#006837","#006837","#004529")
 YlGnBu<-c("#FFFFD9","#EDF8B1","#C7E9B4","#7FCDBB","#41B6C4","#1D91C0","#225EA8","#253494","#253494","#081D58")
 YlOrBr<-c("#FFFFE5","#FFF7BC","#FEE391","#FEC44F","#FE9929","#EC7014","#CC4C02","#993404","#993404","#662506")
 YlOrRd<-c("#FFFFCC","#FFEDA0","#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#BD0026","#BD0026","#800026")
 BrBG<-c("#003C30","#01665E","#35978F","#80CDC1","#C7EAE5","#F5F5F5",
         "#F6E8C3","#DFC27D","#DFC27D","#BF812D","#8C510A","#543005")
 PiYG<-c("#276419","#4D9221","#7FBC41","#B8E186","#E6F5D0","#F7F7F7","#FDE0EF",
        "#F1B6DA","#F1B6DA","#DE77AE","#C51B7D","#8E0152")
 PRGn<-c("#00441B","#1B7837","#5AAE61","#A6DBA0","#D9F0D3","#F7F7F7",
        "#E7D4E8","#C2A5CF","#C2A5CF","#9970AB","#762A83","#40004B")
 PuOr<-c("#2D004B","#542788","#8073AC","#B2ABD2","#D8DAEB","#F7F7F7","#FEE0B6",
        "#FDB863","#FDB863","#E08214","#B35806","#7F3B08")
 RdBu<-c("#053061","#2166AC","#4393C3","#92C5DE","#D1E5F0","#F7F7F7","#FDDBC7","#F4A582",
        "#F4A582","#D6604D","#B2182B","#67001F")
 RdGy<-c("#1A1A1A","#4D4D4D","#878787","#BABABA","#E0E0E0","#FFFFFF","#FDDBC7",
        "#F4A582","#F4A582","#D6604D","#B2182B","#67001F")
 RdYlBu<-c("#313695","#4575B4","#74ADD1","#ABD9E9","#E0F3F8","#FFFFBF","#FEE090","#FDAE61","#FDAE61",
            "#F46D43","#D73027","#A50026")
 RdYlGn<-c("#006837","#1A9850","#66BD63","#A6D96A","#D9EF8B","#FFFFBF","#FEE08B","#FDAE61",
            "#FDAE61","#F46D43","#D73027","#A50026")
 Spectral<-c("#5E4FA2","#3288BD","#66C2A5","#ABDDA4","#E6F598","#FFFFBF","#FEE08B",
            "#FDAE61","#FDAE61","#F46D43","#D53E4F","#9E0142")
 Accent<-c("#7FC97F","#BEAED4","#FDC086","#FFFF99","#386CB0","#F0027F","#BF5B17","#666666","#666666")
 Dark2<-c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#666666","#666666")
 Paired<-c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C",
            "#FDBF6F","#FF7F00","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99")
 Pastel1<-c("#FBB4AE","#B3CDE3","#CCEBC5","#DECBE4","#FED9A6","#FFFFCC","#E5D8BD","#FDDAEC","#FDDAEC","#F2F2F2")
 Pastel2<-c("#B3E2CD","#FDCDAC","#CBD5E8","#F4CAE4","#E6F5C9","#FFF2AE","#F1E2CC","#CCCCCC","#CCCCCC")
 Set1<-c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF","#F781BF","#999999")
 Set2<-c("#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3","#B3B3B3")
 Set3<-c ("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69",
        "#FCCDE5","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5")

    assign("col.sty", get(pal))
    calendar.pal <- colorRampPalette((col.sty), space = "Lab")
    def.theme <- lattice.getOption("default.theme")
    cal.theme <- function() {
        theme <- list(strip.background = list(col = "transparent"),
            strip.border = list(col = "transparent"), axis.line = list(col = "transparent"),
            par.strip.text = list(cex = 0.8))
    }
    lattice.options(default.theme = cal.theme)
    yrs <- (unique(caldat$yr))
    nyr <- length(yrs)

   ##############################################################
   # Tell Level Plot to use "breaks" defined in calendar plot to serve as the "at". #
   ##############################################################
    print(cal.plot <- levelplot(value ~ woty * dotw | yr, data = caldat,
        as.table = TRUE, aspect = .33, layout = c(1, nyr%%7),
        between = list(x = 0, y = c(1, 1)), strip = TRUE, main = paste("Calendar Heat Map of ",
            varname, sep = ""), scales = list(x = list(at = c(seq(2.9,
            52, by = 4.42)), labels = month.abb, alternating = c(1,
            rep(0, (nyr - 1))), tck = 0, cex = 0.7), y = list(at = c(0,
            1, 2, 3, 4, 5, 6), labels = c("Sunday", "Monday",
            "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
            alternating = 1, cex = 0.6, tck = 0)), xlim = c(0.4,
            54.6), ylim = c(6.6, -0.6),
            at=breaks,                       #HERE IS THE LINE OF CODE TO CHANGE
            cuts = ncolors-1 ,
            pretty=TRUE, col.regions = (calendar.pal(ncolors)),
            xlab = "", ylab = "", colorkey = list(col = calendar.pal(ncolors),
            width = 0.6, height = 0.5), subscripts = TRUE))
    panel.locs <- trellis.currentLayout()

    for (row in 1:nrow(panel.locs)) {
        for (column in 1:ncol(panel.locs)) {
            if (panel.locs[row, column] > 0) {
                trellis.focus("panel", row = row, column = column,
                  highlight = FALSE)
                xyetc <- trellis.panelArgs()
                subs <- caldat[xyetc$subscripts, ]
                dates.fsubs <- caldat[caldat$yr == unique(subs$yr),
                  ]
                y.start <- dates.fsubs$dotw[1]
                y.end <- dates.fsubs$dotw[nrow(dates.fsubs)]
                dates.len <- nrow(dates.fsubs)
                adj.start <- dates.fsubs$woty[1]
                for (k in 0:6) {
                  if (k < y.start) {
                    x.start <- adj.start + 0.5
                  }
                  else {
                    x.start <- adj.start - 0.5
                  }
                  if (k > y.end) {
                    x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] -
                      0.5
                  }
                  else {
                    x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] +
                      0.5
                  }
                  grid.lines(x = c(x.start, x.finis), y = c(k -
                    0.5, k - 0.5), default.units = "native",
                    gp = gpar(col = "grey", lwd = 1))
                }
                if (adj.start < 2) {
                  grid.lines(x = c(0.5, 0.5), y = c(6.5, y.start -
                    0.5), default.units = "native", gp = gpar(col = "grey",
                    lwd = 1))
                  grid.lines(x = c(1.5, 1.5), y = c(6.5, -0.5),
                    default.units = "native", gp = gpar(col = "grey",
                      lwd = 1))
                  grid.lines(x = c(x.finis, x.finis), y = c(dates.fsubs$dotw[dates.len] -
                    0.5, -0.5), default.units = "native", gp = gpar(col = "grey",
                    lwd = 1))
                  if (dates.fsubs$dotw[dates.len] != 6) {
                    grid.lines(x = c(x.finis + 1, x.finis + 1),
                      y = c(dates.fsubs$dotw[dates.len] - 0.5,
                        -0.5), default.units = "native", gp = gpar(col = "grey",
                        lwd = 1))
                  }
                  grid.lines(x = c(x.finis, x.finis), y = c(dates.fsubs$dotw[dates.len] -
                    0.5, -0.5), default.units = "native", gp = gpar(col = "grey",
                    lwd = 1))
                }
                for (n in 1:51) {
                  grid.lines(x = c(n + 1.5, n + 1.5), y = c(-0.5,
                    6.5), default.units = "native", gp = gpar(col = "grey",
                    lwd = 1))
                }
                x.start <- adj.start - 0.5
                if (y.start > 0) {
                  grid.lines(x = c(x.start, x.start + 1), y = c(y.start -
                    0.5, y.start - 0.5), default.units = "native",
                    gp = gpar(col = "grey50", lwd = 1.75))
                  grid.lines(x = c(x.start + 1, x.start + 1),
                    y = c(y.start - 0.5, -0.5), default.units = "native",
                    gp = gpar(col = "grey50", lwd = 1.75))
                  grid.lines(x = c(x.start, x.start), y = c(y.start -
                    0.5, 6.5), default.units = "native", gp = gpar(col = "grey50",
                    lwd = 1.75))
                  if (y.end < 6) {
                    grid.lines(x = c(x.start + 1, x.finis + 1),
                      y = c(-0.5, -0.5), default.units = "native",
                      gp = gpar(col = "grey50", lwd = 1.75))
                    grid.lines(x = c(x.start, x.finis), y = c(6.5,
                      6.5), default.units = "native", gp = gpar(col = "grey50",
                      lwd = 1.75))
                  }
                  else {
                    grid.lines(x = c(x.start + 1, x.finis), y = c(-0.5,
                      -0.5), default.units = "native", gp = gpar(col = "grey50",
                      lwd = 1.75))
                    grid.lines(x = c(x.start, x.finis), y = c(6.5,
                      6.5), default.units = "native", gp = gpar(col = "grey50",
                      lwd = 1.75))
                  }
                }
                else {
                  grid.lines(x = c(x.start, x.start), y = c(-0.5,
                    6.5), default.units = "native", gp = gpar(col = "grey50",
                    lwd = 1.75))
                }
                if (y.start == 0) {
                  if (y.end < 6) {
                    grid.lines(x = c(x.start, x.finis + 1), y = c(-0.5,
                      -0.5), default.units = "native", gp = gpar(col = "grey50",
                      lwd = 1.75))
                    grid.lines(x = c(x.start, x.finis), y = c(6.5,
                      6.5), default.units = "native", gp = gpar(col = "grey50",
                      lwd = 1.75))
                  }
                  else {
                    grid.lines(x = c(x.start + 1, x.finis), y = c(-0.5,
                      -0.5), default.units = "native", gp = gpar(col = "grey50",
                      lwd = 1.75))
                    grid.lines(x = c(x.start, x.finis), y = c(6.5,
                      6.5), default.units = "native", gp = gpar(col = "grey50",
                      lwd = 1.75))
                  }
                }
                for (j in 1:12) {
                  last.month <- max(dates.fsubs$seq[dates.fsubs$month ==
                    j])
                  x.last.m <- dates.fsubs$woty[last.month] +
                    0.5
                  y.last.m <- dates.fsubs$dotw[last.month] +
                    0.5
                  grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5,
                    y.last.m), default.units = "native", gp = gpar(col = "grey50",
                    lwd = 1.75))
                  if ((y.last.m) < 6) {
                    grid.lines(x = c(x.last.m, x.last.m - 1),
                      y = c(y.last.m, y.last.m), default.units = "native",
                      gp = gpar(col = "grey50", lwd = 1.75))
                    grid.lines(x = c(x.last.m - 1, x.last.m -
                      1), y = c(y.last.m, 6.5), default.units = "native",
                      gp = gpar(col = "grey50", lwd = 1.75))
                  }
                  else {
                    grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5,
                      6.5), default.units = "native", gp = gpar(col = "grey50",
                      lwd = 1.75))
                  }
                }
            }
        }
        trellis.unfocus()
    }
    lattice.options(default.theme = def.theme)
}