#' Create Pretty Coefficient Plots
#'
#' @param m A model from glm, lm, etc.
#' @param title The title for the plot
#' @param pval the p-value at which to color significant coefficients blue
#' @param null the value at which a coefficient is null (0 or 1, usually)

#' @return
#' A pretty coefficient plot
#'
#' @export
#'
#' @examples
#' y = rnorm(100)
#' x1 = rnorm(100)
#' x2 = rnorm(100)
#' x3 = rnorm(100)
#' x4 = rnorm(100)
#' m <- glm(y ~ x1 + x2 + x3 + x4)
#' pretty_coefs(m)

pretty_coefs <- function(m,
						title="",
						pval = 0.05,
						null=0
						)
{

	################ generate data ###############################

	# setup
	t.05 <- qt(1-0.025, 634, lower.tail = TRUE, log.p=FALSE)
	t.5 <- qt(1-0.25, 634, lower.tail = TRUE, log.p=FALSE)
	t.75 <- qt(1-0.5, 634, lower.tail = TRUE, log.p=FALSE)
	N <- length(m$coefficients) - 1
	coef_quantil <- data.frame(matrix(0, nrow=N, ncol=11))
	names(coef_quantil) <- c("coef", "mean", "sd",
							 "X2.5", "X25", "X37.5",
							 "X50", "X62.5", "X75",
							 "X97.5", "pval")

	# calculate coefficient quantils
	for(i in 1:N) {
	    coefnames <- row.names(summary(m)$coefficients)[i+1]
	    coef_quantil[i,1] <- coefnames
	    coef_quantil[i,c(2,3,7)] <- summary(m)$coefficients[i+1,c(1,2,1)]
	    coef_quantil[i, c(4,10)] <- coef_quantil[i,2]+c(-1, 1)*t.05*coef_quantil[i,3]
	    coef_quantil[i, c(5,9)] <- coef_quantil[i,2]+c(-1, 1)*t.5*coef_quantil[i,3]
	    coef_quantil[i, c(6,8)] <- coef_quantil[i,2]+c(-1, 1)*t.75*coef_quantil[i,3]
	    coef_quantil[i, 11] <- summary(m)$coefficients[i+1,4]
	}

	# add aic, significance binary, sory my coefficients
	aic <- round(m$aic, 0)
	coef_quantil$sig <-ifelse(coef_quantil$pval <= pval, 1, 0)
	coef_quantil <- coef_quantil[order(coef_quantil$mean, decreasing=TRUE),]


	################ plot coefficients ############################

	# set colors
	dark_blue = "#045168"
	blue = "#078ab2"
	light_blue = "#85ccc2"
	blues = c(dark_blue, blue, light_blue)

	dark_red = "#681b04"
	red = "#812105"
	light_red = "#b54f5d"
	reds = c(dark_red, red, light_red)

	# algorithmically pad x and y
	pad_range <- function(min, max, f=0.2) {
		range <- max - min
		min <- min - range*f
		max <- max + range*f
		c(min, max)
	}
	coef_min <- min(coef_quantil[,4])
	coef_max <- max(coef_quantil[,8])
	xlim <- pretty(pad_range(coef_min, coef_max))

	# manually pad y
	ypad <- N*0.075
	ylim <- c(0 - ypad*2, N + ypad*2)

	# graphical parameters
	par(family="HersheySans",  #specify the font to use
		bg="white", #specify the background color
	 	fg="grey50", #specify the color of the "foreground" - axes, labels, etc.
	 	ljoin=0, #make the axes have rounded edges
	 	col.main="grey30", # specify the color of the title
	 	col.sub="grey30",# specify the color of the subtitle
	 	mai=c(0.5,0,0.5,0),
		cex=0.5,
		cex.main=1.5,
		bg="white")

	# plot skeleton
	plot(0,0,
	    xlim= c(xlim[1], xlim[length(xlim)]),
	    ylim=ylim,
	    type="n",
	    axes=FALSE,
	    ylab="",
	    xlab="")

	# title
	title(title, sub=paste("AIC:", aic), line=0, cex=0.75, outer=FALSE)

    # ticks and text
    # generate x range
    range <- pretty(seq(xlim[2],xlim[length(xlim)-1], 0.25))
    n_range <- length(range)

    # plot x axis
    for (i in 1:n_range){
      # bottom ticks and text:
        text(range[i], 0, range[i], pos=1, cex=1.1)
  	  #top ticks and text:
  		text(range[i], N, range[i], pos=3, cex=1.1)
      # gridlines:
        segments(range[i], 0, range[i], N, lwd=0.5, lty=2, col="grey75")
    }

    # add null line
    segments(null, null, 0, N, lwd=1.5, lty=1, col="grey75")

    # draw coefs and CIs
    for (i in 1:N) {

		# color by significance
		if (coef_quantil$sig[i]==1) {
			cols = blues
		  } else{
			cols = reds
		}

		# allign coefficients
	    j <- i-ypad

	    # 95% CI
	    lines(c(coef_quantil$X2.5[i],
	            coef_quantil$X97.5[i]),
	            rep(j, 2),
	            lwd=2,
	            col=alpha(cols[3], 0.95))

	    # 50% CI
	    lines(c(coef_quantil$X25[i],
	            coef_quantil$X75[i]),
	            rep(j,2),
	            col=alpha(cols[2], 0.6),
	            lwd=4)

	    # 25% CI
	    lines(c(coef_quantil$X37.5[i],
	            coef_quantil$X62.5[i]),
	            rep(j,2),
	            col=alpha(cols[2], 0.6),
	            lwd=6)

	    # coef
	    z <- 3
	    radius <- sqrt(z)/pi
	    symbols(coef_quantil$mean[i], j,
	    		circles=radius,
	    		inches=0.04,
	            bg=alpha(cols[1], 0.5),
	            fg="white",
	            add=TRUE)

	    # label
	    text(coef_quantil$X2.5[i], j,
	    		labels=paste0(coef_quantil$coef[i], "  "),
	            adj=1,
	            cex=1.05,
	            col=cols[2])
	}
}