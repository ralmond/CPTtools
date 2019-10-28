### StackedBars.R
### $Revision: 1.4 $
### $Date: 2010/02/03 21:18:16 $
### Authors:  Russell Almond

colorspread <- function(col,steps,maxsat=FALSE,rampval=FALSE) {
  hsvmat <- rgb2hsv(col2rgb(rep(col,steps)))
  if (maxsat) {
    hsvmat["s",] <- 1
  }
  hsvmat["s",] <- hsvmat["s",]*(1:steps)/steps
  if (rampval) {
    hsvmat["v",] <- hsvmat["v",]*(steps:1)/steps
  }
  hsv(hsvmat["h",],hsvmat["s",],hsvmat["v",])
}


"stackedBarplot" <-
function (height, width = 1, space = 0.2, offset = 0, names.arg = NULL,
    legend.text = NULL, horiz = FALSE, density = NULL,
    angle = 45, col = NULL, border = par("fg"), main = NULL,
    sub = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,
    xpd = TRUE, axis=TRUE, axisnames = TRUE, cex.axis = par("cex.axis"),
    cex.names = par("cex.axis"), newplot = TRUE,
    axis.lty = 0, ...)
{
    space <- space * mean(width)
    if (axisnames && missing(names.arg))
        names.arg <- if (is.matrix(height))
            colnames(height)
        else names(height)
    if (is.vector(height) || (is.array(height) && (length(dim(height)) ==
        1))) {
        height <- cbind(height)
        if (is.null(col))
            col <- "grey"
    }
    else if (is.matrix(height)) {
        if (is.null(col))
            col <- grey.colors(nrow(height))
    }
    else stop("'height' must be a vector or a matrix")
    if (is.logical(legend.text))
        legend.text <- if (legend.text && is.matrix(height))
            rownames(height)
    NR <- nrow(height)
    NC <- ncol(height)
    {
        width <- rep(width, length.out = NC)
        height <- rbind(0, apply(height, 2, cumsum))
    }
    offset <- rep(as.vector(offset), length.out = length(width))
    delta <- width/2
    w.r <- cumsum(space + width)
    w.m <- w.r - delta
    w.l <- w.m - delta
    if (horiz) {
        if (missing(xlim))
            xlim <- range(-0.01 * height + offset, height + offset,
                na.rm = TRUE)
        if (missing(ylim))
            ylim <- c(min(w.l), max(w.r))
    }
    else {
        if (missing(xlim))
            xlim <- c(min(w.l), max(w.r))
        if (missing(ylim))
            ylim <- range(-0.01 * height + offset, height + offset,
                na.rm = TRUE)
    }
    opar <- if (horiz)
      par(xaxs = "i", xpd = xpd)
    else par(yaxs = "i", xpd = xpd)
    on.exit(par(opar))
    if (newplot) {
      plot.new()
      plot.window(xlim, ylim, log = "", ...)
    }
    xyrect <- function(x1, y1, x2, y2, horizontal = TRUE, ...) {
      if (horizontal)
        rect(x1, y1, x2, y2, ...)
      else rect(y1, x1, y2, x2, ...)
    }
    for (i in 1:NC) {
      xyrect(height[1:NR, i] + offset[i], w.l[i],
             height[-1, i] + offset[i], w.r[i], horizontal = horiz,
             angle = angle, density = density, col = col,
             border = border)
    }
    if (axisnames && !is.null(names.arg)) {
      if (length(names.arg) != length(w.m)) {
        if (length(names.arg) == NC)
          at.l <- colMeans(w.m)
        else stop("incorrect number of names")
      }
      else at.l <- w.m
      axis(if (horiz) 2 else 1, at = at.l, labels = names.arg, lty = axis.lty,
           cex.axis = cex.names, ...)
    }
    if (!is.null(legend.text)) {
      legend.col <- rep(col, length.out = length(legend.text))
      if (!horiz) {
        legend.text <- rev(legend.text)
        legend.col <- rev(legend.col)
        density <- rev(density)
        angle <- rev(angle)
      }
      xy <- par("usr")
      legend(xy[2] - xinch(0.1), xy[4] - yinch(0.1), legend = legend.text,
             angle = angle, density = density, fill = legend.col,
             xjust = 1, yjust = 1)
    }
    if (newplot)
      title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
    if (axis)
      axis(if (horiz) 1 else 2, cex.axis = cex.axis, ...)
    invisible(w.m)
}


### This graph produces Jodi Underwood's stacked bar breakdown charts.
### Data is assumed to be a matrix with useful row and column labels.
### data --- Data matrix (rows are levels, columns are groups)
### digits --- number of significant digits to use when printing labels
### Percent --- report scores as percents rather than fractions
"stackedBars" <-
function (data, profindex, ..., ylim = c(min(offsets)-.25,max(1+offsets)),
                         cex.names=par("cex.axis"),
                         percent=TRUE, digits=2*(1-percent),labrot=FALSE) {
  data <- as.matrix(data)
  ngroups <- ncol(data)
  nlevels <- nrow(data)
  normdata <- sweep(data,2,apply(data,2,sum),"/")
  if (profindex == 1) {
    offsets <- - normdata[1,]
  } else {
    offsets <- -apply(normdata[1:profindex,],2,sum)
  }

  xmid <- barplot(normdata,offset=offsets, axes=FALSE,cex.names=cex.names,
                  ylim = ylim,
                  ## xlim=c(0,ngroups+2.5),width=.75,legend.text=TRUE,
                  ...)
  xmid <- matrix(rep(xmid,each=nlevels),dim(data))
  ## Vertical Line
  lines(c(0,ngroups*1.25),c(0,0))
  ymid <- sweep(apply(normdata,2,cumsum),2,offsets,"+") - normdata/2
  labels <- paste(round(data,digits))
  if (percent)
    labels <- paste(round(normdata*100,digits))
  srt <- par("srt")
  if (labrot) srt <- srt+90
  text(xmid[data!=0],ymid[data!=0],labels[data!=0],cex=cex.names,srt=srt)
}



### This is a variant on the stacked bars graph meant to show
### prior/posterior comparisons.  It assumes the pairs come in
### groups and are labeled "skill.prior" "skill.post"
"compareBars" <-
function (data1, data2, profindex,
          groupNames=c(deparse(data1),deparse(data2)),...,
          ylim = c(min(offsets)-.25,max(1+offsets)),
          cex.names=par("cex.axis"), digits=2,
          legend.loc=c(0,1),legend.cex=par("cex"),
          col=par("col"), col1= NULL, col2=NULL,
          main=NULL, sub=NULL, xlab=NULL, ylab=NULL,rotlab=FALSE){
  if (missing(col1) && !missing(col))
    col1 <- col
  if (missing(col1) && !missing(col))
    col2 <- col
  data1 <- as.matrix(data1)
  data2 <- as.matrix(data2)
  npair <- ncol(data1)
  gaps <- rep(c(.3,.1),npair)
  gaps1 <- rep(1.4,npair)
  gaps1[1] <- .3
  gaps2 <- rep(1.4,npair)
  data <- cbind(data1[,1],data2[,1])
  for (icol in 2:npair)
    data <- cbind(data,data1[,icol],data2[,icol])
  if (profindex == 1) {
    offsets <- - data[1,]
    offsets1 <- - data1[1,]
    offsets2 <- - data2[1,]
  } else {
    offsets <- -apply(data[1:profindex,],2,sum)
    offsets1 <- -apply(data1[1:profindex,],2,sum)
    offsets2 <- -apply(data2[1:profindex,],2,sum)
  }
  xlim <- c(0,npair*2.4)
  # Calculate positions
  xpos <- barplot(data,space=gaps,offset=offsets,
                  width=1, cex.names=cex.names, xlim=xlim, ylim=ylim,
                  plot=FALSE, ...)
  plot.new()
  plot.window(xlim=xlim, ylim=ylim,log="", ...)
  title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  names <- rep("",npair)
  xpos1 <- stackedBarplot(data1,space=gaps1,offset=offsets1,
                         width=1, cex.names=cex.names, col=col1,
                         newplot=FALSE, axis=FALSE,names.arg=names,...)
  xpos2 <- stackedBarplot(data2,space=gaps2,offset=offsets2,
                         width=1, cex.names=cex.names, col=col2,
                         newplot=FALSE, axis=FALSE,names.arg=names,...)
  abline(h=0)
  xposave <- (xpos[(1:npair)*2-1] + xpos[(1:npair)*2])/2
  yl <- ylim[2]-ylim[1]
  srt <- par("srt")
  if (rotlab) srt <- srt + 90
#  text(xpos,min(offsets)-.05*yl,rep(groupNames,npair),cex=cex.names,xpd=NA,srt=srt)
  text(xposave,min(offsets)-.1*yl,dimnames(data1)[[2]],cex=cex.names,xpd=NA,srt=srt)
  xpos <- rep(xpos,each=nrow(data))
  ypos <- sweep(apply(data,2,cumsum),2,offsets,"+") - data/2
  labels <- paste(round(data,digits))
  text(xpos[data!=0],ypos[data!=0],labels[data!=0],cex=cex.names,srt=srt)
  ### Legend
  labels <- as.vector(t(outer(groupNames,dimnames(data1)[[1]],paste,sep=":")))
  legend(legend.loc,legend=labels,fill=c(col1,col2), cex=legend.cex)
  invisible(xpos)
}

### This is a variant on the stacked bars graph meant to show
### prior/posterior comparisons.  It assumes the pairs come in
### groups and are labeled "skill.prior" "skill.post"
"compareBars2" <-
function (data1, data2, profindex,groupNames=c("Prior","Post"),
          error.bars=2,scale=100,err.col="gray20",...,
          ylim = NULL) {
  if (any(error.bars <1) || any(error.bars>2) ||
      abs(error.bars - round(error.bars)) > .00001 ) {
    stop("Expected error.bars to be 1 or 2.")
  }
  data1 <- as.matrix(data1)
  data2 <- as.matrix(data2)

  npair <- ncol(data1)
  gaps <- rep(c(.3,.1),npair)
  data <- cbind(data1[,1],data2[,1])
  for (col in 2:npair)
    data <- cbind(data,data1[,col],data2[,col])
  ss <- matrix(colSums(data),nrow(data),ncol(data),byrow=TRUE)
  ## Set up list of which error bars to show.
  nbars <- nrow(data)
  whichbars <- 1:(nbars*npair*2)
  if (length(error.bars) < 2) {
    whichbars <- rep(((1:npair)*2-3+error.bars)*nbars,
                     each=nbars)+1:nbars
  }
  if (profindex == 1) {
    offsets <- -data[profindex,]
  } else {
    offsets <- -apply(data[1:profindex,],2,sum)
  }
  ci <- proflevelci(data,profindex)
  ## Rescale data
  ci.ll <- ci$lower*scale
  ci.ul <- ci$upper*scale
  data <- data/ss*scale
  offsets <- offsets/ss[1,]*scale
  data1 <- sweep(data1,2,colSums(data1),"/")*scale
  data2 <- sweep(data2,2,colSums(data2),"/")*scale
  ## Barplot
  if (!is.numeric(ylim)) {
    ylim <- c(min(data-offsets,ci.ll[whichbars],ci.ul[whichbars])-scale/10,
              max(data-offsets,ci.ll[whichbars],ci.ul[whichbars])+scale/10)
  }
  xpos <- compareBars(data1,data2,profindex,groupNames, ylim=ylim,...)
  xpos <- matrix(xpos,nrow=nbars)
  ## Now add error bar segments
  ## Add a small offset to each error bar
  xpos <- xpos + (1:nbars)/(nbars+1) -.5
  ## Now for the segments
  segments(xpos[whichbars],ci.ll[whichbars],
           xpos[whichbars],ci.ul[whichbars],
           col=err.col, lwd=1.5)
  invisible(list(xpos=xpos,ci.ul=ci.ul,ci.ll=ci.ll))
}



## ### Test Graphs
## #postscript("~/text/david/likelihood.eps",onefile=FALSE,horizontal=FALSE,
## #           paper="special",width=5,height=3)
## compareBars(likelihood,3,main="Likelihoods for Medium TroubleShooting Task",
##             sub="Observables:  cfgCor=Medium, logCor=High, logEff=Medium",
##             cex.names=.75,
##             names.arg=c("Troubleshoot","","NDK","","Modeling",""))
## #dev.off()
## #postscript("~/text/david/margins.eps",onefile=FALSE,horizontal=FALSE,
## #           paper="special",width=5,height=3)
## compareBars(margins,3,main="Margins before/after Medium TroubleShooting Task",
##             sub="Observables:  cfgCor=Medium, logCor=High, logEff=Medium",
##             cex.names=.75,
##             names.arg=c("Troubleshoot","","NDK","","Modeling",""))

## #dev.off()
## #postscript("~/text/david/forcast.eps",onefile=FALSE,horizontal=FALSE,
## #           paper="special",width=5,height=3)
## compareBars(forcast,1,main="Predicted distributions for observables",
##             sub="Observables:  cfgCor=Medium, logCor=High, logEff=Medium",
##             cex.names=.75,
##             names.arg=c("cfg-Cor","","dgm-Cor","","flst-Cor","","log-Cor","","log-Eff",""))
## #dev.off()
## #postscript("~/text/david/CNAPscore.eps",onefile=FALSE,horizontal=FALSE,
##            paper="special",width=5,height=3)
## stackedBars(posterior,3,main="Sample score report",
##             sub="Third Semester Student", cex.names=.75)
## #dev.off()


barchart.CPF <- function(x, data=NULL, ..., baseCol="firebrick",
                         auto.key=TRUE,par.settings=list()) {
  nstates <- length(getTableStates(x))
  if (is.null(baseCol))
    ps <- par.settings
  else
    ps <- list(par.settings,
               superpose.polygon=list(col=rev(colorspread(baseCol,nstates))))
  if (nrow(x)==1L) { ## No Parent Case
    barchart(as.matrix(x),data,auto.key=auto.key, par.settings=ps,...)
  } else if (length(getTableParents(x))==1L) { ## One parent
    xx <- numericPart(x)
    rownames(xx) <- factorPart(x)
    barchart(xx,data,auto.key=auto.key, par.settings=ps,...)
  } else { ## Two or more parents
      xx <- as.CPA(x)
      dd <- dim(xx)
      nstates <- dd[length(dd)]
      for (ddd in 1L:(length(dd)-1L)) {
        dimnames(xx)[[ddd]] <- paste(names(dd)[ddd],"=",dimnames(xx)[[ddd]])
      }
      barchart(xx,data,auto.key=auto.key, par.settings=ps,...)
    }
}

#######
## Utility functions for checking to see if a series is increasing or
## decreasing.

isIncreasing <- function (vec) {
  if (length(vec) < 2L) return (NA)
  for (k in 2L:length(vec)) {
    if (vec[k-1L] >= vec[k]) return (FALSE)
  }
  return (TRUE)
}

isNondecreasing <- function (vec) {
  if (length(vec) < 2L) return (NA)
  for (k in 2L:length(vec)) {
    if (vec[k-1L] > vec[k]) return (FALSE)
  }
  return (TRUE)
}

isDecreasing <- function (vec) {
  if (length(vec) < 2L) return (NA)
  for (k in 2L:length(vec)) {
    if (vec[k-1L] <= vec[k]) return (FALSE)
  }
  return (TRUE)
}

isNonincreasing <- function (vec) {
  if (length(vec) < 2L) return (NA)
  for (k in 2L:length(vec)) {
    if (vec[k-1L] < vec[k]) return (FALSE)
  }
  return (TRUE)
}

isMonotonic <- function (vec, strict=TRUE) {
  if (length(vec) < 2L) return (NA)
  direction <- 0 ## 1 for increasing, -1 for decreasing
  result <- FALSE
  if (vec[1L] < vec[2L]) {
    direction <- 1                            #Increasing
    if (length(vec) == 2L) {
      result <- TRUE
    } else {
      ## Recurse
      if (strict) {
        result <- isIncreasing(vec[-1L])
      } else {
        result <- isNondecreasing(vec[-1L])
      }
    }

  } else if (vec[1L] > vec[2L]) {
    direction <- -1                            #Increasing
    if (length(vec) == 2L) {
      result <- TRUE
    } else {
      ## Recurse
      if (strict) {
        result <- isDecreasing(vec[-1L])
      } else {
        result <- isNonincreasing(vec[-1L])
      }
    }

  } else {
    ## Flat
    if (!strict) {
      if (length(vec)==2L) {
        result <- TRUE
      } else {
        result <- isMonotonic(vec[-1L],strict)
        direction <- attr(result,"direction")
      }
    }
  }
  attr(result,"direction") <- direction
  result
}
