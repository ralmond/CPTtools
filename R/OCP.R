#####################################
## Observable Characteristic Plot

"OCP" <-
function (x,n,labs,pi, pilab=names(pi),
          groupnames = names(x), a=.5, b=.5,
          reflty=1,..., newplot=TRUE, main= NULL, sub= NULL,
          xlab="Groups", ylab = "Probability", cex=par("cex"),
          xlim=NULL, ylim=NULL, cex.axis=par("cex.axis")) {
  groups <- 1:length(x)
  if (is.null(groupnames)) {
    groupnames <- paste(groups)
  }
  if (is.null(pilab)) {
    pilab <- paste(along(pi))
  }
  ci <- betaci(x,n,a=a,b=b)
  means <- (x+a)/(n+a+b)
  ## Set up plot window.
  if (newplot) {
    xlim <- c(0,length(x)+1)
    ylim <- c(min(ci$lower,ci$upper),max(ci$lower,ci$upper))
    par(mar=c(5,4,4,4))
    plot.new()
    plot.window(xlim, ylim, log = "", ...,bty="o")
    box()
    title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  }
  text(groups,means,labs,cex=2*cex)
  segments(groups,ci$lower,groups,ci$upper)
  abline(h=pi,lty=reflty)
  axis(2)
  axis(1,groups,groupnames,cex.axis=cex.axis)
  axis(4,pi,pilab,cex.axis=2*cex,las=1)
  invisible(ci)
}


## nn <- c(30,15,20,35)
## pi <- c("+"=.15,"-"=.85)
## grouplabs <- c(rep("-",3),"+")
## x <- c("(0,0)"=7,"(0,1)"=4,"(1,0)"=2,"(1,1)"=31)
## OCP (x,nn,grouplabs,pi,c("-","+"),ylim=c(0,1), reflty=2:3)
