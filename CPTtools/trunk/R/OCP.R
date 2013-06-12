#####################################
## Observable Characteristic Plot

"OCP" <-
function (x,n,lclabs,pi, pilab=names(pi),
          lcnames = names(x), a=.5, b=.5,
          reflty=1,..., newplot=TRUE, main= NULL, sub= NULL,
          xlab="Latent Classes", ylab = "Probability", cex=par("cex"),
          xlim=NULL, ylim=NULL, cex.axis=par("cex.axis")) {
  groups <- 1:length(x)
  if (is.null(lcnames)) {
    lcnames <- paste(groups)
  }
  if (is.null(pilab)) {
    pilab <- paste(seq_along(pi))
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
  text(groups,means,lclabs,cex=2*cex)
  segments(groups,ci$lower,groups,ci$upper)
  abline(h=pi,lty=reflty)
  axis(2)
  axis(1,groups,lcnames,cex.axis=cex.axis)
  axis(4,pi,pilab,cex.axis=2*cex,las=1)
  invisible(ci)
}


"OCP2" <-
function (x,n,lclabs,pi, pilab=names(pi),
          lcnames = names(x), set1=seq(1,length(x)-1,2),
          setlabs=c("Set1","Set2"), setat=-1, a=.5, b=.5,
          reflty=1,..., newplot=TRUE, main= NULL, sub= NULL,
          xlab="Latent Classes", ylab = "Probability", cex=par("cex"),
          xlim=NULL, ylim=NULL, cex.axis=par("cex.axis")) {
  groups <- 1:length(x)
  if (is.null(lcnames)) {
    lcnames <- paste(groups)
  }
  if (is.null(pilab)) {
    pilab <- paste(seq_along(pi))
  }
  ci <- betaci(x,n,a=a,b=b)
  means <- (x+a)/(n+a+b)
  ## Set up plot window.
  if (newplot) {
    xlim <- c(0,length(x)+1)
    ylim <- c(min(ci$lower,ci$upper),max(ci$lower,ci$upper))
    par(mar=c(6,4,4,4))
    plot.new()
    plot.window(xlim, ylim, log = "", ...,bty="o")
    box()
    title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  }
  text(groups,means,lclabs,cex=2*cex)
  segments(groups,ci$lower,groups,ci$upper)
  abline(h=pi,lty=reflty)
  axis(2)
  axis(1,groups[set1],lcnames[set1],cex.axis=cex.axis)
  mtext(setlabs[1],1,1,at=setat)
  axis(1,groups[-set1],lcnames[-set1],cex.axis=cex.axis,mgp=c(3,2,0))
  mtext(setlabs[2],1,2,at=setat)
  axis(4,pi,pilab,cex.axis=2*cex,las=1)
  invisible(ci)
}


## nn <- c(30,15,20,35)
## pi <- c("+"=.15,"-"=.85)
## grouplabs <- c(rep("-",3),"+")
## x <- c("(0,0)"=7,"(0,1)"=4,"(1,0)"=2,"(1,1)"=31)
## OCP (x,nn,grouplabs,pi,c("-","+"),ylim=c(0,1), reflty=2:3)
