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


## eCPTplot <- function(cpf,counts) {

## }

## pct <- tribble(
##     ~Level, ~count, ~prob,
##     "High", 1, 1/3,
##     "Med", 2, 1/3,
##     "Low", 3, 1/3
## )

## pct <- tibble(Level=ordered(c("High","Medium","Low"),
##                             levels=c("High","Medium","Low")),
##               count=1:3, prob=1/3)



## full_join(pct,dirchci(pct$prob,pct$Level)) %>%
##   mutate(xpos=(1:nrow(pct))/nrow(pct)+.5) %>%
##   ggplot(aes(x=1,y=prob,fill=Level)) +
##   geom_col() +
##   geom_pointrange(aes(x=xpos,y=mid,ymin=lower,ymax=upper,shape=Level))

OCP.CPF <- function(obs, exp, ..., baseCol="chocolate",
                    limits=c(lower=0.025,upper=0.975),
                    Parents = getTableParents(exp),
                    States = getTableStates(exp),
                    nstates=length(States),
                    key=list(points=FALSE,rectangles=TRUE,text=States),
                    par.settings=list(),
                    point.pch = paste((nstates-1):0),
                    point.col="black", point.cex=1.25,
                    line.col="black", line.type=1) {

  if (!all.equal(getTableStates(obs),getTableStates(exp))) {
    stop("Child variable states don't match.")
  }
  if (!all.equal(getTableParents(obs),getTableParents(exp))) {
    stop("Parent variables don't match.")
  }
  if (nrow(obs)!=nrow(exp)) {
    stop("Observed and expected table sizes don't match.")
  }

  ## Set up color scale
  if (is.null(baseCol)) {
    ps <- par.settings
  } else {
    colscale <- rev(colorspread(baseCol,nstates))
    ps <- c(par.settings,
            list(superpose.polygon=list(col=colscale)))
    key <- c(key,list(col=colscale))
  }

  ## Build CI's for cumulative probabilities.
  cobs <-t(apply(numericPart(exp)+numericPart(obs),1,cumsum))
  cobs.tot <- matrix(cobs[,nstates],nrow(cobs),nstates)
  cobs[,nstates] <- NA ## drop the last category
  cobs.lower <- qbeta(limits["lower"],cobs,cobs.tot-cobs)
  cobs.mid <- cobs/cobs.tot
  cobs.upper <- qbeta(limits["upper"],cobs,cobs.tot-cobs)

  ## Set up the barchart
  if (length(Parents) == 0L) {
    form <- as.formula("~p")
  } else {
    form <- as.formula(paste("~p |",paste(Parents,collapse="+")))
  }
  exp_long <- tidyr::pivot_longer(exp,tidyr::all_of(States),names_to="State",values_to="p")
  exp_long$State <- ordered(exp_long$State,levels=States)

  ## Set up Y placement based on number of categories
  YY <- .5 + ((nstates-1):0)/nstates

  lattice::barchart(form,data=exp_long,
                    groups=exp_long$State,auto.key=key,stack=TRUE,
                    panel = function (x,y,subscripts,groups,...) {
                      lattice::panel.barchart(x=x,y=y,subscripts=subscripts,
                                              groups=groups,box.width=1,...)
                      lattice::lpoints(t(cobs.mid)[subscripts],YY,
                                       pch=point.pch,
                                       col=point.col,cex=point.cex)
                      lattice::lsegments(t(cobs.lower)[subscripts],YY,
                                         t(cobs.upper)[subscripts],YY,
                                         col=line.col,lty=line.type)
                    },
                    strip=lattice::strip.custom(strip.names=TRUE),
                    par.settings=ps,
                    ...)

}


OCP2.CPF <- function(obs1, obs2, exp, ..., baseCol="chocolate",
                    limits=c(lower=0.025,upper=0.975),
                    Parents = getTableParents(exp),
                    States = getTableStates(exp),
                    nstates=length(States),
                    key=list(points=FALSE,rectangles=TRUE,text=States),
                    par.settings=list(),
                    point1.pch=paste((nstates-1):0),
                    point1.col="black", point1.cex=1.25,
                    line1.col="black", line1.type=1,
                    point2.pch=paste((nstates-1):0),
                    point2.col="cyan", point2.cex=1.25,
                    line2.col="cyan", line2.type=2) {

  if (!all.equal(getTableStates(obs1),getTableStates(exp))) {
    stop("Child variable states don't match.")
  }
  if (!all.equal(getTableStates(obs2),getTableStates(exp))) {
    stop("Child variable states don't match.")
  }
  if (!all.equal(getTableParents(obs1),getTableParents(exp))) {
    stop("Parent variables don't match.")
  }
  if (!all.equal(getTableParents(obs2),getTableParents(exp))) {
    stop("Parent variables don't match.")
  }
  if (nrow(obs1)!=nrow(exp) || nrow(obs2)!=nrow(exp)) {
    stop("Observed and expected table sizes don't match.")
  }


  ## Set up color scale
  if (is.null(baseCol)) {
    ps <- par.settings
  } else {
    colscale <- rev(colorspread(baseCol,nstates))
    ps <- c(par.settings,
            list(superpose.polygon=list(col=colscale)))
    key <- c(key,list(col=colscale))
  }

  ## Build CI's for cumulative probabilities.
  cobs1 <-t(apply(numericPart(exp)+numericPart(obs1),1,cumsum))
  cobs1.tot <- matrix(cobs1[,nstates],nrow(cobs1),nstates)
  cobs1[,nstates] <- NA ## drop the last category
  cobs1.lower <- qbeta(limits["lower"],cobs1,cobs1.tot-cobs1)
  cobs1.mid <- cobs1/cobs1.tot
  cobs1.upper <- qbeta(limits["upper"],cobs1,cobs1.tot-cobs1)

  cobs2 <-t(apply(numericPart(exp)+numericPart(obs2),1,cumsum))
  cobs2.tot <- matrix(cobs2[,nstates],nrow(cobs2),nstates)
  cobs2[,nstates] <- NA ## drop the last category
  cobs2.lower <- qbeta(limits["lower"],cobs2,cobs2.tot-cobs2)
  cobs2.mid <- cobs2/cobs2.tot
  cobs2.upper <- qbeta(limits["upper"],cobs2,cobs2.tot-cobs2)


  ## Set up the barchart
  if (length(Parents) == 0L) {
    form <- as.formula("~p")
  } else {
    form <- as.formula(paste("~p |",paste(Parents,collapse="+")))
  }
  exp_long <- tidyr::pivot_longer(exp,tidyr::all_of(States),names_to="State",values_to="p")
  exp_long$State <- ordered(exp_long$State,levels=States)

  ## Set up Y placement based on number of categories
  YY1 <- 1 + ((nstates-1):0)/(2*nstates)
  YY2 <- .5 + ((nstates-1):0)/(2*nstates)

  lattice::barchart(form,data=exp_long,
                    groups=exp_long$State,auto.key=key,stack=TRUE,
                    panel = function (x,y,subscripts,groups,...) {
                      lattice::panel.barchart(x=x,y=y,subscripts=subscripts,
                                              groups=groups,box.width=1,...)
                      lattice::lpoints(t(cobs1.mid)[subscripts],YY1,
                                       pch=point1.pch,
                                       col=point1.col,cex=point1.cex)
                      lattice::lsegments(t(cobs1.lower)[subscripts],YY1,
                                         t(cobs1.upper)[subscripts],YY1,
                                         col=line1.col,lty=line1.type)
                      lattice::lpoints(t(cobs2.mid)[subscripts],YY2,
                                       pch=point2.pch,
                                       col=point2.col,cex=point2.cex)
                      lattice::lsegments(t(cobs2.lower)[subscripts],YY2,
                                         t(cobs2.upper)[subscripts],YY2,
                                         col=line2.col,lty=line2.type)
                    },
                    strip=lattice::strip.custom(strip.names=TRUE),
                    par.settings=ps,
                    ...)

}


cptChi2 <- function(obs, exp) {

  if (!all.equal(getTableStates(obs),getTableStates(exp))) {
    stop("Child variable states don't match.")
  }
  if (!all.equal(getTableParents(obs),getTableParents(exp))) {
    stop("Parent variables don't match.")
  }
  if (nrow(obs)!=nrow(exp)) {
    stop("Observed and expected table sizes don't match.")
  }

  ## Build CI's for cumulative probabilities.
  cobs <- numericPart(exp)+numericPart(obs)
  cobs.tot <-apply(cobs,1,sum)
  nexp <- sweep(numericPart(exp),1,cobs.tot,"*")
  
  df = nrow(cobs)*(ncol(cobs)-1)
  chi2 <- sum((cobs-nexp)^2/nexp)
  attr(chi2,"d.f.") <- df
  chi2
}
