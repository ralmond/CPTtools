## New file containing weight of evidence stuff.

## This takes a bunch of rows of the form "[High:.3,Med:.5,Low:.2]"
## and parses it into a vector c(High=.3,Med=.5,Low=.2)

parseProbVec <-
function (pVec) {
  ## trim whitespace
  pVec <- sub('^[[:space:]]+','',pVec)
  pVec <- sub('[[:space:]]+$','',pVec)
  if ( any(substr(pVec,1,1) != "[") ||
       any(substr(pVec,nchar(pVec),nchar(pVec)) != "]")) {
    stop("Expected a Probability vector of form [state0:val,state1:val,...]")
  }
  pVec <- substr(substr(pVec,1,nchar(pVec)-1),2,999999)
  pvs <- strsplit(pVec,",")
  t(sapply(pvs,parseProbVecRow))
}


## This takes a list of the form: c("High:.3","Med:.5","Low:.3")

parseProbVecRow <-
function (splitrow) {
  splits <- strsplit(splitrow,":")
  data <- sapply(splits,function(x) as.numeric(x[2]))
  names(data) <- sapply(splits,function(x) x[1])
  data
}


## Reads in from Larry's spreadsheet
readHistory <- function (csvfile) {
  rawdata <- read.csv(csvfile,as.is=TRUE)
  seq <- parseProbVec(rawdata$Result)
  row.names(seq) <- sub('.xml$','',rawdata$Item)
  seq
}

woeHist <- function (hist, pos, neg) {
  if (length(pos) >1) {
    ppos <- apply(hist[,pos],1,sum)
  } else {
    ppos <- hist[,pos]
  }
  if (length(neg) >1) {
    pneg <- apply(hist[,neg],1,sum)
  } else {
    pneg <- hist[,neg]
  }
  ## Centiban units
  lodds <- 100*log10(ppos/pneg)
  diff(lodds)
}


woeBal <- function (hist, pos, neg, obs=NULL, title="Evidence Balance Sheet",
                    col=rev(colorspread("slategray",ncol(hist),maxsat=TRUE)),
                    posCol="cyan",negCol="red",
                    stripCol=c("white","lightgray"),
                    lcex=.65) {
  oldpar <- par(mfrow=c(1,3),oma=c(0,2,2,2),mar=c(5,0.1,4,0.1)+.01)
  timesteps <- nrow(hist)
  mids <- barplot(c(rep(1,timesteps-1),0),xlab="",ylab="",
                  xaxt="n",yaxt="n",col=stripCol,horiz=TRUE,
                  border=NA)
  text(0,rev(mids),row.names(hist),adj=c(0,0.5),cex=lcex)
  if (!is.null(obs))
    text(1,rev(mids),obs,adj=c(1,0.5),cex=lcex)

  barplot(t(hist[timesteps:1,]),horiz=TRUE,names.arg=NULL,col=col,
          main="Probabilities",yaxt="n")
  woe <- woeHist(hist,pos,neg)
  barplot(rev(c(NA,woe)),horiz=TRUE,names.arg=NULL,
          col=c(ifelse(rev(woe)<0,negCol,posCol),"black"),cex.main=lcex,
          main=paste("WOE for ",paste(pos,collapse=","),
            " vs ",paste(neg,collapse=",")),
          yaxt="n")
  mtext(title,outer=TRUE)
  par(oldpar)
  invisible(mids)
}


EAPBal <- function (hist, contexts=names(hist), obs=NULL,
                    varname=deparse(substitute(hist)),elim=c(-1,1),
                    etic=4,
                    title=paste("EAP Balance Sheet:",varname),
                    col="slategray",
                    posCol="cyan",negCol="red",
                    stripCol=c("white","lightgray"),
                    lcex=.65) {
  oldpar <- par(mfrow=c(1,3),oma=c(0,2,2,2),mar=c(5,0.1,4,0.1)+.01)
  timesteps <- length(hist)
  mids <- barplot(c(rep(1,timesteps-1),0),xlab="",ylab="",
                  xaxt="n",yaxt="n",col=stripCol,horiz=TRUE,
                  border=NA)
  text(0,rev(mids),contexts,adj=c(0,0.5),cex=lcex)
  if (!is.null(obs))
    text(1,rev(mids),obs,adj=c(1,0.5),cex=lcex)

  xlim <- elim - min(elim)
  xlen <- diff(elim)
  barplot(rev(hist)-min(elim),horiz=TRUE,names.arg=NULL,col=col,
          main=paste("EAP[",varname,"]"),yaxt="n",xaxt="n",xlim=xlim)
  axis(1,at=seq(xlim[1],xlim[2],xlen/etic),labels=seq(elim[1],elim[2],xlen/etic))
  abline(v=mean(xlim))
  dhist <- diff(hist)
  barplot(rev(c(NA,dhist)),horiz=TRUE,names.arg=NULL,
          col=c(ifelse(rev(dhist)<0,negCol,posCol),"black"),
          xlim=elim,
          main=paste("ΔEAP[",varname,"]"),
          yaxt="n")
  abline(v=0)
  mtext(title,outer=TRUE)
  par(oldpar)
  invisible(mids)
}

