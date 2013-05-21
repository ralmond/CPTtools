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


woeBal <- function (hist, pos, neg, title="Evidence Balance Sheet",
                    col=rev(colorspread("slategray",ncol(hist),maxsat=TRUE)),
                    lcex=.65) {
  par(mfrow=c(1,3),oma=c(0,0,2,0))
  timesteps <- nrow(hist)
  plot(c(0,1),c(0,timesteps),xlab="",ylab="",type="n",
       frame=FALSE,xaxt="n",yaxt="n")
  text(0,(timesteps:1)-.5,row.names(hist),adj=c(0,0.5),cex=lcex)
  barplot(t(hist[timesteps:1,]),horiz=TRUE,names.arg=NULL,col=col,main="Probabilities")
  woe <- woeHist(hist,pos,neg)
  barplot(rev(c(NA,woe)),horiz=TRUE,names.arg=NULL,
          col=c(ifelse(rev(woe)<0,"red","cyan"),"black"),cex.main=lcex,
          main=paste("WOE for ",paste(pos,collapse=","),
            " vs ",paste(neg,collapse=",")))
  mtext(title,outer=TRUE)
  invisible(woe)
}
  
