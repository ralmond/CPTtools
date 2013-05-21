################################
## Confidence intervals for proportions based on Beta distributions.

"betaci" <-
function (sumData, totals = NULL,
          limits=c(lower=0.025,upper=0.975),
          a=.5,b=.5) {
  if (missing(totals)) {
    totals <- matrix (colSums(sumData),nrow(sumData),ncol(sumData),byrow=TRUE)
  }
  lapply(limits, function (prob)
         qbeta(prob,sumData+a,totals-sumData+b))
}


"proflevelci" <- function(data,profindex,
                          levels=list(lower=.025,upper=.975),
                          a=.5, b=.5) {
  ss <- matrix(colSums(data),nrow(data),ncol(data),byrow=TRUE)
  if (profindex == 1) {
    datlow <- data[profindex,]
    sslow <- ss[profindex,]
  } else {
    ## Need a reverse cumsum here.
    datlow <- apply(data[profindex:1,],2,cumsum)
    datlow <- datlow[profindex:1,]
    sslow <- ss[1:profindex,]
  }
  if (profindex == nrow(data)-1) {
    dathigh <- data[profindex+1,]
    sshigh <- ss[profindex+1,]
  } else {
    dathigh <- apply(data[(profindex+1):nrow(data),],2,cumsum)
    sshigh <- ss[(profindex+1):nrow(data),]
  }
  ci.low <- betaci(datlow,sslow,levels,a,b)
  ci.high <- betaci(dathigh,sshigh,levels,a,b)
  result <- sapply (names(levels), function (bound)
                    rbind(-ci.low[[bound]],ci.high[[bound]]))
  names(result) <- names(levels)
  result
}
