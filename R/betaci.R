################################
## Confidence intervals for proportions based on Beta distributions.

"betaci" <-
function (sumData, totals = NULL,
          limits=c(lower=0.025,upper=0.975),
          a=.5,b=.5) {
  if (missing(totals)) {
    if (is.array(sumData)) {
      totals <- rowSums(sumData)
    } else {
      totals <- sum(sumData)
    }
  }
  if (is.matrix(sumData) && !is.matrix(totals)) {
    totals <- matrix(totals,nrow(sumData),ncol(sumData))
  }
  lapply(limits, function (prob)
         qbeta(prob,sumData+a,totals-sumData+b))
}

## Doesn't work and not currently used.
# dirichci <- function (counts, lnames=names(counts),
#                      a = rep(1/length(counts),length(counts)),
#                      limits=c(lower=0.025,mid=.5,upper=0.975)) {
#   tot <- sum(counts+a)
#   dat <- cumsum(counts+a)[-length(counts)]
#   data.frame(Level = lnames[-length(counts)],
#              lapply(limits, qbeta,dat, tot-dat))
# }


"proflevelci" <- function(data,profindex,
                          limits=list(lower=.025,upper=.975),
                          a=.5, b=.5) {
  if (is.data.frame(data)) data <- as.matrix(data)
  if (!is.matrix(data)) 
    data <- matrix(data,ncol=1,dimnames=list(names(data),
                                             toString(substitute(data))))
  ss <- matrix(colSums(data),nrow(data),ncol(data),byrow=TRUE)
  if (profindex == 1) {
    datlow <- data[profindex,,drop=FALSE]
    sslow <- ss[profindex,,drop=FALSE]
  } else {
    ## Need a reverse cumsum here.
    datlow <- apply(data[profindex:1,,drop=FALSE],2,cumsum)
    datlow <- datlow[profindex:1,,drop=FALSE]
    sslow <- ss[1:profindex,,drop=FALSE]
  }
  if (profindex == nrow(data)-1) {
    dathigh <- data[profindex+1,,drop=FALSE]
    sshigh <- ss[profindex+1,,drop=FALSE]
  } else {
    dathigh <- apply(data[(profindex+1):nrow(data), ,drop=FALSE],2,cumsum)
    sshigh <- ss[(profindex+1):nrow(data), ,drop=FALSE]
  }
  ci.low <- betaci(datlow,sslow,limits,a,b)
  ci.high <- betaci(dathigh,sshigh,limits,a,b)
  result <- lapply (names(limits), function (bound) {
    tab <- rbind(-ci.low[[bound]],ci.high[[bound]])
    rownames(tab) <- rownames(data)
    tab
  })
  names(result) <- names(limits)
  result
}
