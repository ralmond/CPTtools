
### Link Functions
### These are a function of three arguments
### et -- a table of effective thetas, one for row for each
###       configuration of parent variables, and one column for
###       each child state except for the last.
### k -- the number of states of the child variable.
### linkScale --- a scale parameter used by some link functions.
### obsLevel --- a list of names of the observables, assume to be
### sorted from highest to lowest.

### It returns a conditional probability table.

partialCredit <- function (et,linkScale=NULL,obsLevels=NULL) {
  m <- ncol(et)+1
  zt <- apply(et,1,function(x) rev(cumsum(rev(x))))
  if (m==2) {
    zt <- matrix(zt,length(et),1)
  } else {
    zt <- t(zt)
  }
  pt <- cbind(exp(1.7*zt),1)
  pt <- sweep(pt,1,apply(pt,1,sum),"/")
  probs <- pt[,1:m,drop=FALSE]
  probs <- ifelse(probs<0,0,probs)
  if (!is.null(obsLevels)) {
    dimnames(probs) <- list(NULL,obsLevels)
  }
  probs
}


gradedResponse <- function (et,linkScale=NULL,obsLevels=NULL) {
  m <- ncol(et)+1
  zt <- 1/(1+exp(-1.7*et))
  ## The cummax corrects for problems where the
  ## Pr(X> m+1) is greater than Pr(X>m)
  zt <- apply(zt,1,cummax)
  if (m==2) {
    zt <- matrix(zt,length(et),1)
  } else {
    zt <- t(zt)
  }
  pt <- cbind(0,zt,1)
  probs <- pt[,2:(m+1),drop=FALSE]-pt[,1:m,drop=FALSE]
  # probs <- ifelse(probs<0,0,probs)
  if (!is.null(obsLevels)) {
    dimnames(probs) <- list(NULL,obsLevels)
  }
  probs
}

normalLink <- function (et,linkScale=NULL,obsLevels=NULL) {
  if (is.null(linkScale))
    stop("Link Scale parameter required for normal link.")
  m <- ncol(et)+1
  
  cuts <- qnorm( ((m-1):1)/m)
  ## Only play attention to the first column.
  pt <- pnorm(outer(-et[,1],cuts,"+")/linkScale)
  pt <- cbind(1,pt,0)
  probs <- pt[,1:m,drop=FALSE]-pt[,1+(1:m),drop=FALSE]
  if (!is.null(obsLevels)) {
    dimnames(probs) <- list(NULL,obsLevels)
  }
  probs
}

