
gkLambda <- function (tab, weights=c("None","Linear","Quadratic"),
                      W = diag(nrow(tab))) {
  K <- nrow(tab)
  if (nrow(tab)!=ncol(tab))
    stop("Expected a square matrix.")
  if (missing(W)) {
    if (is.character(weights)) weights <- match.arg(weights)
    W <- switch(weights,
                None=W,
                Linear= W - abs(outer(1:K,1:K,"-"))/(K-1),
                Quadratic= W - (outer(1:K,1:K,"-")/(K-1))^2)
  }
  N <- sum(tab)
  prow <- rowSums(tab)/N
  pmax <- max(colSums(sweep(W,1,prow,"*")))
  lambda <- (sum(W*tab)/N - pmax)/(1-pmax)
  return(lambda)
}

fcKappa <- function (tab, weights=c("None","Linear","Quadratic"),
                     W = diag(nrow(tab))) {
  K <- nrow(tab)
  if (nrow(tab)!=ncol(tab))
    stop("Expected a square matrix.")
  if (missing(W)) {
    if (is.character(weights)) weights <- match.arg(weights)
    W <- switch(weights,
                None=W,
                Linear= W - abs(outer(1:K,1:K,"-"))/(K-1),
                Quadratic= W - (outer(1:K,1:K,"-")/(K-1))^2)
  }
  N <- sum(tab)
  agree <- sum(W*tab)/N
  prow <- rowSums(tab)/N
  pcol <- colSums(tab)/N
  expagree <-sum(W*outer(prow,pcol))
  (agree - expagree)/(1-expagree)
}

gkGamma <- function (tab) {
    tab <- as.matrix(tab)
    N <- sum(tab)
    rtab <- tab/N
    PIs <- PId <- 0
    for (a in 1:(nrow(rtab)-1)) {
        for (aa in (a+1):nrow(rtab)) {
             for (b in 1:ncol(rtab)) {
                 if (ncol(rtab) > b) {
                     for (bb in (b+1):ncol(rtab)) {
                         PIs <- PIs + rtab[a,b]*rtab[aa,bb]
                     }
                 }
                 if (b > 1) {
                     for (bb in 1:(b-1)) {
                         PId <- PId + rtab[a,b]*rtab[aa,bb]
                     }
                 }
             }
        }
    }
    PIs <- 2*PIs
    PId <- 2*PId
    PIt <- 1-PIs-PId
    (PIs-PId)/(1-PIt)
    
}

