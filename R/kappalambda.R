
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
