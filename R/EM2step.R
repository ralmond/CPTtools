#library(stats4) ## Needed for optim

### This does an optimization given the posterior matrix.
mapDPC <- function (postTable,skillLevels,obsLevels,lnAlphas,betas,
                    rules="Compensatory",link="partialCredit",linkScale=NULL,
                    Q=TRUE, tvals=lapply(skillLevels,
                                function (sl) effectiveThetas(length(sl))),
                    ...) {
  k <- length(obsLevels)
  pvec <- numeric(0L)
  iparam <- 0L
  ialpha <- vector("list",k-1L)
  if (!is.list(lnAlphas)) {
    nalpha <- length(lnAlphas)
    if (nalpha > 0L) {
      ia <- 1L:nalpha
    } else {
      ia <- numeric(0L)
    }
    ialpha <- rep(list(ia),k-1L)
    pvec <- c(pvec,lnAlphas)
    iparam <- nalpha
  } else {
    if (length(lnAlphas)!=k-1L) {
      stop("Number of Alpha vectors should match number of states less one.")
    }
    for (kk in 1L:(k-1L)) {
      nalpha <- length(lnAlphas[[kk]])
      ialpha[[kk]] <- iparam+(1L:nalpha)
      pvec <- c(pvec,lnAlphas[[kk]])
      iparam <- iparam + nalpha
    }
  }
  ibeta <- vector("list",k-1L)
  if (!is.list(betas)) {
    nbeta <- length(betas)
    pvec <- c(pvec,betas)
    if (nbeta > 0L) {
      ib <- iparam+1L:nbeta
    } else {
      ib <- numeric(0L)
    }
    ibeta <- rep(list(ib),k-1L)
    iparam <- iparam + nbeta
  } else {
    if (length(betas)!=k-1L) {
      stop("Number of Beta vectors should match number of states less one.")
    }
    for (kk in 1L:(k-1L)) {
      nbeta <- length(betas[[kk]])
      ibeta[[kk]] <- iparam+(1L:nbeta)
      pvec <- c(pvec,betas[[kk]])
      iparam <- iparam + nbeta
    }
  }
  if (length(linkScale) > 0L) {
    iscale <- iparam+1L:length(linkScale)
    pvec <- c(pvec,log(linkScale))
  } else {
    iscale <- NULL
  }

  if (!is.list(rules)) rules <- list(rules)
  if (length(rules) != k-1L) rules <- rep(rules,k-1L)

  p <- length(skillLevels)
  if (length(Q)==1L) {
    Q <- matrix(TRUE,k-1L,p)
  } else if (!is.matrix(Q) || nrow(Q) != k-1L || ncol(Q) != p) {
    stop("Q must be a",k-1,"by",p,"matrix.")
  }

  thetas <- do.call("expand.grid",tvals)

  llike <- function (pv) {
    et <- matrix(0,max(nrow(thetas),1L),k-1L)
    for (kk in 1L:(k-1L)) {
      et[,kk] <- do.call(rules[[kk]],
                        list(thetas[,Q[kk,],drop=FALSE],
                             exp(pv[ialpha[[kk]]]),
                             pv[ibeta[[kk]]]))
    }
    if (!is.null(iscale)) {
      ls <- exp(pv[iscale])
    } else {
      ls <- NULL
    }
    probs <- do.call(link,list(et,ls,obsLevels))
    -2*sum(as.vector(postTable)*as.vector(log(probs)))
 }
  map <- optim(pvec,llike,...)
  if (map$convergence !=0) {
    warning("Optimization did not converge.")
    warning(map$message)
  }
  ## Extract alpha and beta vectors.
  if (!is.list(lnAlphas)) {
    map$lnAlphas <- map$par[ialpha[[1L]]]
  } else {
    map$lnAlphas <- vector("list",k-1L)
    for (kk in 1L:(k-1L)) {
      map$lnAlphas[[kk]] <- map$par[ialpha[[kk]]]
    }
  }
  if (!is.list(betas)) {
    map$betas <- map$par[ibeta[[1L]]]
  } else {
    map$betas <- vector("list",k-1L)
    for (kk in 1L:(k-1L)) {
      map$betas[[kk]] <- map$par[ibeta[[kk]]]
    }
  }
  if (!is.null(iscale)) {
    map$linkScale <- exp(map$par[iscale])
  }
  map
}

