library(stats4) ## Needed for optim

### This does an optimization given the posterior matrix.
mapDPC <- function (postTable,skillLevels,obsLevels,lnAlphas,betas,
                    rules="Compensatory",link="partialCredit",linkScale=NULL,...) {
  k <- length(obsLevels)
  pvec <- numeric(0)
  iparam <- 0
  ialpha <- vector("list",k-1)
  if (!is.list(lnAlphas)) {
    nalpha <- length(lnAlphas)
    ialpha <- rep(list(1:nalpha),k-1)
    pvec <- c(pvec,lnAlphas)
    iparam <- (k-1)*nalpha
  } else {
    if (length(lnAlphas)!=k-1) {
      stop("Number of Alpha vectors should match number of states less one.")
    }
    for (kk in 1:(k-1)) {
      nalpha <- length(lnAlphas[[kk]])
      ialpha[[kk]] <- iparam+(1:nalpha)
      pvec <- c(pvec,lnAlphas[[kk]])
      iparam <- iparam + nalpha
    }
  }
  ibeta <- vector("list",k-1)
  if (!is.list(betas)) {
    nbeta <- length(betas)
    pvec <- c(pvec,betas)
    ibeta <- rep(list(iparam+(1:nbeta)),k-1)
  } else {
    if (length(betas)!=k-1) {
      stop("Number of Beta vectors should match number of states less one.")
    }
    for (kk in 1:(k-1)) {
      nbeta <- length(betas[[kk]])
      ibeta[[kk]] <- iparam+(1:nbeta)
      pvec <- c(pvec,betas[[kk]])
      iparam <- iparam + nbeta
    }
  }
  if (!is.list(rules)) rules <- list(rules)
  if (length(rules) != k-1) rules <- rep(rules,k-1)

  pdims <- sapply(skillLevels,length)
  tvals <- lapply(pdims,effectiveThetas)
  thetas <- do.call("expand.grid",tvals)

  llike <- function (pv) {
    et <- matrix(0,nrow(thetas),k-1)
    for (kk in 1:(k-1)) {
      et[,kk] <- do.call(rules[[kk]],
                        list(thetas,exp(pv[ialpha[[kk]]]),
                             pv[ibeta[[kk]]]))
    }
    probs <- do.call(link,list(et,k,obsLevels))
    -2*sum(postTable*log(probs))
  }
  map <- optim(pvec,llike,...)
  if (map$convergence !=0) {
    warning("Optimization did not converge.")
    warning(map$message)
  }
  ## Extract alpha and beta vectors.
  if (!is.list(lnAlphas)) {
    map$lnAlphas <- map$par[ialpha[[1]]]
  } else {
    map$lnAlphas <- vector("list",k-1)
    for (kk in 1:(k-1)) {
      map$lnAlphas[[kk]] <- map$par[ialpha[[kk]]]
    }
  }
  if (!is.list(betas)) {
    map$betas <- map$par[ibeta[[1]]]
  } else {
    map$betas <- vector("list",k-1)
    for (kk in 1:(k-1)) {
      map$betas[[kk]] <- map$par[ibeta[[kk]]]
    }
  }
  
  map 
}

