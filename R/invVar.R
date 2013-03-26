## Scale a covariance matrix to have a unit diagonal (i.e., create a
## correlation matrix from a covariance matrix. 
scaleMatrix <- function (X) {
  scale <- sqrt(diag(X))
  sweep(sweep(X,1,scale,"/"),2,scale,"/")
}

# Test code:
#all(round(scaleMatrix(MG.var),2)==MG.cor)

structMatrix <- function(X,threshold=0.1) {
  pcor <- scaleMatrix(solve(X))
  abs(pcor)>threshold
}

mcSearch <- function (sm,start = colnames(sm)[1]) {
  vars <- colnames(sm)
  ord <- rep(0,length(vars))
  names(ord) <- vars

  ## First choice is arbitrary
  ord[start] <- 1
  if (length(vars) == 1) {
    return(ord)
  }

  ## Now loop through assignments
  for (i in 2:length(vars)) {
    cards <- (ord >0)%*%sm
    cards[ord>0] <- -1 ## already picked
    nextvar <- vars[cards==max(cards)][1]
    ord[nextvar] <- i
  }
  ord
}

## Test cases
#> mcSearch(sm)
# Mechanics    Vectors    Algebra   Analysis Statistics 
#         1          2          3          4          5 
#> mcSearch(sm,"Algebra")
# Mechanics    Vectors    Algebra   Analysis Statistics 
#         2          3          1          4          5 

buildParentList <-
function (sm,start=colnames(sm)[1], ord=mcSearch(sm,start)) {

  vars <- names(sort(ord))
  nvars <- length(vars)
  ssm <- sm[order(ord),order(ord)]
  ## Select lower triangle
  ssm <- ssm & outer(1:nvars,1:nvars,">")
  apply(ssm,1,function(row) vars[row])
}

## > buildParentList(sm)
## $Mechanics
## character(0)

## $Vectors
## [1] "Mechanics"

## $Algebra
## [1] "Mechanics" "Vectors"  

## $Analysis
## [1] "Algebra"

## $Statistics
## [1] "Algebra"  "Analysis"

## > buildParentList(sm,"Algebra")
## $Algebra
## character(0)

## $Mechanics
## [1] "Algebra"

## $Vectors
## [1] "Algebra"   "Mechanics"

## $Analysis
## [1] "Algebra"

## $Statistics
## [1] "Algebra"  "Analysis"

################################################
## Probability transfer functions

pvecToCutpoints <- function (pvec, mean=0, std=1) {
  Ps <- c(0,cumsum(pvec))
  qnorm(Ps,mean,std)
}

pvecToMidpoints <- function (pvec, mean=0, std=1) {
  Ps <- cumsum(pvec)-pvec/2
  qnorm(Ps,mean,std)
}

## > pvecToCutpoints(c(.025,.95,.025))
## [1]      -Inf -1.959964  1.959964       Inf
## > pvecToMidpoints(c(.025,.95,.025))
## [1] -2.241403  0.000000  2.241403
## > pnorm(pvecToCutpoints(c(.025,.95,.025)))
## [1] 0.000 0.025 0.975 1.000
## > pnorm(pvecToMidpoints(c(.025,.95,.025)))
## [1] 0.0125 0.5000 0.9875

areaProbs <- function (pvec,condmean, condstd,mean=0,std=1) {
  cuts <- (pvecToCutpoints(pvec,mean,std)-condmean)/condstd
  probs <- diff(pnorm(cuts))
  names(probs) <- names(pvec)
  probs
}

## > areaProbs(c(.25,.5,.25),1,.5)
## [1] 0.000405549 0.257111078 0.742483373


buildRegressions <-
function (Sigma,means=0,
          parents = buildParentList(structMatrix(Sigma))) {
  if (length(means) ==1) {
    means <- rep(means,length(parents))
    names(means) <- names(parents)
  }
  result <- lapply(names(parents), function (var) {
    pars <- parents[[var]]
    sigYY <- Sigma[var,var]
    muY <- means[var]
    if (length(pars) == 0) {
      list(b=numeric(0), a=muY, std=sqrt(sigYY))
    } else {
      sigYX <- Sigma[var,pars]
      invsigXX <- solve(Sigma[pars,pars])
      muX <- means[pars]
      b <- sigYX %*% invsigXX
      colnames(b) <- pars
      list(b = b, a = as.vector(muY - b %*% muX),
           std = sqrt(as.vector(sigYY - b %*% sigYX)))
    }
  })
  names(result) <- names(parents)
  result
}
           

buildRegressionTables <-
function(regs, pvecs, mean=0, std=1) {
  vnames <- names(pvecs)
  if (length(mean) ==1) {
    mean <- rep(mean,length(pvecs))
    names(mean) <- vnames
  }
  if (length(std) ==1) {
    std <- rep(std,length(pvecs))
    names(std) <- vnames
  }
  ## calculate matrix of state names and x* values.
  vstates <- lapply(pvecs,function(pv) names(pv))
  vvals <- lapply(names(pvecs), function (v1) {
    rev(pvecToMidpoints(rev(pvecs[[v1]]),mean[v1],std[v1]))
  })
  names(vvals) <- names(pvecs)
  
  result <- lapply(vnames, function (var) {
    if (length(regs[[var]]$b) == 0) {
      as.data.frame(matrix(pvecs[[var]],nrow=1,
                           dimnames=list(NULL,names(pvecs[[var]]))))
    } else {
      slopes <- regs[[var]]$b
      pars <- colnames(slopes)   # co-efficients should be named list.
      parstates <- expand.grid(vstates[pars])
      parvals <- as.matrix(expand.grid(vvals[pars]))
      condmeans <- (parvals %*% t(slopes)) + regs[[var]]$a
      probs <- t(sapply(condmeans, function(mu) {
        areaProbs(rev(pvecs[[var]]),mu,regs[[var]]$std,mean[[var]],std[[var]])
      }))
      data.frame(parstates,probs[,vstates[[var]]])
    }
  })
  names(result) <- vnames
  result
}

## rt <- buildRegressions(MG.var,MG.means, buildParentList(sm,"Algebra"))
## tabs <- buildRegressionTables(rt, MG.pvecs, MG.means, sqrt(diag(MG.var)))



## amid <- pvecToMidpoints(MG.pvecs[["Algebra"]],MG.means["Algebra"],
##                 sqrt(MG.var["Algebra","Algebra"]))

## mmid <- pvecToMidpoints(MG.pvecs[["Mechanics"]],MG.means["Mechanics"],
##                 sqrt(MG.var["Mechanics","Mechanics"]))

## mtab <- cbind(tabs[["Mechanics"]],x.Algebra=amid)
## mtab <- mtab[,c(1,5,2:4)]

## vtab <- cbind(tabs[["Vectors"]],x.Algebra=amid,x.Mechanics=mmid)
## vtab <- vtab[,c(1:2,6:7,3:5)]

