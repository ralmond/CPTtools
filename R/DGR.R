### Discrete Graded Response Models.

### These are similar to the DiBello-Samejima models, except that the
### two Samejima curves have different discriminations.  Also, the
### discriminations go with the output states, not the input
### variables.

### skillLevels --- list of labels for the skill level variable.
### obsLevels --- labels for obserable values.
### skillLevels and obsLevels are assumed to be ordered highest to
### lowest.

### Unlike the DiBello-Samejima rule, lnAlphas, betas, and
### lnAlphas --- set of log slopes, one for each obs level
### beta --- difficulty (-intercept) parameter
### rule --- Function for computing effective theta.  This should have the
### signature   function(thetas,alphas,beta)

### The first one in the list represents the difference between the
### highest and next lowest obs state, and so forth.
calcDPCTable <- function (skillLevels, obsLevels, lnAlphas, betas,
                          rules="Compensatory", link="partialCredit",
                          linkScale=NULL,Q=TRUE,
                          tvals=lapply(skillLevels,
                              function (sl) effectiveThetas(length(sl)))) {

  k <- length(obsLevels)
  if (k < 2) {
    stop("There must be at least two obsLevels to caluclate a CPT, got",
         obsLevels)
  }
  if (!is.list(lnAlphas)) lnAlphas <- list(lnAlphas)
  if (length(lnAlphas) != k-1) lnAlphas <- rep(lnAlphas,k-1)
  if (!is.list(betas)) betas <- list(betas)
  if (length(betas) != k-1) betas <- rep(betas,k-1)
  if (!is.list(rules)) rules <- list(rules)
  if (length(rules) != k-1) rules <- rep(rules,k-1)
  if (!is.list(tvals)) tvals <- list(tvals)

  p <- length(skillLevels)
  if (length(Q)==1) {
    Q <- matrix(TRUE,k-1L,max(p,1L))
  } else if (!is.matrix(Q) || nrow(Q) != k-1L || ncol(Q) != p) {
    stop("Q must be a",k-1,"by",p,"matrix.")
  }

  thetas <- do.call("expand.grid",tvals)
  if (nrow(thetas) == 0L) {
    thetas <- data.frame(X0=0)
  }

  et <- matrix(0,nrow(thetas),k-1) #Take care of no parent case
  for (kk in 1:(k-1)) {
    a <- exp(lnAlphas[[kk]])
    b <- betas[[kk]]
    npar <- sum(Q[kk,])
    rul <- rules[[kk]]
    if (npar > 1L) {
      if (isOffsetRule(rul)) {
        if (length(b)==1L) b <- rep(b,npar)
      } else {
        if (length(a)==1L) 
          a <- rep(a,npar)
      }
    }
    et[,kk] <- do.call(rul,list(thetas[,Q[kk,],drop=FALSE],a,b))
  }
  do.call(link,list(et,linkScale,obsLevels))
}

calcDPCFrame <-
function (skillLevels, obsLevels, lnAlphas, betas,
          rules="Compensatory", link="partialCredit",
          linkScale=NULL,Q=TRUE,
          tvals=lapply(skillLevels,
              function (sl) effectiveThetas(length(sl)))) {
  markers <- expand.grid(skillLevels)
  tab <- calcDPCTable(skillLevels,paste(obsLevels),
                      lnAlphas,betas,rules,link,
                      linkScale,Q,tvals)
  if (length(skillLevels)>0L) {
    result <- data.frame(markers,tab)
  } else {
    result <- data.frame(tab)
  }
  if (is.numeric(obsLevels) ||
      names(result)[length(skillLevels)+1]!=paste(obsLevels[1])) {
    ## R is "helpfully" fixing our numeric labels.  Need to insist.
    names(result) <- c(names(skillLevels),paste(obsLevels))
  }
  as.CPF(result)

}

