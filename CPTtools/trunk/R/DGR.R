### Discrete Graded Response Models.

### These are similar to the DiBello-Samejima models, except that the
### two Samejima curves have different discriminations.  Also, the
### discriminations go with the output states, not the input
### variables.

### skillLevels --- list of labels for the skill level variable.
### obsLevels --- labels for obserable values.
### skillLevels and obsLevels are assumed to be ordered highest to
### lowest.

### Unlike the DiBello-Samejima model, lnAlphas, betas, and
### lnAlphas --- set of log slopes, one for each obs level -1
### betas --- difficulty (-intercept) parameter
### rules --- Function for computing effective theta.  This should have the
### signature   function(thetas,alphas,beta)

### The first one in the list representes the difference between the
### highest and next lowest obs state, and so forth.
calcDGRTable <- function (skillLevels, obsLevels, lnAlphas, betas, 
                         rules="Compensatory") {
  
  k <- length(obsLevels)
  if (!is.list(lnAlphas)) lnAlphas <- list(lnAlphas)
  if (length(lnAlphas) != k-1) lnAlphas <- rep(lnAlphas,k-1)
  if (!is.list(betas)) betas <- list(betas)
  if (length(betas) != k-1) betas <- rep(betas,k-1)
  if (!is.list(rules)) rules <- list(rules)
  if (length(rules) != k-1) rules <- rep(rules,k-1)


  pdims <- sapply(skillLevels,length)
  tvals <- lapply(pdims,effectiveThetas)
  thetas <- do.call("expand.grid",tvals)
  
  pt <- matrix(0,nrow(thetas),k-1)
  for (kk in 1:(k-1)) {
    etheta <- do.call(rules[[kk]],
                      list(thetas,exp(lnAlphas[[kk]]),betas[[kk]]))
    pt[,kk] <- 1/(1+exp(-1.7*etheta))
  }
  gradedResponse(pt,k,obsLevels)
}

gradedResponse <- function (pt,k,obsLevels=NULL) {
  ## The cummax corrects for problems where the
  ## Pr(X> k+1) is greater than Pr(X>k)
  pt <- cbind(0,t(apply(pt,1,cummax)),1)
  probs <- pt[,2:(k+1)]-pt[,1:k]
  # probs <- ifelse(probs<0,0,probs)
  if (!is.null(obsLevels)) {
    dimnames(probs) <- list(NULL,obsLevels)
  }
  probs
}


calcDGRFrame <-
function (skillLevels, obsLevels, lnAlphas, betas, 
          rules="Compensatory") {
  result <- data.frame(expand.grid(skillLevels),
                       calcDGRTable(skillLevels,paste(obsLevels),
                                   lnAlphas,betas,rules))
  if (is.numeric(obsLevels) ||
      names(result)[length(skillLevels)+1]!=paste(obsLevels[1])) {
    ## R is "helpfully" fixing our numeric labels.  Need to insist.
    names(result) <- c(names(skillLevels),paste(obsLevels))
  }
  result

}


### skillLevels --- list of labels for the skill level variable.
### obsLevels --- labels for obserable values.
### skillLevels and obsLevels are assumed to be ordered highest to
### lowest.

### Unlike the DiBello-Samejima rule, lnAlphas, betas, and
### lnAlphas --- set of log slopes, one for each obs level
### beta --- difficulty (-intercept) parameter
### rule --- Function for computing effective theta.  This should have the
### signature   function(thetas,alphas,beta)

### The first one in the list representes the difference between the
### highest and next lowest obs state, and so forth.
calcDPCTable <- function (skillLevels, obsLevels, lnAlphas, betas, 
                         rules="Compensatory") {
  
  k <- length(obsLevels)
  if (!is.list(lnAlphas)) lnAlphas <- list(lnAlphas)
  if (length(lnAlphas) != k-1) lnAlphas <- rep(lnAlphas,k-1)
  if (!is.list(betas)) betas <- list(betas)
  if (length(betas) != k-1) betas <- rep(betas,k-1)
  if (!is.list(rules)) rules <- list(rules)
  if (length(rules) != k-1) rules <- rep(rules,k-1)

  pdims <- sapply(skillLevels,length)
  tvals <- lapply(pdims,effectiveThetas)
  thetas <- do.call("expand.grid",tvals)
  
  pt <- matrix(0,nrow(thetas),k-1)
  for (kk in 1:(k-1)) {
    etheta <- do.call(rules[[kk]],
                      list(thetas,exp(lnAlphas[[kk]]),betas[[kk]]))
    pt[,kk] <- exp(1.7*etheta)
  }
  partialCredit(pt,k,obsLevels)
}

partialCredit <- function (pt,k,obsLevels=NULL) {
  pt <- cbind(pt,1)
  pt <- sweep(pt,1,apply(pt,1,sum),"/")
  probs <- pt[,1:k]
  probs <- ifelse(probs<0,0,probs)
  if (!is.null(obsLevels)) {
    dimnames(probs) <- list(NULL,obsLevels)
  }
  probs
}

calcDPCFrame <-
function (skillLevels, obsLevels, alphas, betas, 
          rules="Compensatory") {
  result <- data.frame(expand.grid(skillLevels),
                       calcDPCTable(skillLevels,paste(obsLevels),
                                   alphas,betas,rules))
  if (is.numeric(obsLevels) ||
      names(result)[length(skillLevels)+1]!=paste(obsLevels[1])) {
    ## R is "helpfully" fixing our numeric labels.  Need to insist.
    names(result) <- c(names(skillLevels),paste(obsLevels))
  }
  result

}
