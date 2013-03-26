### This is a test script for DiBello-Samejima Likelihoods
### It is based on Kentaro Kato's getll.r test script.

### Calculates the effective theta values for a skill variable
### with the argument number of levels
effectiveThetas <- function (nlevels) {
  rev(qnorm((2*(1:nlevels)-1)/(2*nlevels),0,1))
}

### This calculates the DiBello-Samejima likelihood for a number of
### different models.
### skillLevels --- list of labels for the skill level variable.
### obsLevels --- labels for obserable values.
### lnAlphas --- set of log slopes, one for each skill level
### beta --- difficulty (-intercept) parameter
### dinc --- difficulty level increments (for models with more than two levels)
### model --- Function for computing effective theta.  This should have the
### signature   function(thetas,alphas,beta)
calcDSTable <- function (skillLevels, obsLevels, lnAlphas, beta, dinc=0,
                         model="Compensatory") {
  pdims <- sapply(skillLevels,length)
  tvals <- lapply(pdims,effectiveThetas)
  thetas <- do.call("expand.grid",tvals)
  etheta <- do.call(model,list(thetas,exp(lnAlphas),beta))
  #print(etheta)
  
  k <- length(obsLevels)
  if (k > 2) {
    ## dinc parameters are given from highest state to lowest,
    ## but we want to generate data from lowest to highest
    d <- rev(cumsum(c(0,rev(dinc)))) - sum(dinc)/2
  } else {
    d <- 0
  }
  #print(d)
  pt <- 1/(1+exp(outer(-1.7*etheta,d,"+")))
  pt <- cbind(0,pt,1)
  #print(pt)
  probs <- pt[,2:(k+1)]-pt[,1:k]
  dimnames(probs) <- list(NULL,obsLevels)
  #print(data.frame(expand.grid(skillLevels),probs))
  probs
}

calcDSFrame <-
function (skillLevels, obsLevels, lnAlphas, beta, dinc=0,
          model="Compensatory") {
  result <- data.frame(expand.grid(skillLevels),
                       calcDSTable(skillLevels,paste(obsLevels),
                                   lnAlphas,beta,dinc,model))
  if (is.numeric(obsLevels) ||
      names(result)[length(skillLevels)+1]!=paste(obsLevels[1])) {
    ## R is "helpfully" fixing our numeric labels.  Need to insist.
    names(result) <- c(names(skillLevels),paste(obsLevels))
  }
  result

}


eThetaFrame <- function (skillLevels, lnAlphas, beta, model="Compensatory") {
  pdims <- sapply(skillLevels,length)
  tvals <- lapply(pdims,effectiveThetas)
  thetas <- do.call("expand.grid",tvals)
  names(thetas)<- paste(names(skillLevels),"theta",sep=".")
  etheta <- do.call(model,list(thetas,exp(lnAlphas),beta))
  data.frame(expand.grid(skillLevels),thetas, Effective.theta=etheta)
}


### This calculates the DiBello Normal likelihood for a number of
### different models.
### skillLevels --- list of labels for the skill level variable.
### lnAlphas --- set of log slopes, one for each skill level
### obsLevels --- labels for obserable values.
### beta --- difficulty (-intercept) parameter
### lnStd --- log of standard deviation
### model --- Function for computing effective theta.  This should have the
### signature   function(thetas,alphas,beta)
calcDNTable <- function (skillLevels, obsLevels, lnAlphas, beta, std,
                         model="Compensatory") {
  pdims <- sapply(skillLevels,length)
  tvals <- lapply(pdims,effectiveThetas)
  thetas <- do.call("expand.grid",tvals)
  etheta <- do.call(model,list(thetas,exp(lnAlphas),beta))

  k <- length(obsLevels)
  cuts <- qnorm( ((k-1):1)/k)
  #print(cuts)
  pt <- pnorm(outer(-etheta,cuts,"+")/exp(std))
  pt <- cbind(1,pt,0)
  #print(pt)
  probs <- pt[,1:k]-pt[,2:(k+1)]
  dimnames(probs) <- list(NULL,obsLevels)
  #print(data.frame(expand.grid(skillLevels),probs))
  probs
}


calcDNFrame <- function (skillLevels, obsLevels, lnAlphas, beta, std,
                         model="Compensatory") {
  result <- data.frame(expand.grid(skillLevels),
                       calcDNTable(skillLevels,paste(obsLevels),
                                   lnAlphas,beta,std,model))
  if (is.numeric(obsLevels) ||
      names(result)[length(skillLevels)+1]!=paste(obsLevels[1])) {
    ## R is "helpfully" fixing our numeric labels.  Need to insist.
    names(result) <- c(names(skillLevels),paste(obsLevels))
  }
  result
}


### Calculates the effective thetas for the Compensatory distribution.
Compensatory <- function (theta, alphas, beta) {
  nparents <- length(alphas)
  if (ncol(theta) != nparents) {
    stop("Dimension missmatch between theta and alpha.")
  }
  theta <- apply(sweep(theta,2,alphas,"*"),1,sum)
  theta/sqrt(nparents) - beta
}

### Calculates the effective thetas for the Conjunctive combination
### rule. 
Conjunctive <- function (theta, alphas, beta) {
  nparents <- length(alphas)
  if (ncol(theta) != nparents) {
    stop("Dimension missmatch between theta and alpha.")
  }
  theta <- apply(sweep(theta,2,alphas,"*"),1,min)
  theta - beta
}

### Calculates the effective thetas for the Disjunctive combination
### rule. 
Disjunctive <- function (theta, alphas, beta) {
  nparents <- length(alphas)
  if (ncol(theta) != nparents) {
    stop("Dimension missmatch between theta and alpha.")
  }
  theta <- apply(sweep(theta,2,alphas,"*"),1,max)
  theta - beta
}

### Calculates the effective thetas for the OffsetConjunctive
### combination rule.
### This uses a common discrimination, but gives a different beta
### offset to each parent.
OffsetConjunctive <- function (theta, alpha, betas) {
  nparents <- length(betas)
  if (ncol(theta) != nparents) {
    stop("Dimension missmatch between theta and beta.")
  }
  theta <- apply(sweep(theta,2,betas,"-"),1,min)
  alpha*theta
}

### Calculates the effective thetas for the OffsetDisjunctive
### combination rule.
### This uses a common discrimination, but gives a different beta
### offset to each parent.
OffsetDisjunctive <- function (theta, alpha, betas) {
  nparents <- length(betas)
  if (ncol(theta) != nparents) {
    stop("Dimension missmatch between theta and alpha.")
  }
  theta <- apply(sweep(theta,2,betas,"-"),1,max)
  theta*alpha
}

### This function builds up a contingency table for the various combinations
### of parent and child state.
dataTable <- function (data, parents, child, childStates) {
  ncol <- length(childStates)
  t <- table(data[,c(parents,child)])
  matrix(t,ncol=ncol,dimnames=list(NULL,childStates))
}

### This calculates the actual DiBello Samejima log likelihood
calcDSllike <- function (data,parents,skillLevels, child, obsLevels,
                        lnAlphas, beta, dinc=0, 
                        model="Compensatory") {
  ## Reversing parents gets us to the same order as matrixes are
  ## stored in StatShop
  probs <- calcDSTable(rev(skillLevels),obsLevels,rev(lnAlphas),beta,dinc,model)
  #print(probs)
  counts <- dataTable(data,rev(parents),child,obsLevels)
  #print(counts)
  sum(ifelse(counts>0,counts*log(probs),0))
}


### This calculates the actual DiBello Normal log likelihood
calcDNllike <- function (data,parents,skillLevels, child, obsLevels,
                        lnAlphas, beta, std, 
                        model="Compensatory") {
  ## Reversing parents gets us to the same order as matrixes are
  ## stored in StatShop
  probs <- calcDNTable(rev(skillLevels),obsLevels,rev(lnAlphas),beta,std,model)
  #print(probs)
  counts <- dataTable(data,rev(parents),child,obsLevels)
  #print(counts)
  sum(ifelse(counts>0,counts*log(probs),0))
}



