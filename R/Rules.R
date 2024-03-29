

### Calculates the effective theta values for a skill variable
### with the argument number of levels
effectiveThetas <- function (nlevels) {
  rev(qnorm((2*(1:nlevels)-1)/(2*nlevels),0,1))
}

eThetaFrame <- function (skillLevels, lnAlphas, beta, rule="Compensatory") {
  pdims <- sapply(skillLevels,length)
  tvals <- lapply(pdims,effectiveThetas)
  thetas <- do.call("expand.grid",tvals)
  names(thetas)<- paste(names(skillLevels),"theta",sep=".")
  etheta <- do.call(rule,list(thetas,exp(lnAlphas),beta))
  data.frame(expand.grid(skillLevels),thetas, Effective.theta=etheta)
}


### Calculates the effective thetas for the Compensatory distribution.
Compensatory <- function (theta, alphas, beta) {
  if (ncol(theta)==0L || length(alphas)==0L) {
    return(-beta)    # No parent case
  }
  nparents <- length(alphas)
  if (ncol(theta) != nparents) {
    stop("Dimension missmatch between theta and alpha.")
  }
  if (nparents==0) {
    -beta
  } else {
    theta <- apply(sweep(theta,2,alphas,"*"),1,sum)
    theta/sqrt(nparents) - beta
  }
}

### Calculates the effective thetas for the Conjunctive combination
### rule.
Conjunctive <- function (theta, alphas, beta) {
  if (ncol(theta)==0L || length(alphas)==0L) {
    return(-beta)    # No parent case
  }
  nparents <- length(alphas)
  if (nparents>0 && ncol(theta) != nparents) {
    stop("Dimension missmatch between theta and alpha.")
  }
  if (nparents==0) {
    -beta
  } else {
    theta <- apply(sweep(theta,2,alphas,"*"),1,min)
    theta - beta
  }
}

### Calculates the effective thetas for the Disjunctive combination
### rule.
Disjunctive <- function (theta, alphas, beta) {
  if (nrow(theta)==0L || length(alphas)==0L) {
    return(-beta)    # No parent case
  }
  nparents <- length(alphas)
  if (ncol(theta) != nparents) {
    stop("Dimension missmatch between theta and alpha.")
  }
  if (nparents==0) {
    -beta
  } else {
    theta <- apply(sweep(theta,2,alphas,"*"),1,max)
    theta - beta
  }
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

### List of rules which should be considered offset functions
OffsetRules <- c("OffsetConjunctive","OffsetDisjunctive")
getOffsetRules<- function() { OffsetRules}
setOffsetRules<- function(newval) {
  assignInMyNamespace("OffsetRules",newval)
}

isOffsetRule <- function (rl) {
  if (is.character(rl))
    return(rl %in% OffsetRules)
  else if (is.function(rl)) {
    rl <- deparse(substitute(rl))
    return (rl %in% OffsetRules)
  }
  else if (is.list(rl)) {
    sapply(rl,isOffsetRule)
  }
  return(FALSE)
}

## TODO:  Need to deal with case where rules is a list.
defaultAlphas <- function (rule,pnames,states=c("Yes","No"),
                           link="partialCredit") {
  if (is.function(rule)) rule <- deparse(substitute(rule))
  if (is.function(link)) link <- deparse(substitute(link))
  if (isOffsetRule(rule)) alphas <- 1
  else {
    alphas <- rep(1,length(pnames))
    names(alphas) <- pnames
  }
  if (length(states)>2L && link=="partialCredit") {
    states <- states[-length(states)]
    alphas <- lapply(states, function(s) alphas)
    names(alphas) <- states
  }
  alphas
}

defaultBetas <- function (rule,pnames,states=c("Yes","No"),
                          link=partialCredit) {
  if (is.function(rule)) rule <- deparse(substitute(rule))
  if (is.function(link)) link <- deparse(substitute(link))
  if (!isOffsetRule(rule)) betas <- 0
  else {
    betas <- rep(0,length(pnames))
    names(betas) <- pnames
  }
  if (length(states)>2L && link!="normalLink") {
    states <- states[-length(states)]
    betas <- lapply(states, function(s) betas)
    names(betas) <- states
  }
  
  betas
}




