### This is a test script for DiBello-Samejima Likelihoods
### It is based on Kentaro Kato's getll.r test script.


### This calculates the DiBello-Samejima likelihood for a number of
### different combination rules.
### skillLevels --- list of labels for the skill level variable.
### obsLevels --- labels for obserable values.
### lnAlphas --- set of log slopes, one for each skill level
### beta --- difficulty (-intercept) parameter
### dinc --- difficulty level increments (for child variables with more than two levels)
### rule --- Function for computing effective theta.  This should have the
### signature   function(thetas,alphas,beta)
calcDSTable <- function (skillLevels, obsLevels, lnAlphas, beta, dinc=0,
                         rule="Compensatory") {
  pdims <- sapply(skillLevels,length)
  tvals <- lapply(pdims,effectiveThetas)
  thetas <- do.call("expand.grid",tvals)
  etheta <- do.call(rule,list(thetas,exp(lnAlphas),beta))
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
  pt <- 1/(1+exp(-1.7*outer(etheta,d,"-")))
  pt <- cbind(0,pt,1)
  #print(pt)
  probs <- pt[,2:(k+1)]-pt[,1:k]
  dimnames(probs) <- list(NULL,obsLevels)
  #print(data.frame(expand.grid(skillLevels),probs))
  probs
}

calcDSFrame <-
function (skillLevels, obsLevels, lnAlphas, beta, dinc=0,
          rule="Compensatory") {
  result <- data.frame(expand.grid(skillLevels),
                       calcDSTable(skillLevels,paste(obsLevels),
                                   lnAlphas,beta,dinc,rule))
  if (is.numeric(obsLevels) ||
      names(result)[length(skillLevels)+1]!=paste(obsLevels[1])) {
    ## R is "helpfully" fixing our numeric labels.  Need to insist.
    names(result) <- c(names(skillLevels),paste(obsLevels))
  }
  as.CPF(result)

}





### This calculates the DiBello Normal likelihood for a number of
### different combination rules.
### skillLevels --- list of labels for the skill level variable.
### lnAlphas --- set of log slopes, one for each skill level
### obsLevels --- labels for obserable values.
### beta --- difficulty (-intercept) parameter
### lnStd --- log of standard deviation
### rule --- Function for computing effective theta.  This should have the
### signature   function(thetas,alphas,beta)
calcDNTable <- function (skillLevels, obsLevels, lnAlphas, beta, std,
                         rule="Compensatory") {
  pdims <- sapply(skillLevels,length)
  tvals <- lapply(pdims,effectiveThetas)
  thetas <- do.call("expand.grid",tvals)
  etheta <- do.call(rule,list(thetas,exp(lnAlphas),beta))

  k <- length(obsLevels)
  cuts <- qnorm( ((k-1):1)/k)
  #print(cuts)
  pt <- pnorm(outer(-etheta,cuts,"+")/exp(std))
  pt <- cbind(1,pt,0)
  #print(pt)
  probs <- pt[,1:k,drop=FALSE]-pt[,2:(k+1),drop=FALSE]
  dimnames(probs) <- list(NULL,obsLevels)
  #print(data.frame(expand.grid(skillLevels),probs))
  probs
}


calcDNFrame <- function (skillLevels, obsLevels, lnAlphas, beta, std,
                         rule="Compensatory") {
  result<- calcDNTable(skillLevels,paste(obsLevels),
                         lnAlphas,beta,std,rule)
  if (length(skillLevels) > 0L) {
    result <- data.frame(expand.grid(skillLevels),result)
  } else {
    result <- data.frame(result)
  }
  if (is.numeric(obsLevels) ||
      names(result)[length(skillLevels)+1]!=paste(obsLevels[1])) {
    ## R is "helpfully" fixing our numeric labels.  Need to insist.
    names(result) <- c(names(skillLevels),paste(obsLevels))
  }
  as.CPF(result)
}


### This calculates the actual DiBello Samejima log likelihood
calcDSllike <- function (data,parents,skillLevels, child, obsLevels,
                        lnAlphas, beta, dinc=0,
                        rule="Compensatory") {
  ## Reversing parents gets us to the same order as matrixes are
  ## stored in StatShop
  probs <- calcDSTable(rev(skillLevels),obsLevels,rev(lnAlphas),beta,dinc,rule)
  #print(probs)
  counts <- dataTable(data,rev(parents),child,obsLevels)
  #print(counts)
  sum(ifelse(counts>0,counts*log(probs),0))
}


### This calculates the actual DiBello Normal log likelihood
calcDNllike <- function (data,parents,skillLevels, child, obsLevels,
                        lnAlphas, beta, std,
                        rule="Compensatory") {
  ## Reversing parents gets us to the same order as matrixes are
  ## stored in StatShop
  probs <- calcDNTable(rev(skillLevels),obsLevels,rev(lnAlphas),beta,std,rule)
  #print(probs)
  counts <- dataTable(data,rev(parents),child,obsLevels)
  #print(counts)
  sum(ifelse(counts>0,counts*log(probs),0))
}



