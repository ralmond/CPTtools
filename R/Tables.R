############################################
## Table Building Functions


buildMarginTab <-
function(data,stateNames,skillNames,reverse=TRUE,stem="P",sep=".") {
  cnames <- paste(stem,skillNames,"",sep=sep)
  margincols <- sapply(cnames,function(n) grep(n,names(data),fixed=TRUE))
  grandmeans <- apply(data[,as.vector(margincols)],2,mean)
  meanmat <- matrix(grandmeans,nrow=length(stateNames))
  dimnames(meanmat) <- list(stateNames,skillNames)
  if (reverse)
    meanmat[rev(stateNames),]
  else
    meanmat
}

marginTab <-
function(datarow,stateNames,skillNames,reverse=TRUE,stem="P",sep=".") {
  cnames <- paste(stem,skillNames,"",sep=sep)
  marginmatch <- sapply(cnames,function(n) any(grepl(n,names(datarow))))
  if (any(!marginmatch)) {
    warning(sprintf("Did not find any entries starting with %s",
                 paste(cnames[!marginmatch], collapse=", ")))
  }
  csnames <- unlist(lapply(cnames,function (cn) paste(cn,stateNames,sep=sep)))
  means <- as.numeric(datarow[csnames])
  meanmat <- matrix(means,nrow=length(stateNames))
  dimnames(meanmat) <- list(stateNames,skillNames)
  if (reverse)
    meanmat[rev(stateNames),]
  else
    meanmat
}

buildFactorTab <-
function(data,fact,stateNames,skillName,reverse=TRUE,stem="P",sep=".") {
  cname <- paste(stem,skillName,"",sep=sep)
  margincols <- grep(cname,names(data),value=TRUE)
  subdata <- data[,margincols]
  names(subdata) <- stateNames
  groups <- split(subdata,fact)
  groupmeans <-sapply(groups,function(x) apply(x,2,mean))
  groupmeans
 if (reverse)
   groupmeans[rev(stateNames),]
 else
   groupmeans
}

build2FactorTab <-
function(data,fact1,fact2,stateNames,skillName,reverse=TRUE,
         stem="P",sep=".") {
  cname <- paste(stem,skillName,".",sep=sep)
  margincols <- grep(cname,names(data),value=TRUE)
  subdata <- data[,margincols]
  names(subdata) <- stateNames
  result <- array(0,c(length(levels(fact1)),length(levels(fact2)),
                      length(stateNames)),
                  dimnames=list(levels(fact1),levels(fact2),stateNames))
  groups1 <- split(subdata,fact1)
  fact2groups <- split(fact2,fact1)
  for (i in seq(along=groups1)) {
    gdata <- groups1[[i]]
    gfact2 <- fact2groups[[i]]
    ggroups <- split(gdata,gfact2)
    tab <-sapply(ggroups,function(x) apply(x,2,mean))
    if (reverse)
      result[i,,] <- t(tab[rev(stateNames),])
    else
      result[i,,] <-t(tab)
  }
  result
}

#############################################################################
##  Conditional Probability Frames versus Tables
############################################################################

## There are two reasonable ways to store a conditional probility
## table.  The first is a multi-dimensional array with each dimension
## corresponding to a parent variable, and the last dimension
## corresponding to the child variable.  Call this method the CPA.

## The second way is to make a data frame where the first couple of
## variables are factor variables that indicate the state of the
## parent variables.  The last couple of variables are real variables
## that indicate the probabilities associated with the states of the
## child variable.  Call that the CPF

is.CPF <- function (x) {
  is(x,"CPF")
}

as.CPF <- function(x) {
  if (is.array(x)) {
    if (length(dim(x)) > 1L) {
      dnames <- dimnames(x)
      npar <- length(dnames) -1L
      parfactors <- do.call(expand.grid,dnames[1L:npar])
      probs <- data.frame(matrix(x,nrow(parfactors)))
      ## This counts on the fact that "." is not a legal name char in Netica
      names(probs) <-
        paste(names(dnames)[npar+1L],dnames[[npar+1L]],sep=".")
      result <- data.frame(parfactors,probs)
      class(result) <- c("CPF",class(result))
      return (result)
    } else {
      probs <- data.frame(matrix(x,1L))
      names(probs) <- names(x)
      class(probs) <- c("CPF",class(probs))
      return (probs)
    }
  } else if (is.data.frame(x)) {
    ## Resort columns so factors are in front.
    facts <- sapply(x,is.factor)
    result <- data.frame(x[facts],x[!facts])
    if (!is.CPF(x)) {
      class(x) <- c("CPF",class(x))
    }
    return(x)
  } else {
    stop("Don't know how to turn a ",class(x), " into a CPF.")
  }
}

is.CPA <- function (x) {
  is(x,"CPA")
}

as.CPA <- function (x) {
  if (is.array(x)) {
    if (!is(x,"CPA")) {
      class(x) <- c("CPA",class(x))
    }
    if (is.null(dimnames(x)) || any(sapply(dimnames(x),is.null))) {
      warning("Array being coerced to CPA does not have dimnames.")
    }
    if (is.null(names(dimnames(x))) || any(nchar(names(dimnames(x)))==0)) {
      warning("Array being coerced to CPA does not have names(dimnames).")
    }
    return(x) ## Hope this really is a CPA
  } else  if (is.data.frame(x)) {
    ##First set up the diminsions
    facts <- sapply(x,is.factor)
    pnames <- lapply(x[facts],levels)
    npar <- length(pnames)
    names(pnames) <- names(x)[facts]
    ## Try to create state names and var names for dependent varaible
    ## by splitting at "."
    cnames <- strsplit(names(x)[!facts],".",fixed=TRUE)
    nstates <- length(cnames)
    vname <- cnames[[1L]][1L]
    cnames <- lapply(cnames, function(x) x[length(x)])
    if (vname == cnames[1L]) vname <- "Prob"
    dnames <- c(pnames,list(cnames))
    names(dnames)[npar+1L] <- vname
    dims <- sapply(dnames,length)
    result <- array(NA,dims,dnames)
    probs <- as.matrix(x[,!facts])
    configs <- do.call("cbind",lapply(x[,facts],as.integer))
    for (i in 1L:nrow(x)) {
      sel <- matrix(configs[i,],nstates,npar,byrow=TRUE)
      result[cbind(sel,1L:nstates)] <- probs[i,]
    }
    class(result) <- c("CPA",class(result))
    return(result)
  } else {
    stop("Don't know how to turn a ",class(x), " into a CPA.")
  }
}

######################################################################
### Normalization
######################################################################

normalize <- function(cpt) {
  UseMethod("normalize",cpt)
}

normalize.array <- function(cpt) {
  normalize(as.CPA(cpt))
}

normalize.data.frame <- function (cpt) {
  normalize(as.CPF(cpt))
}

normalize.table <- function (cpt) {
  normalize(as.CPA(cpt))
}

normalize.default <- function (cpt) {
  if (!is.numeric(cpt)) {
    stop("Can only normalize CPAs, CPFs and numeric objects.")
  }
  return (cpt/sum(cpt))
}

normalize.matrix <- function (cpt) {
  if (!is.numeric(cpt)) {
    stop("Can only normalize CPAs, CPFs and numeric objects.")
  }
  sweep(cpt,1L,apply(cpt,1L,sum),"/")
}

normalize.CPA <- function (cpt) {
  ndim <- length(dim(cpt))
  if (ndim <= 1L) return(cpt/sum(cpt))
  ndim <- ndim-1L
  sweep(cpt,1L:ndim,apply(cpt,1L:ndim,sum),"/")
}

normalize.CPF <- function (cpt) {
  probs <- sapply(cpt,is.numeric)
  cpt[probs] <- sweep(cpt[probs],1L,apply(cpt[probs],1L,sum),"/")
  cpt
}

### This function builds up a contingency table for the various combinations
### of parent and child state.
dataTable <- function (data, parents, child, childStates) {
  ncol <- length(childStates)
  t <- table(data[,c(parents,child)])
  matrix(t,ncol=ncol,dimnames=list(NULL,childStates))
}

