############################################
## Table Building Functions


buildMarginTab <-
function(data,stateNames,skillNames,reverse=TRUE) {
  margincols <- grep(paste("margin\\.",skillNames,sep=""),names(data))
  grandmeans <- apply(data[,margincols],2,mean)
  meanmat <- matrix(grandmeans,nrow=length(stateNames))
  dimnames(meanmat) <- list(stateNames,skillNames)
  if (reverse) 
    meanmat[rev(stateNames),]
  else
    meanmat
}

marginTab <-
function(datarow,stateNames,skillNames,reverse=TRUE) {
  margincols <- grep(paste("margin\\.",skillNames,sep=""),names(data))
  grandmeans <- data[,margincols]
  meanmat <- matrix(grandmeans,nrow=length(stateNames))
  dimnames(meanmat) <- list(stateNames,skillNames)
  if (reverse) 
    meanmat[rev(stateNames),]
  else
    meanmat
}

buildFactorTab <-
function(data,fact,stateNames,skillNames,reverse=TRUE) {
  margincols <- grep(paste("margin\\.",skillNames,sep=""),names(data))
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
function(data,fact1,fact2,stateNames,skillNames,reverse=TRUE) {
  margincols <- grep(paste("margin\\.",skillNames,sep=""),names(data))
  subdata <- data[,margincols]
  names(subdata) <- stateNames
  result <- array(0,c(length(levels(fact1)),length(levels(fact2)),
                      length(stateNames)))
  groups1 <- split(subdata,fact1)
  fact2groups <- split(fact2,fact1)
  for (i in seq(along=groups1)) {
    gdata <- groups1[[i]]
    gfact2 <- fact2groups[[i]]
    ggroups <- split(gdata,gfact2)
    tab <-sapply(ggroups,function(x) apply(x,2,sum))
    if (reverse) 
      result[i,,] <- t(tab[rev(stateNames),])
    else
      result[i,,] <-t(tab)
  }
  result
}



