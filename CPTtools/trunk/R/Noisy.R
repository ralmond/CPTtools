## These are functions related to noisy-and/ noisy-or &c.


calcNoisyAndTable <-
  function (skillLevels,obsLevels=c("True","False"),
            noSlip=1,bypass=rep(0,length(skillLevels)),
            thresholds = sapply(skillLevels,function(states) states[1])) {
  pdims <- sapply(skillLevels,length)
  ## Logical values indicating if each value has met the threshold.
  svals <- vector("list",length(skillLevels))
  for (j in 1:length(skillLevels)) {
    svals[[j]] <- rev(cumsum(rev(skillLevels[[j]]==thresholds[j])))
  }
  ss <- as.matrix(do.call("expand.grid",svals))
  rr <- sweep(ss,2,bypass,function(s,r) ifelse(s,1,r))
  probs <- noSlip*apply(rr,1,prod)
  probs <- cbind(probs,1-probs)
  colnames(probs)<-obsLevels
  probs
}

calcNoisyAndFrame <-
  function (skillLevels,obsLevels=c("True","False"),
            noSlip=1,bypass=rep(0,length(skillLevels)),
            thresholds = sapply(skillLevels,function(states) states[1])) {
  result <- data.frame(expand.grid(skillLevels),
                       calcNoisyAndTable(skillLevels,paste(obsLevels),
                                         noSlip,bypass,thresholds))
  if (is.numeric(obsLevels) ||
      names(result)[length(skillLevels)+1]!=paste(obsLevels[1])) {
    ## R is "helpfully" fixing our numeric labels.  Need to insist.
    names(result) <- c(names(skillLevels),paste(obsLevels))
  }
  result
}

calcNoisyOrTable <- function (skillLevels,obsLevels=c("True","False"),
                                noGuess=1,suppression=rep(0,length(skillLevels)),
                                thresholds = sapply(skillLevels,function(states) states[1]))
{
  pdims <- sapply(skillLevels,length)
  ## Logical values indicating if each value has met the threshold.
  svals <- vector("list",length(skillLevels))
  for (j in 1:length(skillLevels)) {
    svals[[j]] <- rev(cumsum(rev(skillLevels[[j]]==thresholds[j])))
  }
  ss <- as.matrix(do.call("expand.grid",svals))
  qq <- sweep(ss,2,suppression,function(s,q) ifelse(s,q,1))
  probs <-   1-noGuess*apply(qq,1,prod)
  probs <- cbind(probs,1-probs)
  colnames(probs)<-obsLevels
  probs
}

calcNoisyOrFrame <-
  function (skillLevels,obsLevels=c("True","False"),
            noGuess=1,suppression=rep(0,length(skillLevels)),
            thresholds = sapply(skillLevels,function(states) states[1])) {
  result <- data.frame(expand.grid(skillLevels),
                       calcNoisyOrTable(skillLevels,paste(obsLevels),
                                         noGuess,suppression,thresholds))
  if (is.numeric(obsLevels) ||
      names(result)[length(skillLevels)+1]!=paste(obsLevels[1])) {
    ## R is "helpfully" fixing our numeric labels.  Need to insist.
    names(result) <- c(names(skillLevels),paste(obsLevels))
  }
  result
}


