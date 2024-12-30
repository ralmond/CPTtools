## These models interpolate between an expert and novice profile.

calcDDTable <-
function (skillLevels, obsLevels, skillWeights,
          masterProfile, noviceProfile=.5,
          rule="Compensatory") {
  pdims <- sapply(skillLevels,length)
  tvals <- lapply(pdims,effectiveThetas)
  thetas <- do.call("expand.grid",tvals)
  etheta <- do.call(rule,list(thetas,skillWeights,0.0))

  rowWeights <- (etheta-min(etheta))/(max(etheta)-min(etheta))
  tab <- t(sapply(rowWeights,
                   function(w) {
                    w*masterProfile + (1-w)*noviceProfile
                  }))
  dimnames(tab) <- list(NULL,obsLevels)
  tab
}

calcDDFrame <-
function (skillLevels, obsLevels, skillWeights,
          masterProfile, noviceProfile=.5,
          rule="Compensatory") {
  result <-
    data.frame(expand.grid(skillLevels),
               calcDDTable(skillLevels,paste(obsLevels),skillWeights,
                           masterProfile,noviceProfile,rule))
  if (is.numeric(obsLevels)) {
    ## R is "helpfully" fixing our numeric labels.  Need to insist.
    names(result) <- c(names(skillLevels),paste(obsLevels))
  }
  as.CPF(result)
}


  

