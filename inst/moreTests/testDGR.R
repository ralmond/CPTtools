library(CPTtools)

testFit <- function(pLevels,obsLevels,trueLnAlphas,trueBetas,
                    priorLnAlphas,priorBetas, weights=1000,
                    rules="Compensatory", link="partialCredit",
                    stoponerror=FALSE, tol=.001,control=list()) {
  
  testname <- paste("Testing:  rules=",paste(rules,collapse=", "),"; link=",link)
  error <- FALSE
  truedist <- calcDPCTable(pLevels,obsLevels,trueLnAlphas,trueBetas,
                           rules=rules,link=link)
  prior <- calcDPCTable(pLevels,obsLevels,priorLnAlphas,priorBetas,
                        rules=rules,link=link)
  
  post1 <- prior + round(sweep(truedist,1,weights,"*"))
  map1 <- mapDPC(post1,pLevels,obsLevels,priorLnAlphas,priorBetas,rules,link,control=control)
  
  if (map1$convergence != 0) {
    fail(paste("Optimization did not converge:", map1$message))
  }
  
  postLnAlphas <- map1$lnAlphas
  postBetas <- map1$betas
  
  fitdist <- calcDPCTable(pLevels,obsLevels,map1$lnAlphas,map1$betas,rules,link)
  
  maxdistdif <- max(abs(fitdist-truedist))
  if (maxdistdif > tol) {
    error <- TRUE
    print(paste("Posterior and True CPT differ, maximum difference ",maxdistdif))
  } else {
    print("Posterior matches True CPT")
  }
  if (any(abs(unlist(postLnAlphas)-unlist(trueLnAlphas))>tol)) {
    error <- TRUE
    print("Log(alphas) differ by more than tolerance")
  } else {
    print("Log(alphas) differ by less than tolerance")
  }
  if (any(abs(unlist(postBetas)-unlist(trueBetas))>tol)) {
    error <- TRUE
    print("Betas differ by more than tolerance")
  } else {
    print("Betas differ by less than tolerance")
  }
  
  invisible(list(error=error,pseudoData=post1,
                 truedist=truedist,fitdist=fitdist,
                 trueLnAlphas=trueLnAlphas,fitLnAlphas=postLnAlphas,
                 trueBetas=trueBetas,fitBetas=postBetas,map=map1))
}

skill1 <- c("High","Med","Low")
skill2 <- c("High","Med","Low")
troph <- c("Gold","Silver","None")

tf1 <- testFit(list(skill1),troph,list(log(1),log(.25)),list(2,-.5),
               list(log(.5),log(.5)),list(1,-1),tol=.001,
               control=list(trace=2))





