test_that("mapDPC", {
  pLevels <- list(Skill1=c("High","Med","Low"))
  obsLevels <- c("Full","Partial","None")
  
  trueLnAlphas <- list(log(1),log(.25))
  trueBetas <- list(2, -.5)
  
  priorLnAlphas <- list(log(.5),log(.5))
  priorBetas <- list(1, -1)
  
  truedist <- calcDPCTable(pLevels,obsLevels,trueLnAlphas,trueBetas,
                           rules="Compensatory",link="partialCredit")
  prior <- calcDPCTable(pLevels,obsLevels,priorLnAlphas,priorBetas,
                        rules="Compensatory",link="partialCredit")
  
  post1 <- prior + round(truedist*1000)
  
  map1 <- mapDPC(post1,pLevels,obsLevels,priorLnAlphas,priorBetas,
                 rules="Compensatory",link="partialCredit")
  failed <- FALSE
  if (map1$convergence != 0) {
    testthat::fail(paste("Optimization did not converge:", map1$message))
    failed<-TRUE
  }
  
  postLnAlphas <- map1$lnAlphas
  postBetas <- map1$betas
  fitdist <- calcDPCTable(pLevels,obsLevels,map1$lnAlphas,map1$betas,
                          rules="Compensatory",link="partialCredit")
  ## Tolerance for recovery test.
  tol <- .01
  maxdistdif <- max(abs(fitdist-truedist))
  if (maxdistdif > tol) {
    fail("Posterior and True CPT differ, maximum difference ",maxdistdif)
    failed<-TRUE
  }
  if (any(abs(unlist(postLnAlphas)-unlist(trueLnAlphas))>tol)) {
    fail("Log(alphas) differ by more than tolerance")
    failed<-TRUE
  }
  if (any(abs(unlist(postBetas)-unlist(trueBetas))>tol)) {
    fail("Betas differ by more than tolerance")
    failed<-TRUE
  }
  if (!failed) {
    testthat::succeed("All mapDPC tests passed.")
  }
})
