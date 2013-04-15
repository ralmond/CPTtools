
skill1 <- c("High","Med","Low")
skill2 <- c("High","Med","Low")
troph <- c("Gold","Silver","None")

calcDPCFrame(list(skill1),troph,list(log(.5),log(1)),list(1,-1),
             link="gradedResponse")
calcDPCFrame(list(skill1),troph,list(log(.5),log(.01)),list(1,-1),
             link="gradedResponse")
calcDPCFrame(list(skill1),troph,list(log(.5),log(.5)),list(1,1),
             link="gradedResponse")


calcDPCFrame(list(skill1),troph,list(log(.5),log(1)),list(1,-1))
calcDPCFrame(list(skill1),troph,list(log(.5),log(1)),list(-1,-1))
calcDPCFrame(list(skill1),troph,list(log(.5),log(.01)),list(1,-1))
calcDPCFrame(list(skill1),troph,list(log(.01),log(.5)),list(0,-1))
calcDPCFrame(list(skill1),troph,list(log(.5),log(.5)),list(1,1))
calcDPCFrame(list(skill1),troph,list(log(.5),log(.01)),list(1,0))


cbind(expand.grid(1:3,1:3),
      theta=OffsetConjunctive(expand.grid(1:3,1:3),-1,c(0,.5)))
cbind(expand.grid(1:3,1:3),
      theta=OffsetDisjunctive(expand.grid(1:3,1:3),-1,c(0,.5)))

testFit <- function(pLevels,obsLevels,trueLnAlphas,trueBetas,
                    priorLnAlphas,priorBetas, weights=1000,
                    rules="Compensatory", link="partialCredit",
                    stoponerror=FALSE, tol=.001) {

  cat("Testing:  rules=",paste(rules,collapse=", "),"; link=",link,"\n")
  cat("Prior parameters: ln(alphas) = ",paste(priorLnAlphas,collape=", "),
      "; betas=",paste(priorBetas,collapse=", "),"\n")
  cat("True parameters: ln(alphas) = ",paste(trueLnAlphas,collape=", "),
      "; betas=",paste(trueBetas,collapse=", "),"\n")
  
  error <- FALSE
  truedist <- calcDPCTable(pLevels,obsLevels,trueLnAlphas,trueBetas,
                           rules=rules,link=link)
  prior <- calcDPCTable(pLevels,obsLevels,priorLnAlphas,priorBetas,
                        rules=rules,link=link)

  post1 <- prior + round(sweep(truedist,1,weights,"*"))
  map1 <- mapDPC(post1,pLevels,obsLevels,priorLnAlphas,priorBetas,rules,link)

  if (map1$convergence != 0) {
    warning("Optimization did not converge:", map1$message)
  }

  postLnAlphas <- map1$lnAlphas
  postBetas <- map1$betas
  cat("Estimated parameters: ln(alphas) = ",paste(postLnAlphas,collape=", "),
      "; betas=",paste(postBetas,collapse=", "),"\n")
  fitdist <- calcDPCTable(list(skill1),troph,map1$lnAlphas,map1$betas)

  maxdistdif <- max(abs(fitdist-truedist))
  if (maxdistdif > tol) {
    error <- TRUE
    if (stoponerror) {
      stop("Posteror and True CPT differ, maximum difference ",maxdistdif)
    } else {
      warning("Posteror and True CPT differ, maximum difference ",maxdistdif)
    }
  }
  if (any(abs(unlist(postLnAlphas)-unlist(trueLnAlphas))>tol)) {
    error <- TRUE
    if (stoponerror) {
      stop("Log(alphas) differ by more than tolerance")
    } else {
      warning("Log(alphas) differ by more than tolerance")
    }
  }
  if (any(abs(unlist(postBetas)-unlist(trueBetas))>tol)) {
    error <- TRUE
    if (stoponerror) {
      stop("Betas differ by more than tolerance")
    } else {
      warning("Betas differ by more than tolerance")
    }
  }

  invisible(list(error=error,pseudoData=post1,
                 truedist=truedist,fitdist=fitdist,
                 trueLnAlphas=trueLnAlphas,fitLnAlphas=postLnAlphas,
                 trueBetas=trueBetas,fitBetas=postBetas,map=map1))
}
  

tf1 <- testFit(list(skill1),troph,list(log(1),log(.25)),list(2,-.5),
               list(log(.5),log(.5)),list(1,-1),tol=.001)
