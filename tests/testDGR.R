library(CPTtools)

skill1 <- c("High","Med","Low")
skill2 <- c("High","Med","Low")
troph <- c("Gold","Silver","None")


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
  fitdist <- calcDPCTable(pLevels,obsLevels,map1$lnAlphas,map1$betas,rules,link)

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



### Test of offset conjuctive model

counts <- structure(c(67.1614032700581, 35.1614084302491, 16.1614089795132,
 29.7062485312078, 40.7062511148065, 31.7062490922995, 16.5770465477286,
 19.4229688399514, 15.4229695261955, 5.83861035705343, 1.83861064323719,
 1.83861049630488, 8.29377110852289, 17.2937717733771, 10.293771232885,
 6.42296993295514, 28.5770449883257, 46.5770504711631), .Dim = structure(c(3L,
 3L, 2L), .Names = c("theta1", "theta0", "odis")), class = c("CPA",
 "array"), .Dimnames = structure(list(theta1 = structure(c("Low",
 "Med", "High"), .Names = c("Low", "Med", "High")), theta0 = structure(c("Low",
 "Med", "High"), .Names = c("Low", "Med", "High")), odis = structure(c("Yes",
 "No"), .Names = c("Yes", "No"))), .Names = c("theta1", "theta0", "odis")))

ps <- list(theta1=c("Low","Med","High"),theta9=c("Low","Med","High"))
ns <- c("Yes","No")
la <- 0
bs <- c(theta1=.5,theta0=-.5)


### Test of mapDPC for an error
mapodis <- mapDPC(counts,ps,ns,la,bs,"OffsetDisjunctive","partialCredit",
                  NULL,TRUE,control=list(maxit=5))

### Theta 1 (normal link function)
pt1 <-structure(c(68.316553949222, 30.246630638977, 13.1343228568749,
                  29.5491546230287, 51.5067694985964, 40.5491513632223,
                  8.13432352485097, 34.2466303800047, 57.3165562974809),
                .Dim = structure(c(3L, 3L), .Names = c("theta0", "theta1")),
                class = c("CPA", "matrix"),
.Dimnames = structure(list(theta0 = structure(c("Low", "Med", "High"),
                                              .Names = c("Low", "Med", "High")),
                           theta1 = structure(c("Low", "Med", "High"),
                                              .Names = c("Low", "Med", "High"))),
                      .Names = c("theta0", "theta1")))
pt1a <- structure(c(192.316583512236, 84.24664079792, 37.13434305954,
                    81.5491718753547, 145.50679168569, 114.549172058553,
                    22.134342781261, 96.2466493535649, 159.316583386035),
                  .Dim = structure(c(3L, 3L),
                                   .Names = c("theta0", "theta1")),
                  class = c("CPA", "matrix"),
                  .Dimnames =
                    structure(list(theta0 = structure(c("Low", "Med", "High"),
                                                      .Names = c("Low", "Med", "High")),
                                   theta1 = structure(c("Low", "Med", "High"),
                                                      .Names = c("Low", "Med", "High"))),
                              .Names = c("theta0", "theta1")))

sl1 <- list(theta0=c("Low","Med","High"))
ol1 <- c("Low","Med","High")
la1 <- -0.5108256
bs1 <- 0
rules1 <- list(c("Compensatory"),c("Compensatory"))
lnk1 <- "normalLink"
lnks1 <- .8

mapth1 <- mapDPC(pt1,sl1,ol1,la1,bs1,rules1,lnk1,lnks1,control=list(maxit=5))
stopifnot(mapth1$linkScale>0)


## Theta 0 update
pt0 <- structure(c(98.6666731497735, 108.666677439104, 103.6666799287),
                 class = c("CPA", "array"),
                 .Dim = structure(3L, .Names = "theta0"),
                 .Dimnames = structure(list(theta0 =
                                              structure(c("Low", "Med", "High"),
                                                        .Names = c("Low", "Med", "High"))),
                                       .Names = "theta0"))
sl0 <- list()
ol0 <- c("Low","Med","High")
la0 <- numeric()
bs0 <- 0
rules0 <- "Compensatory"
lnk0 <- "normalLink"
lnks0 <- 1

mapth0 <- mapDPC(pt0,sl0,ol0,la0,bs0,rules0,lnk0,lnks0,control=list(maxit=5))
stopifnot(mapth0$linkScale>0)


