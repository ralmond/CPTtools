test_that("OCP", {
  nn <- c(30,15,20,35)
  pi <- c("+"=.15,"-"=.85)
  grouplabs <- c(rep("-",3),"+")
  x <- c("(0,0)"=7,"(0,1)"=4,"(1,0)"=2,"(1,1)"=31)
  fits <- function()
  OCP (x,nn,grouplabs,pi,c("-","+"),ylim=c(0,1), reflty=c(2,4),
       main="Data that fit the model")
  vdiffr::expect_doppelganger("ocp fits",fits)
  
  x1 <- c("(0,0)"=7,"(0,1)"=4,"(1,0)"=11,"(1,1)"=31)
  notfit <- function()
  OCP (x1,nn,grouplabs,pi,c("-","+"),ylim=c(0,1), reflty=c(2,4),
       main="Data that don't fit the model")
  vdiffr::expect_doppelganger("ocp not fit",notfit)
  
})

test_that("OCP2", {
  nn <- c(30,15,20,35)
  pi <- c("+"=.15,"-"=.85)
  grouplabs <- c(rep("-",3),"+")
  x <- c("(0,0)"=7,"(0,1)"=4,"(1,0)"=2,"(1,1)"=31)
  nnn <- c("(0,0,0)"=20,"(0,0,1)"=10,
           "(0,1,0)"=10,"(0,1,0)"=5,
           "(1,0,0)"=10,"(1,0,1)"=10,
           "(1,1,1)"=10,"(1,1,1)"=25)
  xx <- c("(0,0,0)"=5,"(0,0,1)"=2,
          "(0,1,0)"=2,"(0,1,1)"=2,
          "(1,0,0)"=2,"(1,0,1)"=0,
          "(1,1,0)"=9,"(1,1,1)"=21)
  grouplabs1 <- rep(grouplabs,each=2)
  
  irrelevant <- function()
  OCP2 (xx,nnn,grouplabs1,pi,c("-","+"),ylim=c(0,1), reflty=c(2,4),
        setlabs=c("Low Skill3","High Skill3"),setat=-.8,
        main="Data for which Skill 3 is irrelevant")
  vdiffr::expect_doppelganger("skill 3 irrelanvt",irrelevant)
  
  xx1 <- c("(0,0,0)"=2,"(0,0,1)"=5,
           "(0,1,0)"=1,"(0,1,1)"=3,
           "(1,0,0)"=0,"(1,0,1)"=2,
           "(1,1,0)"=5,"(1,1,1)"=24)
  relevant <- function()
  OCP2 (xx1,nnn,grouplabs1,pi,c("-","+"),ylim=c(0,1), reflty=c(2,4),
        setlabs=c("Low Skill3","High Skill3"),setat=-.8,
        main="Data for which Skill 3 is relevant")
  vdiffr::expect_doppelganger("Skill 3 relevant",relevant)
})

test_that("OCP.CPF", {

  ## Random numbers appear different on different platforms
  ## So we will grab this from a file instead.
  load(testthat::test_path("OCPdat.RData"))  
  #set.seed(1223344567) # So plots are consistent.
  # skill1l <- c("High","Medium","Low") 
  # skill2l <- c("High","Medium","Low","LowerYet") 
  # correctL <- c("Correct","Incorrect") 
  # pcreditL <- c("Full","Partial","None")
  # gradeL <- c("A","B","C","D","E") 

  
  cpfTheta <- calcDPCFrame(list(),skill1l,numeric(),0,rule="Compensatory",
                           link="normalLink",linkScale=.5)
  
  ## Compatible data  
  # datTheta <- cpfTheta ## Copy to get the shape
  # datTheta[1,] <- rmultinom(1,25,cpfTheta[1,])
  
  vdiffr::expect_doppelganger("ocp cpf no par compatible",
                              OCP.CPF(datTheta,cpfTheta))
  
  ## Incompatible data
  # datThetaX <- cpfTheta ## Copy to get the shape
  # datThetaX[1,] <- rmultinom(1,100,c(.05,.45,.5))
  
 vdiffr::expect_doppelganger("ocp cpf no par incompatible",
                             OCP.CPF(datThetaX,cpfTheta))
 vdiffr::expect_doppelganger("ocp2 cpf no par",
              OCP2.CPF(datTheta,datThetaX,cpfTheta))
  
  cptComp <- calcDPCFrame(list(S2=skill2l,S1=skill1l),correctL,
                          lnAlphas=log(c(1.2,.8)), betas=0,
                          rule="Compensatory")
  
  cptConj <- calcDPCFrame(list(S2=skill2l,S1=skill1l),correctL,
                          lnAlphas=log(c(1.2,.8)), betas=0,
                          rule="Conjunctive")
  
  # datComp <- cptComp
  # for (i in 1:nrow(datComp))
  #   ## Each row has a random sample size.
  #   datComp[i,3:4] <- rmultinom(1,rnbinom(1,mu=15,size=1),cptComp[i,3:4])
  # 
  # datConj <- cptConj
  # for (i in 1:nrow(datConj))
  #   ## Each row has a random sample size.
  #   datConj[i,3:4] <- rmultinom(1,rnbinom(1,mu=15,size=1),cptConj[i,3:4])
  
  ## Compatible
  vdiffr::expect_doppelganger("ocp cpf binary compatible",
                              OCP.CPF(datConj,cptConj))
  ## Incompatible
  vdiffr::expect_doppelganger("ocp cpf binary incompatible",
                              OCP.CPF(datComp,cptConj))
  vdiffr::expect_doppelganger("ocp2 cpf binary",
              OCP2.CPF(datConj,datComp,cptConj))
  
  cptPC1 <- calcDPCFrame(list(S1=skill1l,S2=skill2l),pcreditL,
                         lnAlphas=log(1),
                         betas=list(full=c(S1=0,S2=999),partial=c(S2=999,S2=0)),
                         rule="OffsetDisjunctive")
  cptPC2 <- calcDPCFrame(list(S1=skill1l,S2=skill2l),pcreditL,
                         lnAlphas=log(1),
                         betas=list(full=c(S1=0,S2=999),partial=c(S2=1,S2=0)),
                         rule="OffsetDisjunctive")
  
  
  # datPC1 <- cptPC1
  # for (i in 1:nrow(datPC1))
  #   ## Each row has a random sample size.
  #   datPC1[i,3:5] <- rmultinom(1,rnbinom(1,mu=25,size=1),cptPC1[i,3:5])
  # 
  # datPC2 <- cptPC2
  # for (i in 1:nrow(datPC2))
  #   ## Each row has a random sample size.
  #   datPC2[i,3:5] <- rmultinom(1,rnbinom(1,mu=25,size=1),cptPC2[i,3:5])
  
  ## Compatible
  vdiffr::expect_doppelganger("ocp cpf trinary compatible",
                              OCP.CPF(datPC1,cptPC1))
  ## Incompatible
  vdiffr::expect_doppelganger("ocp cpf trinary incompatible",
                              OCP.CPF(datPC2,cptPC1))
  vdiffr::expect_doppelganger("ocp2 cpf trinary",
                              OCP2.CPF(datPC1,datPC2,cptPC1))
  
})

test_that("cptChi2", {
  ## Random numbers appear different on different platforms
  ## So we will grab this from a file instead.
  load(testthat::test_path("OCPdat.RData"))  
  cpfTheta <- calcDPCFrame(list(),skill1l,numeric(),0,rule="Compensatory",
                           link="normalLink",linkScale=.5)
  ## Compatible data  
  x2 <- cptChi2(datTheta,cpfTheta)
  testthat::expect_lt(as.numeric(x2),qchisq(.95,attr(x2,"d.f.")))
  
  ## Incompatible data
  x2x <- cptChi2(datThetaX,cpfTheta)
  testthat::expect_gt(as.numeric(x2x),qchisq(.95,attr(x2x,"d.f.")))
  
  cptComp <- calcDPCFrame(list(S2=skill2l,S1=skill1l),correctL,
                          lnAlphas=log(c(1.2,.8)), betas=0,
                          rule="Compensatory")
  
  cptConj <- calcDPCFrame(list(S2=skill2l,S1=skill1l),correctL,
                          lnAlphas=log(c(1.2,.8)), betas=0,
                          rule="Conjunctive")
  ## Compatible
  x2 <- cptChi2(datConj,cptConj)
  testthat::expect_lt(as.numeric(x2),qchisq(.95,attr(x2,"d.f.")))

  ## Incompatible
  x2x <- cptChi2(datComp,cptConj)
  testthat::expect_gt(as.numeric(x2x),qchisq(.95,attr(x2x,"d.f.")))
  
  cptPC1 <- calcDPCFrame(list(S1=skill1l,S2=skill2l),pcreditL,
                         lnAlphas=log(1),
                         betas=list(full=c(S1=0,S2=999),partial=c(S2=999,S2=0)),
                         rule="OffsetDisjunctive")
  cptPC2 <- calcDPCFrame(list(S1=skill1l,S2=skill2l),pcreditL,
                         lnAlphas=log(1),
                         betas=list(full=c(S1=0,S2=999),partial=c(S2=1,S2=0)),
                         rule="OffsetDisjunctive")
  ## Compatible
  x2 <- cptChi2(datPC1,cptPC1)
  testthat::expect_lt(as.numeric(x2),qchisq(.95,attr(x2,"d.f.")))

  ## Incompatible
  x2x <- cptChi2(datPC2,cptPC1)
  testthat::expect_gt(as.numeric(x2x),qchisq(.95,attr(x2x,"d.f.")))
  
})

