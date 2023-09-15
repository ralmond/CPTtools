test_that("calcDPCTable", {
  skill1l <- c("High","Medium","Low") 
  skill2l <- c("High","Medium","Low","LowerYet") 
  correctL <- c("Correct","Incorrect") 
  pcreditL <- c("Full","Partial","None")
  gradeL <- c("A","B","C","D","E") 
  cptCorrect <- calcDPCTable(list(S1=skill1l,S2=skill2l),correctL,
                             log(c(S1=1,S2=.75)),1.0,rule="Compensatory",
                             link="partialCredit")
  cptCorrect2 <- calcDPCTable(list(S1=skill1l,S2=skill2l),correctL,
                              log(c(S1=1,S2=.75)),1.0,rule="Compensatory",
                              link="gradedResponse")
  cptCorrect1 <- calcDSTable(list(S1=skill1l,S2=skill2l),correctL,
                             log(c(S1=1,S2=.75)),1.0,rule="Compensatory")
  expect_equal(dim(cptCorrect),c(12L,2L))
  expect_equal(colnames(cptCorrect),correctL)
  expect_true(all (abs(cptCorrect2-cptCorrect1) <.001))
  expect_true(all (abs(cptCorrect-cptCorrect1) <.001))
  expect_equal(rowSums(cptCorrect),rep(1,12))
  
    
})

test_that("calcDPCFrame", {
  skill1l <- c("High","Medium","Low") 
  skill2l <- c("High","Medium","Low","LowerYet") 
  correctL <- c("Correct","Incorrect") 
  pcreditL <- c("Full","Partial","None")
  gradeL <- c("A","B","C","D","E")

  cpfTheta <- calcDPCFrame(list(),skill1l,numeric(),0,rule="Compensatory",
                           link="normalLink",linkScale=.5)
  expect_equal(dim(cpfTheta),c(1L,3L))
  expect_equal(colnames(cpfTheta),skill1l)
  expect_equal(cpfTheta[1,1],cpfTheta[1,3])
  expect_equal(rowSums(cpfTheta),1)

  cptGrade <- calcDPCFrame(list(S1=skill1l,S2=skill2l),gradeL,
                             log(c(S1=1,S2=.75)),1.0,rule="Compensatory",
                             link="partialCredit")
  expect_equal(dim(cptGrade),c(12L,7L))
  expect_equal(colnames(cptGrade),c("S1","S2",gradeL))
  expect_equal(as.character(cptGrade$S1),rep(skill1l,4))
  expect_equal(as.character(cptGrade$S2),rep(skill2l,each=3))
  expect_equal(rowSums(cptGrade[,gradeL]),rep(1,12L))
  
})

test_that("calcDPCTable GR", {
  skill1 <- c("High","Med","Low")
  skill2 <- c("High","Med","Low")
  troph <- c("Gold","Silver","None")
  cpt1 <- calcDPCTable(list(skill1),troph,list(log(.5),log(1)),list(1,-1),
               link="gradedResponse")
  cpt2 <- calcDPCTable(list(skill1),troph,list(log(.5),log(.01)),list(1,-1),
               link="gradedResponse")
  cpt3 <- calcDPCTable(list(skill1),troph,list(log(.5),log(.5)),list(1,1),
               link="gradedResponse")
  expect_equal(cpt1[,1],cpt2[,1])
  expect_equal(cpt1[,1],cpt3[,1])
  expect_equal(cpt3[,2],rep(0,3))
  expect_lt(cpt1[3,2],cpt1[3,3])
  expect_gt(cpt2[3,2],cpt2[3,3])
  
  cpt4 <- calcDPCTable(list(skill1,skill2),troph,log(c(.8,.8)),list(1,-1),
               rules="Compensatory", link="gradedResponse")
  cpt4a <- calcDPCTable(list(skill1,skill2),troph,log(c(.8,.01)),list(1,-1),
                       rules="Compensatory", link="gradedResponse")
  expect_gt(cpt4[1,1]-cpt4[4,1],.1)
  expect_lt(cpt4a[1,1]-cpt4a[4,1],.1)
  
  skill1l <- c("High","Medium","Low") 
  skill2l <- c("High","Medium","Low","LowerYet") 
  correctL <- c("Correct","Incorrect") 
  pcreditL <- c("Full","Partial","None")
  gradeL <- c("A","B","C","D","E")
  ## Graded Response Model, typically uses different difficulties
  cptGraded <- calcDPCTable(list(S1=skill1l),gradeL,
                            log(1),betas=list(A=2,B=1,C=0,D=-1),
                            rule="Compensatory",link="gradedResponse")
  expect_equal(rowSums(cptGraded),rep(1,3))
  expect_true(isDecreasing(cptGraded[,"A"]))
  expect_true(isIncreasing(cptGraded[,"E"]))

  ## Need to be careful when using different slopes (or non-increasing
  ## difficulties) with graded response link as curves may cross.
  
  cptCross <- calcDPCTable(list(S1=skill1l),pcreditL,
                           log(1),betas=list(full=-1,partial=1),
                           rule="Compensatory",link="gradedResponse")
  expect_true(all(abs(cptCross[,"Partial"])<.001))
  
})

test_that("calcDPCTable PC", {
  skill1 <- c("High","Med","Low")
  skill2 <- c("High","Med","Low")
  troph <- c("Gold","Silver","None")
  pctab1 <- calcDPCTable(list(skill1),troph,list(log(1),log(1)),list(0,0))
  pctab1a <- calcDPCTable(list(skill1),troph,list(log(1),log(1)),list(1,0))
  pctab1b <- calcDPCTable(list(skill1),troph,list(log(1),log(1)),list(0,1))
  expect_true(all(pctab1[,"Gold"]>pctab1a[,"Gold"]))
  expect_true(all(pctab1[,"None"]<pctab1b[,"None"]))
  
  pctab2a <- calcDPCTable(list(skill1),troph,list(log(1),log(.01)),list(0,0))
  pctab2b <- calcDPCTable(list(skill1),troph,list(log(.01),log(1)),list(0,0))
  expect_equal(pctab2a[,"Silver"],pctab2a[,"None"],tolerance=.05)
  expect_equal(pctab2b[,"Gold"],pctab2b[,"Silver"],tolerance=.05)
  
  skillm <- c("Master","Non-master")
  pcreditL <- c("Full","Partial","None")
  
  ## Complex model, different rules for different levels
  cptPC2 <- calcDPCTable(list(S1=skillm,S2=skillm),pcreditL,
                         list(full=log(1),partial=log(c(S1=1,S2=.75))),
                         betas=list(full=c(0,999),partial=1.0),
                         rule=list("OffsetDisjunctive","Compensatory"))
  cptPC2Partial <- cptPC2[,2:3]
  cptPC2Partial[,1] <- cptPC2[,2]+cptPC2[,1] 
  cptPC2Full <- cptPC2[,1:2]/rowSums(cptPC2[,1:2])
  expect_true(isIncreasing(cptPC2Partial[c(1,2,4),2]))
  expect_true(isIncreasing(cptPC2Partial[c(1,3,4),2]))
  expect_true(all(cptPC2Full[1,1]>cptPC2Full[2,1]))
  expect_equal(cptPC2Full[1:2,],cptPC2Full[3:4,],tolerance=.001)

  ## Partial credit link is somewhat different
  cptPC5a <- calcDPCTable(list(S1=skill1),pcreditL,
                          log(1),betas=1,
                          rule="Compensatory",link="partialCredit")
  expect_true(all(cptPC5a[,3]>.01))
})

test_that("calcDPCTable PC local-Q", {
  skill1l <- c("High","Medium","Low") 
  skill2l <- c("High","Medium","Low","LowerYet") 
  correctL <- c("Correct","Incorrect") 
  pcreditL <- c("Full","Partial","None")
  gradeL <- c("A","B","C","D","E")
  cptPC1 <- calcDPCTable(list(S1=skill1l,S2=skill2l),pcreditL,
                         lnAlphas=log(1),
                         betas=list(full=c(S1=0,S2=999),partial=c(S2=999,S2=0)),
                         rule="OffsetDisjunctive")
  ##Variant using Q-matrix
  cptPC1a <- calcDPCTable(list(S1=skill1l,S2=skill2l),pcreditL,
                          lnAlphas=log(1),
                          betas=list(full=c(S1=0),partial=c(S2=0)),
                          Q=matrix(c(TRUE,FALSE,FALSE,TRUE),2,2),
                          rule="OffsetDisjunctive")
  expect_equal(cptPC1,cptPC1a,tolerance=.0001)

})

test_that("calcDPCTable normalLink no parents", {
  skill1l <- c("High","Medium","Low") 
  skill2l <- c("High","Medium","Low","LowerYet") 
  correctL <- c("Correct","Incorrect") 
  pcreditL <- c("Full","Partial","None")
  gradeL <- c("A","B","C","D","E")
  ## Test for no parent case
  cptTheta0 <- calcDPCTable(list(),skill1l,numeric(),0,rule="Compensatory",
                           link="normalLink",linkScale=1)
  cptTheta1 <- calcDPCTable(list(),skill1l,numeric(),1,rule="Compensatory",
                           link="normalLink",linkScale=1)
  cptTheta1m <- calcDPCTable(list(),skill1l,numeric(),-1,rule="Compensatory",
                           link="normalLink",linkScale=1)
  cptThetaX <- calcDPCTable(list(),skill1l,numeric(),0,rule="Compensatory",
                           link="normalLink",linkScale=.5)
  expect_equal(as.numeric(cptTheta0),rep(1/3,3))
  expect_true(isIncreasing(cptTheta1))
  expect_true(isDecreasing(cptTheta1m))
  expect_gt(cptThetaX[1,2],cptThetaX[1,1])
  expect_gt(cptThetaX[1,2],cptThetaX[1,3])
  
})
test_that("calcDPCTable normalLink parents", {
  skill1 <- c("High","Med","Low")
  skill2 <- c("High","Med","Low")
  troph <- c("Gold","Silver","None")
  cptTheta3 <- calcDPCTable(list(S1=skill1),troph,0,0,rule="Compensatory",
                            link="normalLink",linkScale=.3)
  cptTheta5 <- calcDPCTable(list(S1=skill1),troph,0,0,rule="Compensatory",
                            link="normalLink",linkScale=.5)
  expect_gt(gkGamma(cptTheta3),gkGamma(cptTheta5))
  
})

