test_that("calcDSTable", {
  skill1l <- c("High","Medium","Low") 
  skill2l <- c("High","Medium","Low","LowerYet") 
  correctL <- c("Correct","Incorrect") 
  gradeL <- c("A","B","C","D","E") 
  
  cptCorrect <- calcDSTable(list(S1=skill1l,S2=skill2l),correctL,
                            log(c(S1=1,S2=.75)),1.0,rule="Conjunctive")
  
  cptCorrect1 <- calcDPCTable(list(S1=skill1l,S2=skill2l),correctL,
                              log(c(S1=1,S2=.75)),1.0,rule="Conjunctive",
                              link=gradedResponse)
  expect_equal(cptCorrect,cptCorrect1)  
  
  cptGraded <- calcDSTable(list(S1=skill1l),gradeL, 0.0, 0.0, dinc=c(.3,.4,.3))
  cptGraded1 <- calcDPCTable(list(S1=skill1l),gradeL, 0.0, 
                             betas=as.list(rev(cumsum(c(0,.3,.4,.3)))-.5),
                             link=gradedResponse)
  expect_equal(cptGraded,cptGraded1)
  
})
test_that("calcDSFrame", {
  skill1l <- c("High","Medium","Low") 
  skill2l <- c("High","Medium","Low","LowerYet") 
  correctL <- c("Correct","Incorrect") 
  gradeL <- c("A","B","C","D","E") 
  
  cptGraded <- calcDSFrame(list(S1=skill1l,S2=skill2l),gradeL, log(c(1.2,.8)), 0.0, dinc=c(.3,.4,.3))
  expect_equal(dim(cptGraded),c(12L,7L))
  expect_equal(names(cptGraded),c("S1","S2",gradeL))
  expect_equal(as.character(cptGraded$S1),rep(skill1l,4))
  expect_equal(as.character(cptGraded$S2),rep(skill2l,each=3))
  expect_equal(rowSums(numericPart(cptGraded)),rep(1,12L))
})

test_that("calcDSllike", {
  skill3 <- c("High","Med","Low") 
  correctL <- c("Yes","No") 
  
  x <- read.table(system.file("testFiles", "300twothetas10items.cas", 
                              package="CPTtools"),as.is=TRUE,header=TRUE)

  x[,"theta0"] <- ordered(x[,"theta0"],skill3)
  x[,"theta1"] <- ordered(x[,"theta1"],skill3)
  x[,"comp"] <- ordered(x[,"comp"],correctL)
  x[,"conj"] <- ordered(x[,"conj"],correctL)
  x[,"oconj"] <- ordered(x[,"oconj"],correctL)
  x[,"disj"] <- ordered(x[,"disj"],correctL)
  x[,"odis"] <- ordered(x[,"odis"],correctL)
  
  like1 <- calcDSllike(x,c("theta0","theta1"),
                      list(theta0=skill3,theta1=skill3), 
                      "comp", correctL,
                      c(theta0=-.25,theta1=-.25),.1,rule="Compensatory")
  like1a <- calcDSllike(x,c("theta0","theta1"),
                       list(theta0=skill3,theta1=skill3), 
                       "comp", correctL,
                       c(theta0=-.25,theta1=-.25),1.1,rule="Compensatory")
  expect_gt(like1,like1a)
  
})
test_that("calcDNTable", {
  skillL <- c("High","Med","Low")
  tab1 <- calcDNTable(list(),skillL,numeric(),0,0)
  tab2 <- calcDNTable(list(theta0=skillL),skillL,log(.6),0,log(.8))
  expect_equal(dim(tab1),c(1L,3L))
  expect_equal(as.numeric(tab1),rep(1/3,3))
  expect_equal(dim(tab2),c(3L,3L))
  expect_equal(as.numeric(tab2[1,1]),as.numeric(tab2[3,3]))
  expect_equal(as.numeric(tab2[2,1]),as.numeric(tab2[2,3]))
  expect_equal(as.numeric(tab2[3,1]),as.numeric(tab2[1,3]))
  expect_equal(as.numeric(tab2[1,2]),as.numeric(tab2[3,2]))
  expect_equal(rowSums(tab2),rep(1.0,3))
})

test_that("calcDNFrame", {
  skillL <- c("High","Med","Low")
  tab1 <- calcDNFrame(list(),skillL,numeric(),0,0)
  tab2 <- calcDNFrame(list(theta0=skillL),skillL,log(.6),0,log(.8))
  expect_equal(dim(tab1),c(1L,3L))
  expect_equal(colnames(tab1),skillL)
  expect_equal(dim(tab2),c(3L,4L))
  expect_equal(as.character(tab2$theta0),skillL)
  expect_equal(colnames(tab2),c("theta0",skillL))
})
test_that("calcDNllike", {
  skill3 <- c("High","Med","Low") 
  correctL <- c("Yes","No") 
  
  x <- read.table(system.file("testFiles", "300twothetas10items.cas", 
                              package="CPTtools"),as.is=TRUE,header=TRUE)
  
  x[,"theta0"] <- ordered(x[,"theta0"],skill3)
  x[,"theta1"] <- ordered(x[,"theta1"],skill3)
  ll1 <- calcDNllike(x,character(),list(),"theta0",skill3,numeric(),0,0)
  ll2 <- calcDNllike(x,"theta0",list(theta0=skill3),"theta1",skill3,log(.6),0,log(.8))
  ll1a <- calcDNllike(x,character(),list(),"theta0",skill3,numeric(),0,log(.5))
  ll2a <- calcDNllike(x,"theta0",list(theta0=skill3),"theta1",skill3,log(.6),1,log(.8))
  expect_gt(ll1,ll1a)
  expect_gt(ll2,ll2a)
})

       