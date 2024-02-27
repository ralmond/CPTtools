test_that("rescaleTable", {
  X2.ptf <- data.frame(Theta=c("Expert","Novice"),
                       correct=c(4,2),
                       incorrect=c(2,4))
  
  X2.t99 <- rescaleTable(X2.ptf,99/6)  #Reweight to effective samples size of 99
  X2.t31 <- rescaleTable(X2.ptf,c(3,1))  #Weight expert prior 3 times more than
  #novice prior.
  expect_equal(X2.t99[1,"correct"],66)
  expect_equal(rowSums(numericPart(X2.t99)),rep(99,2))
  expect_equal(X2.t31[2,"correct"],2)
  expect_equal(rowSums(numericPart(X2.t31)),c(18,6))
      
  #Unconditional table
  Theta.ptf <- data.frame(Expert=3,Novice=3)
  Theta.t100 <- rescaleTable(Theta.ptf,100/6)  #Reweight to effective
  expect_equal(Theta.t100[1,],data.frame(Expert=50,Novice=50))  
})

test_that("normalizeTable", {
  X2.ptf <- data.frame(Theta=c("Expert","Novice"),
                       correct=c(4,2),
                       incorrect=c(2,4))
  X2.dtf <- normalizeTable(X2.ptf)
  expect_equal(colnames(X2.dtf),c("Theta","correct","incorrect"))
  expect_equal(X2.dtf$Theta,c("Expert","Novice"))
  expect_equal(rowSums(numericPart(X2.dtf)),c(1,1))
  
  Theta.ptf <- data.frame(Expert=3,Novice=3)
  Theta.dtf <- normalizeTable(Theta.ptf)
  expect_equal(Theta.dtf,data.frame(Expert=.5,Novice=.5))
  
})

test_that("scaleTable", {
  c1 <- matrix(c(70,20,10,10,20,70),nrow=2,byrow=TRUE,
               dimnames=list(NULL,c("H","M","L")))
  s1 <- matrix(c(7,2,1,10,100,1,2,7,10,100),nrow=2,byrow=TRUE,
               dimnames=list(NULL,c("H","M","L","Sum","Scale")))
  
  ## 1 row matrixes need special handling (c1[1,] is a vector not a matrix)
  c1r1 <- matrix(c1[1,],nrow=1,dimnames=list(NULL,c("H","M","L")))
  s1r1 <- matrix(s1[1,],nrow=1,dimnames=list(NULL,c("H","M","L","Sum","Scale")))
  expect_equal(scaleTable(s1),c1)
  expect_equal(scaleTable(s1[1,]),c1[1,])
  expect_equal(scaleTable(s1r1),c1r1)
  expect_equal(scaleTable(c1),c1)
  expect_equal(scaleTable(c1[1,]),c1[1,])
  expect_equal(scaleTable(c1r1),c1r1)
  
  
})

test_that("numericPart factorPart", {
  name <-as.factor(c("Shahrazad", "Marguerite"))
  height <- c(34, 36)
  weight <- c(28, 26)
  twins <- data.frame(name=name,height=height, weight=weight)
  hw <- cbind(height,weight)
  expect_equal(numericPart(twins),hw)
  expect_equal(numericPart(as.data.frame(hw)),hw)
  expect_equal(factorPart(twins),data.frame(name))
})

test_that("numericPart matrix", {
  mat <- matrix(1:12,3,4)
  expect_equal(numericPart(mat),mat)
})

test_that("getTableParents/States", {
  X2.ptf <- data.frame(Theta=factor(c("Expert","Novice")),
                       correct=c(4,2),
                       incorrect=c(2,4))
  #Unconditional table
  Theta.ptf <- data.frame(Expert=3,Novice=3)
  
  expect_equal(getTableStates(X2.ptf), c("correct","incorrect"))
  expect_equal(getTableStates(Theta.ptf), c("Expert","Novice"))
  expect_equal(getTableParents(X2.ptf), "Theta")
  expect_equal(getTableParents(Theta.ptf), character(0))
})


