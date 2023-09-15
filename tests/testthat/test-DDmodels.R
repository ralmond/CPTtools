test_that("calcDDtable", {
  skill1l <- c("High","Medium","Low") 
  skill2l <- c("High","Low")
  option5L <- c("A","B","C","D","E") 
  
 
  paramT <- calcDDTable(list(S1=skill1l,S2=skill2l), c("A","B"),
                        c(S1=2,S2=1), masterProfile=c(6,1),
                        noviceProfile = 0)
  ## Expected scale factors
  ew <- (2*rep(effectiveThetas(3),2)+rep(effectiveThetas(2),each=3))/sqrt(2)
  ews <- (ew-min(ew))/(max(ew)-min(ew))
  
  expect_equal(paramT[,2],ews)
  expect_equal(paramT[,1],6*ews)
})

test_that("calcDDFrame", {
  skill1l <- c("High","Medium","Low") 
  skill2l <- c("High","Low")
  option5L <- c("A","B","C","D","E") 
  
  ## Expert responses
  eProfile <- c(A=7,B=15,C=3,D=0,E=0)+.5
  ## Expected scale factors
  ew <- (2*rep(effectiveThetas(3),2)+rep(effectiveThetas(2),each=3))/sqrt(2)
  ews <- (ew-min(ew))/(max(ew)-min(ew))
  
  paramF <- calcDDFrame(list(S1=skill1l,S2=skill2l), option5L,
                        c(S1=2,S2=1), masterProfile=5*eProfile,
                        noviceProfile=2)
  expect_equal(as.character(paramF$S1), rep(skill1l,2))
  expect_equal(as.character(paramF$S2), rep(skill2l,each=3))
  expect_equal(paramF$A, 5*eProfile["A"]*ews + 2*(1-ews))
  expect_equal(paramF$B, 5*eProfile["B"]*ews + 2*(1-ews))
  expect_equal(paramF$C, 5*eProfile["C"]*ews + 2*(1-ews))
  expect_equal(paramF$D, 5*eProfile["D"]*ews + 2*(1-ews))
  expect_equal(paramF$E, 5*eProfile["E"]*ews + 2*(1-ews))
  
})
