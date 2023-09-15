test_that("effectiveThetas", {
  expect_equal(effectiveThetas(3),c(.9674,0,-.9674),
               tolerance=.0002)
})


test_that("eThetaFrame", {
  skill <- c("H","M","L")
  esk <- effectiveThetas(3)
  etf <- eThetaFrame(list(S1=skill,S2=skill),
                     -log(c(S1=2,S2=2))/2,0)
  expect_equal(dim(etf),c(9L,5L))
  expect_equal(as.character(etf[,1]),rep(skill,3))
  expect_equal(as.character(etf[,2]),rep(skill,each=3))
  expect_equal(etf[,3],rep(esk,3))
  expect_equal(etf[,4],rep(esk,each=3))
  expect_equal(etf[,5],c(esk/2+esk[1]/2,esk/2,esk/2+esk[3]/2))
})

test_that("Compensatory", {
  thetas <- data.frame(S1=rep(c(1,0,-1),3),S2=rep(c(1,0,-1),each=3))
  expect_equal(Compensatory(thetas,sqrt(c(2,2)),0),
               c(2,1,0,1,0,-1,0,-1,-2))
  expect_equal(Compensatory(thetas,sqrt(c(2,2)),1),
               c(1,0,-1,0,-1,-2,-1,-2,-3))
  expect_equal(Compensatory(thetas,sqrt(c(8,2)),0),
               c(3,1,-1,2,0,-2,1,-1,-3))
  ## Zero parent case
  expect_equal(Compensatory(matrix(NA,1,0),numeric(),-1),1)
  })
test_that("Conjunctive", {
  thetas <- data.frame(S1=rep(c(1,0,-1),3),S2=rep(c(1,0,-1),each=3))
  expect_equal(Conjunctive(thetas,c(1,1),0),
               c(1,0,-1,0,0,-1,-1,-1,-1))
  expect_equal(Conjunctive(matrix(NA,1,0),numeric(),-2),2)
})

test_that("Disjunctive", {
  thetas <- data.frame(S1=rep(c(1,0,-1),3),S2=rep(c(1,0,-1),each=3))
  expect_equal(Disjunctive(thetas,c(1,1),0),
               c(1,1,1,1,0,0,1,0,-1))
  expect_equal(Disjunctive(matrix(NA,1,0),numeric(),-3),3)
})

test_that("OffsetConjuctive", {
  thetas <- data.frame(S1=rep(c(1,0,-1),3),S2=rep(c(1,0,-1),each=3))
  expect_equal(OffsetConjunctive(thetas,1,c(0,0)),
               c(1,0,-1,0,0,-1,-1,-1,-1))
  expect_equal(OffsetConjunctive(thetas,1,c(1,0)),
               c(0,-1,-2,0,-1,-2,-1,-1,-2))
  expect_equal(OffsetConjunctive(thetas,2,c(0,0)),
               2*c(1,0,-1,0,0,-1,-1,-1,-1))
  
})
test_that("OffsetDisjunctive", {
  thetas <- data.frame(S1=rep(c(1,0,-1),3),S2=rep(c(1,0,-1),each=3))
  expect_equal(OffsetDisjunctive(thetas,1,c(0,0)),
               c(1,1,1,1,0,0,1,0,-1))
  
})
test_that("Offset Rules", {
  orules <- getOffsetRules()
  expect_equal(orules,c("OffsetConjunctive","OffsetDisjunctive"))
  expect_false(isOffsetRule("Compensatory"))
  expect_false(isOffsetRule(Compensatory))
  expect_false(isOffsetRule(Conjunctive))
  expect_false(isOffsetRule(Disjunctive))
  expect_true(isOffsetRule("OffsetConjunctive"))
  expect_true(isOffsetRule(OffsetConjunctive))
  expect_true(isOffsetRule(OffsetDisjunctive))
  setOffsetRules(c("Conjunctive","Disjunctive"))
  withr::defer(setOffsetRules(orules))
  expect_true(isOffsetRule(Conjunctive))
  expect_false(isOffsetRule(OffsetConjunctive))
})

test_that("default alphas", {
  expect_equal(defaultAlphas(Compensatory,c("S1","S2")),
               c(S1=1,S2=1))
  expect_equal(defaultAlphas(OffsetConjunctive,c("S1","S2")),
               1)
  expect_equal(
    defaultAlphas("Compensatory",c("S1","S2"),c("Yes","Maybe","No")),
    list(Yes=c(S1=1,S2=1),Maybe=c(S1=1,S2=1)))
  
  expect_equal(
    defaultAlphas("Compensatory",c("S1","S2"),c("Yes","Maybe","No"),
                  "gradedResponse"),
    c(S1=1,S2=1))
  
  expect_equal(
    defaultAlphas("Compensatory",c("S1","S2"),c("Yes","Maybe","No"),
                  "normalLink"),
    c(S1=1,S2=1))

  expect_equal(
    defaultAlphas("OffsetConjunctive",c("S1","S2"),
                c("Yes","Maybe","No"),"gradedResponse"),
    1)
  
  
})

test_that("default betas", {
  expect_equal(defaultBetas(Compensatory,c("S1","S2")),
               0)
  expect_equal(defaultBetas(OffsetConjunctive,c("S1","S2")),
               c(S1=0,S2=0))
  expect_equal(
    defaultBetas("Compensatory",c("S1","S2"),c("Yes","Maybe","No")),
    list(Yes=0, Maybe=0))

  expect_equal(  
    defaultBetas("Compensatory",c("S1","S2"),c("Yes","Maybe","No"),
                 "gradedResponse"),
    list(Yes=0, Maybe=0))
  
  expect_equal(
    defaultBetas("Compensatory",c("S1","S2"),c("Yes","Maybe","No"),
               "normalLink"),
    0)
  
  expect_equal(
    defaultBetas("OffsetConjunctive",c("S1","S2"),
               c("Yes","Maybe","No"),"gradedResponse"),
    list(Yes=c(S1=0,S2=0),Maybe=c(S1=0,S2=0)))
  
})


