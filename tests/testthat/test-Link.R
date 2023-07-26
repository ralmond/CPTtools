
test_that("normalLink", {
  expect_true(all.equal(normalLink(matrix(0,1,2), 1, c("Low", "Med", "High")),
                        matrix(1/3,1,3),check.attributes=FALSE))
  
})

test_that("gradedResponse", {

  pn2 <- exp(-1.7*2)/(1+exp(-1.7*2))
  pn1 <- exp(-1.7)/(1+exp(-1.7))
  pp1 <- exp(1.7)/(1+exp(1.7))
  pp2 <- exp(1.7*2)/(1+exp(1.7*2))
  gr1 <- gradedResponse(matrix(c(-1,0,1),3,1))
  expect_equal(dim(gr1),c(3L,2L))
  expect_equal(rowSums(gr1),rep(1,3))
  expect_equal(gr1[1,],c(pn1,pp1))
  expect_equal(gr1[2,],c(.5,.5))
  expect_equal(gr1[3,],c(pp1,pn1))
  
  gr2 <- gradedResponse(matrix(c(1,0,-1,2:0),3,2))
  expect_equal(dim(gr2),c(3L,3L))
  expect_equal(rowSums(gr2),rep(1,3))
  expect_equal(gr2[1,],c(pp1,pp2-pp1,1-pp2))
  expect_equal(gr2[2,],c(.5,pp1-.5,1-pp1))
  expect_equal(gr2[3,],c(pn1,.5-pn1,.5))
  
  ## Test for non-monotonic difficulties.
  
  gr2a <- gradedResponse(matrix(c(1,0,-1,0,-1,-2),3,2))
  expect_equal(dim(gr2a),c(3L,3L))
  expect_equal(rowSums(gr2a),rep(1,3))
  expect_equal(gr2a[1,],c(pp1,0,1-pp1))
  expect_equal(gr2a[2,],c(.5,0,.5))
  expect_equal(gr2a[3,],c(pn1,0,1-pn1))

  
})

test_that("partialCredit", {
  
  pn1 <- exp(-1.7)/(1+exp(-1.7))
  pp1 <- exp(1.7)/(1+exp(1.7))
  pc1 <- partialCredit(matrix(c(-1,0,1),3,1))
  expect_equal(dim(pc1),c(3L,2L))
  expect_equal(rowSums(pc1),rep(1,3))
  expect_equal(pc1[1,],c(pn1,pp1))
  expect_equal(pc1[2,],c(.5,.5))
  expect_equal(pc1[3,],c(pp1,pn1))
  
  pc2 <- partialCredit(matrix(c(-1,0,1),3,2))
  expect_equal(dim(pc2),c(3L,3L))
  expect_equal(rowSums(pc2),rep(1,3))
  pp <- c(pn1*pn1,pn1*pp1,pp1*pp1)
  pp <- pp/sum(pp)
  expect_equal(pc2[1,],pp)
  expect_equal(pc2[2,],rep(1/3,3))
  expect_equal(pc2[3,],rev(pp))
  
})
