test_that("gkLambda", {
  
   grei <- matrix(c(1,0,0,0,0,0,
                   23,27,41,28,13,0,
                   7,49,85,58,13,1,
                   0,7,82,219,64,0,
                   0,0,0,72,144,6,
                   0,0,0,1,44,15),6,6)
  testthat::expect_equal(gkLambda(t(grei)),0.19,tolerance=.02)
  testthat::expect_equal(gkLambda(t(grei),"Linear"),0.28,tolerance=.02)
  testthat::expect_equal(gkLambda(t(grei),"Quadratic"),0.35,tolerance=.02)
  
  read <- matrix(c(0.207,0.029,0,0.04,0.445,0.025,0,0.025,0.229),3,3,
                 dimnames=list(estimated=c("Advanced","Intermediate","Novice"),
                               actual=c("Advanced","Intermediate","Novice")))
  testthat::expect_equal(gkLambda(read),.762475,tolerance = .00001)
  
})

test_that("fcKappa", {
  
  grei <- matrix(c(1,0,0,0,0,0,
                   23,27,41,28,13,0,
                   7,49,85,58,13,1,
                   0,7,82,219,64,0,
                   0,0,0,72,144,6,
                   0,0,0,1,44,15),6,6)
  testthat::expect_equal(fcKappa(grei),0.31,tolerance=.02)
  testthat::expect_equal(fcKappa(grei,"Linear"),0.50,tolerance=.02)
  testthat::expect_equal(fcKappa(grei,"Quadratic"),0.67,tolerance=.02)
  
  read <- matrix(c(0.207,0.029,0,0.04,0.445,0.025,0,0.025,0.229),3,3,
                 dimnames=list(estimated=c("Advanced","Intermediate","Novice"),
                               actual=c("Advanced","Intermediate","Novice")))
  testthat::expect_equal(fcKappa(read),.8088974, tolerance=.00001)
  testthat::expect_equal(fcKappa(read,"Linear"),.8403507, tolerance=.00001)
  testthat::expect_equal(fcKappa(read,"Quadratic"),.8798887, tolerance=.00001)
})

test_that("accuracy", {
  grei <- matrix(c(1,0,0,0,0,0,
                   23,27,41,28,13,0,
                   7,49,85,58,13,1,
                   0,7,82,219,64,0,
                   0,0,0,72,144,6,
                   0,0,0,1,44,15),6,6)
  testthat::expect_equal(accuracy(grei),.491,tolerance=.002)
  testthat::expect_equal(accuracy(grei,"Linear"),.8814,tolerance=.002)
  testthat::expect_equal(accuracy(grei,"Quadratic"),.96844,tolerance=.002)
})


test_that("gkGamma", {
  nab <- matrix(c(8,0,0,5,8,4,3,1,14,3,0,4),3,4)
  expect_equal(gkGamma(nab),.6122,tolerance=.0001)
  
})
