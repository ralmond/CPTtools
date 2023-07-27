test_that("calcNoisyAndTable", {
  and <- calcNoisyAndTable(list(c("True","False"),c("True","False")),
                           c("Right","Wrong"))
  testthat::expect_equal(and,cbind(Right=c(1,0,0,0),Wrong=c(0,1,1,1)))
  
  
  ## DINA, logical-and except that is allows for a small chance of slipping.
  dina <- calcNoisyAndTable(list(c("True","False"),c("True","False")),
                            c("Right","Wrong"),noSlip=.9)
  testthat::expect_equal(dina,cbind(Right=c(.9,0,0,0),Wrong=c(.1,1,1,1)))
  
  ##NIDA, logical-and except that inputs can randomly be bypassed
  nida <- calcNoisyAndTable(list(c("True","False"),c("True","False")),
                            c("Right","Wrong"),bypass=c(.3,.4))
  testthat::expect_equal(nida,cbind(Right=c(1,.3,.4,.12),Wrong=c(0,.7,.6,.88)))
  
  ##Full Noisy And distribution
  noisyAnd <- calcNoisyAndTable(list(c("True","False"),c("True","False")),
                                c("Right","Wrong"),noSlip=.9,bypass=c(.3,.4))
  testthat::expect_equal(noisyAnd,cbind(Right=c(1,.3,.4,.12)*.9,Wrong=.9*c(0,.7,.6,.88)+.1))
  
  thresh <- calcNoisyAndTable(list(c("H","M","L"),c("H","M","L")),
                              c("Right","Wrong"),
                              threshold=c("M","H"))
  testthat::expect_equal(thresh,cbind(Right=c(1,1,0, 0,0,0, 0,0,0),
                                      Wrong=c(0,0,1, 1,1,1, 1,1,1)))
  
})

test_that("calcNoisyAndFrame", {
  and <- calcNoisyAndFrame(list(c("True","False"),c("True","False")),
                           c("Right","Wrong"))
  testthat::expect_equal(as.character(and$Var1),rep(c("True","False"),2))
  testthat::expect_equal(as.character(and$Var2),rep(c("True","False"),each=2))
  testthat::expect_equal(and$Right,c(1,0,0,0))
  testthat::expect_equal(and$Wrong,c(0,1,1,1))
})

test_that("calcNoisyOrTable", {
  or <- calcNoisyOrTable(list(c("True","False"),c("True","False")),
                         c("Right","Wrong"))
  testthat::expect_equal(or,cbind(Right=c(1,1,1,0),Wrong=c(0,0,0,1)))
  
  
  ## DINO, logical-or except that is allows for a small chance of slipping.
  dino <- calcNoisyOrTable(list(c("True","False"),c("True","False")),
                           c("Right","Wrong"),noGuess=.9)
  testthat::expect_equal(dino,cbind(Right=c(1,1,1,.1),Wrong=c(0,0,0,.9)))
  
  
  ##NIDO, logical-or except that inputs can randomly be bypassed
  nido <- calcNoisyOrTable(list(c("True","False"),c("True","False")),
                           c("Right","Wrong"),suppression=c(.3,.4))
  testthat::expect_equal(nido,cbind(Right=c(.88,.6,.7,0),Wrong=1-c(.88,.6,.7,0)))

  
  ##Full Noisy Or distribution
  noisyOr <- calcNoisyOrTable(list(c("True","False"),c("True","False")),
                              c("Right","Wrong"),noGuess=.9,suppression=c(.3,.4))
  testthat::expect_equal(noisyOr,cbind(Right=.1+.9*c(.88,.6,.7,0),Wrong=c(.12,.4,.3,1)*.9))
  
  thresh <- calcNoisyOrTable(list(c("H","M","L"),c("H","M","L")),
                             c("Right","Wrong"),
                             threshold=c("M","H"))
  testthat::expect_equal(thresh,cbind(Right=c(1,1,1, 1,1,0, 1,1,0),
                                      Wrong=1-c(1,1,1, 1,1,0, 1,1,0)))
  
})

test_that("calcNoisyOrFrame", {
  or <- calcNoisyOrFrame(list(c("True","False"),c("True","False")),
                           c("Right","Wrong"))
  testthat::expect_equal(as.character(or$Var1),rep(c("True","False"),2))
  testthat::expect_equal(as.character(or$Var2),rep(c("True","False"),each=2))
  testthat::expect_equal(or$Right,c(1,1,1,0))
  testthat::expect_equal(or$Wrong,c(0,0,0,1))
})


