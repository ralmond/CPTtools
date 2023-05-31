test_that("betaci", {

  x <- matrix(c(7,4,2,31),2,2)
  bci <- betaci(x)
  expect_length(bci,2L)
  expect_equal(names(bci),c("lower","upper"))
  expect_true(all(bci$upper > bci$lower))
  expect_equal(bci$lower[,1:2]+bci$upper[,2:1],matrix(1.0,2,2))
    
  ## fixed totals
  x <- c(7,2,31)
  nn <- c(30,15,35)
  bci <- betaci(x,nn)
  expect_true(all((bci$upper > x/nn) & (x/nn > bci$lower)))
  bci <- betaci(x,nn, limits=c(low=.05,med=.5,high=.95))
  expect_length(bci,3L)
  expect_equal(names(bci),c("low","med","high"))
  expect_true(all((bci$high > bci$med) & (bci$med > bci$low)))
  
  ## Prior varies according to cell.
  pi0 <- c(.2,.2,.8)
  bci1 <- betaci(x,nn,a=pi0,b=1-pi0)
  bci2 <- betaci(x,nn,a=1-pi0,b=pi0)
  expect_equal(bci1$lower < bci2$lower, c(TRUE,TRUE,FALSE))
  expect_equal(bci1$upper < bci2$upper, c(TRUE,TRUE,FALSE))
  
})

test_that("proflevelci", {
  pl <- proflevelci(c(D=1,C=1,B=1,A=1),1)
  expect_length(pl,2L)
  expect_equal(names(pl),c("lower","upper"))
  expect_true(isIncreasing(pl$lower))
  expect_true(isIncreasing(pl$upper))
  expect_true(all(c(-1,1,1,1)*pl$lower>0))
  expect_true(all(c(-1,1,1,1)*pl$upper>0))
  
  pl <- proflevelci(c(D=1,C=1,B=1,A=1),2)
  expect_true(isIncreasing(pl$lower))
  expect_true(isIncreasing(pl$upper))
  expect_true(all(c(-1,-1,1,1)*pl$lower>0))
  expect_true(all(c(-1,-1,1,1)*pl$upper>0))
  
  pl <- proflevelci(c(D=1,C=1,B=1,A=1),3)
  expect_true(isIncreasing(pl$lower))
  expect_true(isIncreasing(pl$upper))
  expect_true(all(c(-1,-1,-1,1)*pl$lower>0))
  expect_true(all(c(-1,-1,-1,1)*pl$upper>0))
  
  margins <- data.frame (
    Trouble=c(Novice=19,Semester1=24,Semester2=28,Semseter3=20,Semester4=9),
    NDK=c(Novice=1,Semester1=9,Semester2=35,Semseter3=41,Semester4=14),
    Model=c(Novice=19,Semester1=28,Semester2=31,Semseter3=18,Semester4=4)
  )
  pi0 <- c(.25,.25,.25,.75,.75)
  pl <- proflevelci(margins,3,limits=c(low=.25,mid=.5,high=.97),a=.25,b=.75)
  expect_length(pl,3L)
  expect_equal(dim(pl$mid),c(5,3))
  
})
