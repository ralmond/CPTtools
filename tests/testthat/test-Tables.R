test_that("buildFactorTab", {
  data(ACED)
  mat <- buildFactorTab(ACED.scores, ACED.scores$Cond_code, c("H","M","L"), "sgp",
                  reverse = TRUE,
                   stem="P", sep=".")
  expect_true(is.matrix(mat))
  expect_equal(dim(mat),c(3L,4L))
  expect_equal(as.vector(colSums(mat)[1:3]),rep(1,3),tolerance=.0005)
})

test_that("build2FactorTab", {
  data(ACED)
  arr <- build2FactorTab(ACED.scores, ACED.scores$Sequencing, ACED.scores$Feedback,
                  c("H","M","L"), "sgp",
                  reverse = TRUE, stem="P",sep=".")
  expect_true(is.array(arr))
  expect_equal(dim(arr),c(2L,2L,3L))
  sums <- as.vector(apply(arr,1:2,sum))
  expect_equal(sums, c(1,1,NaN,1), tolerance=.00005)
})

test_that("buildMarginTab", {
  data(ACED)
  skills <- ACED.skillNames$short[1:4] 
  levels <- c("H","M","L")
  mt <- buildMarginTab(ACED.scores, levels, skills,
                 reverse = TRUE,
                 stem="P",sep=".")
  expect_true(is.matrix(mt))
  expect_equal(dim(mt),c(3L,4L))
  expect_equal(rownames(mt),rev(levels))
  expect_equal(colnames(mt),skills)
  expect_equal(as.vector(colSums(mt)), rep(1,4), tolerance=.05)
  
  mt <- buildMarginTab(ACED.scores, levels, skills,
                       reverse = FALSE,
                       stem="P",sep=".")
  expect_equal(rownames(mt),levels)
})

test_that("marginTab", {
  data(ACED)
  skills <- ACED.skillNames$short[1:4] 
  levels <- c("H","M","L")
  mt <-  marginTab(ACED.scores[1,], levels, skills, reverse = TRUE,
            stem="P",sep=".")
  expect_true(is.matrix(mt))
  expect_equal(dim(mt),c(3L,4L))
  expect_equal(rownames(mt),rev(levels))
  expect_equal(colnames(mt),skills)
  expect_equal(as.vector(colSums(mt)), rep(1,4), tolerance=.05)
  
  mt <-  marginTab(ACED.scores[1,], levels, skills, reverse = FALSE,
                   stem="P",sep=".")
  expect_equal(rownames(mt),levels)
})

test_that("as.CPA", {
  arr1 <- array(1:24,c(4,3,2),
                      dimnames=list(A=c("a1","a2","a3","a4"),B=c("b1","b2","b3"),
                                    C=c("c1","c2")))
  arr1a <- as.CPA(arr1)
  expect_s3_class(arr1a,"CPA")
  expect_true(is.CPA(arr1a))
  expect_equal(dim(arr1a),c(4L,3L,2L))
  
  arf <- data.frame(A=factor(rep(c("a1","a2"),each=3)),
                  B=factor(rep(c("b1","b2","b3"),2)),
                  C.c1=1:6, C.c2=7:12, C.c3=13:18, C.c4=19:24)
  arfa <- as.CPA(arf)
  expect_true(is.CPA(arfa))
  expect_equal(dim(arfa),c(A=2L,B=3L,C=4L))
  expect_equal(dimnames(arfa),list(A=c("a1","a2"),B=c("b1","b2","b3"),
                                  C=c("c1","c2","c3","c4")))
  
  ## Double warning expected.  This is a bit of kludge but it works.
  expect_warning(expect_warning(as.CPA(array(1:24,2:4)),
                "Array being coerced to CPA does not have dimnames."))


})

test_that("as.CPA 1 dimension", {

  cpf <- calcDNFrame(list(),c("H","M","L"),.6,0,.8)
  cpa <- as.CPA(cpf)
  
  expect_true(is.CPA(cpa))
  expect_equal(dim(cpf),dim(cpa))
    
})

test_that("is.CPA", {
  arr1 <- array(1:24,c(4,3,2),
                dimnames=list(A=c("a1","a2","a3","a4"),B=c("b1","b2","b3"),
                              C=c("c1","c2")))
  arr1a <- as.CPA(arr1)
  expect_true(is.CPA(arr1a))
  
  arf <- data.frame(A=factor(rep(c("a1","a2"),each=3)),
                   B=factor(rep(c("b1","b2","b3"),2)),
                   C.c1=1:6, C.c2=7:12, C.c3=13:18, C.c4=19:24)
  arf <- as.CPF(arf)
  expect_false(is.CPA(arf))
              
})

test_that("as.CPF", {
  arf <- data.frame(A=factor(rep(c("a1","a2"),each=3)),
                    B=factor(rep(c("b1","b2","b3"),2)),
                    C.c1=1:6, C.c2=7:12, C.c3=13:18, C.c4=19:24)
  arf <- as.CPF(arf)
  expect_s3_class(arf,"CPF")
  expect_equal(dim(arf),c(6L,6L))
  facCols <- as.vector(sapply(arf,is.factor))
  expect_equal(facCols, c(rep(TRUE,2),rep(FALSE,4)))
  
  arr <- array(1:24,c(2,3,4),
            dimnames=list(A=c("a1","a2"),B=c("b1","b2","b3"),
                          C=c("c1","c2","c3","c4")))
  arrf <- as.CPF(arr)
  expect_true(is.CPF(arrf))
  expect_equal(dim(arrf),c(6L,6L))
  expect_equal(levels(arrf$A),c("a1","a2"))
  expect_equal(levels(arrf$B),c("b1","b2","b3"))
  expect_equal(names(arrf),names(arf))

})

test_that("is.CPF", {
  arf <- data.frame(A=factor(rep(c("a1","a2"),each=3)),
                    B=factor(rep(c("b1","b2","b3"),2)),
                    C.c1=1:6, C.c2=7:12, C.c3=13:18, C.c4=19:24)
  arf <- as.CPF(arf)
  expect_true(is.CPF(arf))
  arr1 <- array(1:24,c(4,3,2),
                dimnames=list(A=c("a1","a2","a3","a4"),B=c("b1","b2","b3"),
                              C=c("c1","c2")))
  arr1a <- as.CPA(arr1)
  expect_false(is.CPF(arr1a))
})


test_that("normalize.matrix", {
  mat <- normalize(matrix(1:6,2,3))
  expect_equal(dim(mat),c(2L,3L))
  expect_equal(rowSums(mat),rep(1.0,nrow(mat)),tolerance=.0001)
})

test_that("normalize.array", {
  arr <- array(1:24,c(4,3,2),dimnames=list(A=c("a1","a2","a3","a4"),
                                           B=c("b1","b2","b3"),
                                           C=c("c1","c2")))
  narr <- normalize(arr)
  expect_equal(dim(narr),dim(arr))
  rowsums <- as.vector(apply(narr,1:2,sum))
  expect_equal(rowsums,rep(1.0,length(rowsums)),tolerance=.0001)
})

test_that("normalize.table", {
  data("UCBAdmissions")
  tab <- aperm(UCBAdmissions,3:1)
  ntab <- normalize(tab)  
  expect_equal(dim(ntab),dim(tab))
  rowsums <- as.vector(apply(ntab,1:2,sum))
  expect_equal(rowsums,rep(1.0,length(rowsums)),tolerance=.0001)
})

test_that("normalize.data.frame", {
  df2 <- data.frame(parentval=c("a","b"),
                    prob.true=c(1,1),prob.false=c(1,1))
  ndf2 <- normalize(df2)
  expect_true(is.CPF(ndf2))
  facCols <- as.vector(sapply(ndf2,is.character))
  expect_equal(facCols,c(TRUE,FALSE,FALSE))
  expect_equal(rowSums(ndf2[,2:3]),rep(1.0,2),tolerance=.0001)
  
})

test_that("normalize.CPA", {
  arr <- array(1:24,c(4,3,2),
               list(a=c("A1","A2","A3","A4"),
                    b=c("B1","B2","B3"),
                    c=c("C1","C2")))
  arr <- as.CPA(arr)
  narr <- normalize(arr)
  expect_true(is.CPA(narr))
  rowsums <- as.vector(apply(narr,1:2,sum))
  expect_equal(rowsums,rep(1.0,length(rowsums)),tolerance=.0001)
})

test_that("normalize.default", {
  n14 <- normalize(1:4)
  expect_equal(sum(n14),1.0,tolerance=.0001)
})

test_that("normalize.CPF", {
  arr <- array(1:24,c(4,3,2),
               list(a=c("A1","A2","A3","A4"),
                    b=c("B1","B2","B3"),
                    c=c("C1","C2")))
  arf <- as.CPF(arr)
  narf <- normalize(arf)  
  expect_true(is.CPF(narf))
  facCols <- as.vector(sapply(narf,is.factor))
  expect_equal(facCols,c(TRUE,TRUE,FALSE,FALSE))
  expect_equal(rowSums(narf[,3:4]),rep(1.0,4*3),tolerance=.0001)
  
})

test_that("dataTable",{
  skill1l <- c("High","Medium","Low")
  skill3l <- c("High","Better","Medium","Worse","Low")
  correctL <- c("Correct","Incorrect")
  
  x <- read.csv(system.file("testFiles", "randomPinned100.csv",
                            package="CPTtools"),
                header=FALSE, as.is=TRUE,
                col.names = c("Skill1", "Skill2", "Skill3",
                              "Comp.Correct", "Comp.Grade",
                              "Conj.Correct", "Conj.Grade",
                              "Cor.Correct", "Cor.Grade",
                              "Dis.Correct", "Dis.Grade",
                              "Inhib.Correct", "Inhib.Grade"
                ))
  x[,"Skill1"] <- ordered(x[,"Skill1"],skill1l)
  x[,"Skill3"] <- ordered(x[,"Skill3"],skill3l)
  x[,"Comp.Correct"] <- ordered(x[,"Comp.Correct"],correctL)
  
  
  tab <- dataTable(x, c("Skill1","Skill3"),"Comp.Correct",correctL)
  expect_equal(dim(tab),c(15L,2L))
  expect_equal(colSums(tab),c(Correct=77,Incorrect=23))
  expect_equal(tab[1,],c(Correct=21,Incorrect=0))
})
