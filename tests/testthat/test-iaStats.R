test_that("mutualInformation", {
  expect_equal(mutualInformation(matrix(1,2,2)),0.0,tolerance=.0001)
  expect_equal(mutualInformation(diag(2)),log(2),tolerance=.0001)
  expect_equal(mutualInformation(diag(3)),log(3),tolerance=.0001)
})

test_that("ewoe.CPF", {
  t1 <- diag(2)*.9+.1
  expect_equal(ewoe.CPF(t1,1),100*.9/1.1,tolerance=.001)
  
  ACED <- dplyr::inner_join(ACED.scores,ACED.items,by="SubjID")
  expcr <- expTable(ACED,"cr","tCommonRatio1a")
  e1 <- ewoe.CPF(expcr,"H")
  e2 <- ewoe.CPF(expcr,c("H","M"))
  expect_gt(e2,e1)
})

test_that("ciTest", {
  
})

test_that("localDepTest", {
  
})

test_that("catTable",{
  ctab <- catTable(ACED.items,"tCommonRatio1a")
  expect_equal(nrow(ctab),nrow(ACED.items))
  expect_equal(ncol(ctab),2L)
  expect_equal(as.numeric(colSums(ctab,na.rm=TRUE)),
               as.numeric(table(ACED.items$tCommonRatio1a)))
  
})

test_that("pvecTable",{
  etab <- pvecTable(ACED.scores,"sgp")
  expect_equal(nrow(etab),nrow(ACED.scores))
  expect_equal(ncol(etab),3L)
  expect_equal(colnames(etab),c("H","M","L"))
  expect_equal(rowSums(etab),rep(1.0,nrow(etab)),
               tolerance=.002)
})

test_that("expTable",{
  ACED.joined <- dplyr::inner_join(ACED.scores,ACED.items,
                                   "SubjID")
  etab <- expTable(ACED.joined,"cr","tCommonRatio1a")
  expect_equal(nrow(etab),3L)
  expect_equal(rownames(etab),c("H","M","L"))
  expect_equal(ncol(etab),2L)
  expect_equal(sum(etab),sum(!is.na(ACED.joined$tCommonRatio1a)),
               tolerance=.01)
    
})

test_that("plotsimplex",{
  
})

test_that("pvecTable", {
  
})

test_that("simplex_vertex_projection",{
  
})
