test_that("mutualInformation", {
  
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
