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
  cit1 <- ciTest(UCBAdmissions)
  expect_equal(cit1$df,1)
  expect_gt(cit1$G2,qchisq(.99,1))
  expect_gt(cit1$p,.99)
  
  cit2 <- ciTest(UCBAdmissions[,,c("C","D")])
  expect_equal(cit2$df,1)
  expect_lt(cit2$G2,qchisq(.95,1))
  expect_lt(cit2$p,.95)
})

test_that("localDepTest", {
  ACED.items$Correct <- rowSums(ACED.items[,-(1:3)],na.rm=TRUE)
 
  t1 <- localDepTest(ACED.items$tCommonRatio1a,ACED.items$tCommonRatio2a,
                     cut(ACED.items$Correct,3))
  expect_equal(t1$df,1)
  expect_lt(t1$G2,qchisq(.95,1))
  expect_lt(t1$p,.95)
  
  t2 <- localDepTest(ACED.items$tCommonRatio1a,ACED.items$tCommonRatio2a,
                     sample(letters[1:3],nrow(ACED.items),replace=TRUE))
  expect_equal(t2$df,1)
  expect_gt(t2$G2,qchisq(.95,1))
  expect_gt(t2$p,.95)

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
  ptab <- pvecTable(ACED.scores,"sgp")
  vdiffr::expect_doppelganger("simplex plot",invisible(plotsimplex(ptab)))
})


test_that("simplex_vertex_projection",{
  p4 <- simplex_vertex_projection(4)
  expect_equal(p4[,"x"],c(0,1,0,-1))
  expect_equal(p4[,"y"],c(-1,0,1,0))
})
