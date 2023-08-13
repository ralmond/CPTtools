test_that("scaleMatrix", {
  expect_equal(scaleMatrix(MathGrades$var),MathGrades$cor,tolerance=.02)
})

test_that("structMatrix", {
  MG.struct <- structMatrix(MathGrades$var)
  expect_equal(dim(MG.struct),dim(MathGrades$var))
  expect_equal(rownames(MG.struct),rownames(MathGrades$var))
  expect_equal(colnames(MG.struct),colnames(MathGrades$var))
  expect_equal(as.logical(MG.struct[1,]),c(rep(TRUE,3),rep(FALSE,2)))
  expect_equal(as.logical(MG.struct[3,]),rep(TRUE,5))
  expect_equal(as.logical(MG.struct[5,]),c(rep(FALSE,2),rep(TRUE,3)))  
  
})

test_that("mcSearch", {
  MG.struct <- structMatrix(MathGrades$var)
  expord1 <- 1:5
  names(expord1) <- MathGrades$varnames
  expord2 <- c(2:3,1,4:5)
  names(expord2) <- MathGrades$varnames
  
  expect_equal(mcSearch(MG.struct),expord1)
  expect_equal(mcSearch(MG.struct,"Algebra"),expord2)
})

test_that("buildParentList", {
  MG.struct <- structMatrix(MathGrades$var)
  parents <- buildParentList(MG.struct)   # Arbitrary start
  parentsa <- buildParentList(MG.struct, "Algebra") # Put algebra first.
  
  expect_equal(parents$Mechanics,character())
  expect_equal(parents$Vectors,"Mechanics")
  expect_equal(parents$Algebra,c("Mechanics","Vectors"))
  expect_equal(parents$Analysis,c("Algebra"))
  expect_equal(parents$Statistics,c("Algebra","Analysis"))
  
  expect_equal(parentsa$Algebra,character())
  expect_equal(parentsa$Mechanics,"Algebra")
  expect_equal(parentsa$Vectors,c("Algebra","Mechanics"))
  expect_equal(parentsa$Analysis,c("Algebra"))
  expect_equal(parentsa$Statistics,c("Algebra","Analysis"))
  
})

test_that("pvecToCutpoints", {
  probs <- c(Low=.05,Med=.9,High=.05)
  exp <- c(-Inf,Low=qnorm(.05),Med=qnorm(.95),High=Inf)
  
  expect_equal(pvecToCutpoints(probs),exp)
  expect_equal(pvecToCutpoints(probs,1),exp+1)
  expect_equal(pvecToCutpoints(probs,0,2),exp*2)
  
})

test_that("pvecToMidpoints", {
  probs <- c(Low=.05,Med=.9,High=.05)
  exp <- c(Low=qnorm(.025),Med=0,High=qnorm(.975))

  
  expect_equal(pvecToMidpoints(probs),exp)
  expect_equal(pvecToMidpoints(probs,1),exp+1)
  expect_equal(pvecToMidpoints(probs,0,2),exp*2)
  
})


test_that("areaProbs", {
  probs <- c(Low=.05,Med=.9,High=.05)
  
  expect_equal(areaProbs(probs,0,1),probs)
  expect_equal(areaProbs(probs,1,0)[1],c(Low=pnorm(qnorm(.05),1)),tolerance=.01)
  expect_equal(areaProbs(probs,0,2)[1],c(Low=pnorm(qnorm(.05),0,2)),tolerance=.01)
  
})

test_that("buildRegressions", {
  pl <- buildParentList(structMatrix(MathGrades$var),"Algebra")
  rt <- buildRegressions(MathGrades$var,MathGrades$means,pl)
  
  expect_null(names(rt$Algebra$b))
  expect_equal(rt$Algebra$a,MathGrades$means["Algebra"])
  expect_gt(rt$Algebra$std,0)
  
  for (var in names(pl)[-1]) {
    expect_null(names(rt[[var]]$b),pl[[var]])
    b <- rt[[var]]$b
    pmean <- MathGrades$means[colnames(b)]
    expmean <- MathGrades$means[var] -sum(b*pmean)
    names(expmean) <- NULL
    expect_equal(rt[[var]]$a,expmean)
    expect_gt(rt[[var]]$std,0)
  }
  
})

test_that("buildRegressionTables", {
  pl <- buildParentList(structMatrix(MathGrades$var),"Algebra")
  rt <- buildRegressions(MathGrades$var,MathGrades$means,pl)
  tabs <- buildRegressionTables(rt, MathGrades$pvecs, MathGrades$means,
                                sqrt(diag(MathGrades$var)))
  
  expect_equal(tabs$Algebra,data.frame(High=.25,Med=.5,Low=.25))
  expect_equal(colnames(tabs$Mechanics),c(pl$Mechanics,c("High","Med","Low")))
  expect_equal(nrow(tabs$Mechanics),3^(length(pl$Mechanics)))
  expect_equal(colnames(tabs$Vectors),c(pl$Vectors,c("High","Med","Low")))
  expect_equal(nrow(tabs$Vectors),3^(length(pl$Vectors)))
  expect_equal(colnames(tabs$Analysis),c(pl$Analysis,c("High","Med","Low")))
  expect_equal(nrow(tabs$Analysis),3^(length(pl$Analysis)))
  expect_equal(colnames(tabs$Statistics),c(pl$Statistics,c("High","Med","Low")))
  expect_equal(nrow(tabs$Statistics),3^(length(pl$Statistics)))
  
})
