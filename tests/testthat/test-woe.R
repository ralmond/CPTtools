test_that("parseProbVec", {
  pvec <- parseProbVec(c(Good = "[High:.8,Med:.15,Low:.05]",
                 Bad = "[High:.15,Med:.35,Low:.5]",
                 Ugly = "[High:.01,Med:.09,Low:.9]"))
  expect_equal(dim(pvec),c(3L,3L))
  expect_equal(rownames(pvec),c("Good","Bad","Ugly"))
  expect_equal(colnames(pvec),c("High","Med","Low"))
  expect_equal(as.numeric(rowSums(pvec)),rep(1.0,3),tolerance=.005)
  
})

test_that("readHistory", {
  testFiles <- system.file("testFiles",package="CPTtools")
  acdf <- read.csv(file.path(testFiles,"CorrectSequence.csv"),as.is=TRUE)
  allcorrect <- readHistory(acdf, probcol="Margin.sequences.")
  expect_equal(nrow(allcorrect),nrow(acdf))
  expect_equal(colnames(allcorrect),c("High","Medium","Low"))
  expect_true(all(grepl("=1$",rownames(allcorrect))))

  aicdf <- read.csv(file.path(testFiles,"InCorrectSequence.csv"),as.is=TRUE)
  allincorrect <- readHistory(aicdf, probcol="Margin.sequences.")
  expect_equal(nrow(allincorrect),nrow(aicdf))
  expect_equal(colnames(allincorrect),c("High","Medium","Low"))
  expect_true(all(grepl("=0$",rownames(allincorrect))))
  
})

test_that("woeBal", {
  testFiles <- system.file("testFiles",package="CPTtools")
  acdf <- read.csv(file.path(testFiles,"CorrectSequence.csv"),as.is=TRUE)
  allcorrect <- readHistory(acdf, probcol="Margin.sequences.")
  vdiffr::expect_doppelganger("all correct", function() {
    woeBal(allcorrect)
  })
  
  aicdf <- read.csv(file.path(testFiles,"InCorrectSequence.csv"),as.is=TRUE)
  allincorrect <- readHistory(aicdf, probcol="Margin.sequences.")
  vdiffr::expect_doppelganger("all incorrect", function() {
    woeBal(allincorrect)
  })

})

test_that("woeHist", {
  h1 <- rbind(base=c(1,1)/2,evidence=c(10,1)/11)
  expect_equal(woeHist(h1,1,2),c(evidence=100))
  h2 <- rbind(base=c(H=2,M=1,L=1)/4,
              evidence=c(H=20,M=1,L=1)/22)
  expect_equal(woeHist(h2,1),c(evidence=100))
  expect_equal(woeHist(h2,c(TRUE,FALSE,FALSE)),c(evidence=100))
  expect_equal(woeHist(h2,"H"),c(evidence=100))
  h3 <- rbind(base=c(H=1,M=1,L=2)/4,
              evidence=c(H=10,M=10,L=2)/22)
  expect_equal(woeHist(h3,1:2),c(evidence=100))
  expect_equal(woeHist(h3,c(TRUE,TRUE,FALSE)),c(evidence=100))
  expect_equal(woeHist(h3,c("H","M")),c(evidence=100))
  testFiles <- system.file("testFiles",package="CPTtools")
  allcorrect <- readHistory(read.csv(file.path(testFiles,
                                               "CorrectSequence.csv"),as.is=TRUE),
                            probcol="Margin.sequences.")
  acw <- woeHist(allcorrect,"High",c("Medium","Low"))
  expect_length(acw,nrow(allcorrect)-1L)
})

test_that("EAPBal", {
  sampleSequence <- read.csv(system.file("testFiles","SampleStudent.csv",
                                         package="CPTtools"),
                             header=TRUE,row.names=1)
  SolveGeometricProblems <- sampleSequence$H-sampleSequence$L
  names(SolveGeometricProblems) <- rownames(sampleSequence)
  vdiffr::expect_doppelganger("eap balance sheet",function (){
    EAPBal(SolveGeometricProblems, lcex=1.25)
  })
})

