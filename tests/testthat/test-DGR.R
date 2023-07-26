test_that("calcDPCTable", {
  skill1 <- c("High","Med","Low")
  skill2 <- c("High","Med","Low")
  troph <- c("Gold","Silver","None")
  testthat::skip("Test not implemented")
  
})

test_that("calcDPCFrame", {
  skill1 <- c("High","Med","Low")
  skill2 <- c("High","Med","Low")
  troph <- c("Gold","Silver","None")
  testthat::skip("Test not implemented") 
  
})

test_that("calcDPCTable GR", {
  skill1 <- c("High","Med","Low")
  skill2 <- c("High","Med","Low")
  troph <- c("Gold","Silver","None")
  cpt1 <- calcDPCTable(list(skill1),troph,list(log(.5),log(1)),list(1,-1),
               link="gradedResponse")
  cpt2 <- calcDPCTable(list(skill1),troph,list(log(.5),log(.01)),list(1,-1),
               link="gradedResponse")
  cpt3 <- calcDPCTable(list(skill1),troph,list(log(.5),log(.5)),list(1,1),
               link="gradedResponse")
  
  cpt4 <- calcDPCTable(list(skill1,skill2),troph,log(c(.8,.8)),list(1,-1),
               rules="Compensatory", link="gradedResponse")
  testthat::skip("Test not implemented")  
})

test_that("calcDPCTable PC", {
  skill1 <- c("High","Med","Low")
  skill2 <- c("High","Med","Low")
  troph <- c("Gold","Silver","None")
  calcDPCFrame(list(skill1),troph,list(log(.5),log(1)),list(1,-1))
  calcDPCFrame(list(skill1),troph,list(log(.5),log(1)),list(-1,-1))
  calcDPCFrame(list(skill1),troph,list(log(.5),log(.01)),list(1,-1))
  calcDPCFrame(list(skill1),troph,list(log(.01),log(.5)),list(0,-1))
  calcDPCFrame(list(skill1),troph,list(log(.5),log(.5)),list(1,1))
  calcDPCFrame(list(skill1),troph,list(log(.5),log(.01)),list(1,0))
  testthat::skip("Test not implemented")
})

test_that("calcDPCTable PC local-Q", {
  skill1 <- c("High","Med","Low")
  skill2 <- c("High","Med","Low")
  troph <- c("Gold","Silver","None")
  testthat::skip("Test not implemented")  
})

test_that("calcDPCTable normalLink no parents", {
  skill1 <- c("High","Med","Low")
  skill2 <- c("High","Med","Low")
  troph <- c("Gold","Silver","None")
  testthat::skip("Test not implemented")  
})
test_that("calcDPCTable normalLink parents", {
  skill1 <- c("High","Med","Low")
  skill2 <- c("High","Med","Low")
  troph <- c("Gold","Silver","None")
  testthat::skip("Test not implemented")  
})

