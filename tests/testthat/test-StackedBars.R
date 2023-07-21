test_that("barchart.CPF", {
  skill1l <- c("High","Medium","Low") 
  skill2l <- c("High","Medium","Low","LowerYet") 
  correctL <- c("Correct","Incorrect") 
  pcreditL <- c("Full","Partial","None")
  gradeL <- c("A","B","C","D","E") 
  
  cpfTheta <- calcDPCFrame(list(),skill1l,numeric(),0,rule="Compensatory",
                           link="normalLink",linkScale=.5)
  
  vdiffr::expect_doppelganger("no parent barchart", barchart.CPF(cpfTheta))
  
  cptComp <- calcDPCFrame(list(S2=skill2l,S1=skill1l),correctL,
                          lnAlphas=log(c(1.2,.8)), betas=0,
                          rule="Compensatory")
  vdiffr::expect_doppelganger("two parent barchart",
                              barchart.CPF(cptComp,layout=c(3,1)))
  
  cptPC1 <- calcDPCFrame(list(S1=skill1l,S2=skill2l),pcreditL,
                         lnAlphas=log(1),
                         betas=list(full=c(S1=0,S2=999),partial=c(S2=999,S2=0)),
                         rule="OffsetDisjunctive")
  vdiffr::expect_doppelganger("disjunctive barchart",
                              barchart.CPF(cptPC1,baseCol="slateblue"))
})

test_that("stackedBarplot", {
  
  margins <- data.frame (
    Trouble=c(Novice=.19,Semester1=.24,Semester2=.28,Semseter3=.20,Semester4=.09),
    NDK=c(Novice=.01,Semester1=.09,Semester2=.35,Semseter3=.41,Semester4=.14),
    Model=c(Novice=.19,Semester1=.28,Semester2=.31,Semseter3=.18,Semester4=.04)
  )
  margins <- as.matrix(margins)
  baseline <- apply(margins[1:2,],2,sum)

  plotit <- function ()
    stackedBarplot(margins,offset=-baseline,
            main="Marginal Distributions for NetPASS skills",
            sub="Baseline at 2nd Semester level.",
            col=hsv(223/360,.2,0.10*(5:1)+.5))
  vdiffr::expect_doppelganger("stacked barplot",plotit)
})

test_that("compareBars", {
  margins.prior <- data.frame (
    Trouble=c(Novice=.19,Semester1=.24,Semester2=.28,Semseter3=.20,Semester4=.09),
    NDK=c(Novice=.01,Semester1=.09,Semester2=.35,Semseter3=.41,Semester4=.14),
    Model=c(Novice=.19,Semester1=.28,Semester2=.31,Semseter3=.18,Semester4=.04)
  )
  
  margins.post <- data.frame(
    Trouble=c(Novice=.03,Semester1=.15,Semester2=.39,Semseter3=.32,Semester4=.11),
    NDK=c(Novice=.00,Semester1=.03,Semester2=.28,Semseter3=.52,Semester4=.17),
    Model=c(Novice=.10,Semester1=.25,Semester2=.37,Semseter3=.23,Semester4=.05))
  
   plot1 <- function ()
    compareBars(margins.prior,margins.post,3,c("Prior","Post"),
                main="Margins before/after Medium Trouble Shooting Task",
                sub="Observables:  cfgCor=Medium, logCor=High, logEff=Medium",
                legend.loc = "topright",
                cex.names=.75, col1=hsv(h=.1,s=.2*1:5-.1,alpha=1),
                col2=hsv(h=.6,s=.2*1:5-.1,alpha=1))
  vdiffr::expect_doppelganger("compare bars",plot1)
   
  plot2 <- function()
    compareBars2(margins.prior,25*margins.post,3,c("Prior","Post"),
               main="Margins before/after Medium Trouble Shooting Task",
               sub="Observables:  cfgCor=Medium, logCor=High, logEff=Medium",
               legend.loc = "topright",
               cex.names=.75, col1=hsv(h=.1,s=.2*1:5-.1,alpha=1),
               col2=hsv(h=.6,s=.2*1:5-.1,alpha=1))
 vdiffr::expect_doppelganger("compare bars 2",plot2) 
})

test_that("stackedBars", {
  margins <- data.frame (
    Trouble=c(Novice=.19,Semester1=.24,Semester2=.28,Semseter3=.20,Semester4=.09),
    NDK=c(Novice=.01,Semester1=.09,Semester2=.35,Semseter3=.41,Semester4=.14),
    Model=c(Novice=.19,Semester1=.28,Semester2=.31,Semseter3=.18,Semester4=.04)
  )
  
  plotpercent <- function()
  stackedBars(margins,3,
              main="Marginal Distributions for NetPASS skills",
              sub="Baseline at 3rd Semester level.",
              cex.names=.75, col=hsv(223/360,.2,0.10*(5:1)+.5))
  vdiffr::expect_doppelganger("Plot with percents",
                              plotpercent)
  
  plotnum <- function()
  stackedBars(margins,2,
              main="Marginal Distributions for NetPASS skills",
              sub="Baseline at 2nd Semester level.",
              percent=FALSE,digits=2,
              cex.names=.75, col=hsv(223/360,.2,0.10*(5:1)+.5))
  vdiffr::expect_doppelganger("Plot with numbers",plotnum)
})

test_that("colorspread", {
  expect_equal(colorspread("red",4),
               c("#FFBFBF","#FF8080","#FF4040", "#FF0000"))
  expect_equal(colorspread("blue",4),
               c("#BFBFFF","#8080FF","#4040FF", "#0000FF"))
  
})

