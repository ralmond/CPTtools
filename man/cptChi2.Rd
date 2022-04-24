\name{cptChi2}
\alias{cptChi2}
\title{Compare and observed to an expected conditional probability table.}
\description{

  Each row of a conditional probability frame (\link{CPF}) is a
  probability distribution.  Observed data comes in the form of a
  contingency table (or expected contingency table) where the rows
  represent configurations of the parent variables and the columns
  represent the state of the child variable.  The \code{cptChi2}
  calculates the chi-squared data fit statistic for each row of the
  table, and sums them together.  This provides a test of the
  plausibility that the data came from the proposed model.

}
\usage{
cptChi2(obs, exp)
}
\arguments{
  \item{obs}{A \code{\link{CPF}} which contains the observed
    data.}
  \item{exp}{A \code{\link{CPF}} which contains the candidate distribution.}
}
\details{

  The cannonical use for this plot is to test the conditional
  probability model used in particular Bayesian network.  The \code{exp}
  argument is the proposed table.  If the parents are fully observed,
  then the \code{obs} argument would be a contingency table with
  the rows representing parent configurations and the columns the data.
  If the parents are not fully observed, then obs can be replaced by an
  expected contingincy table (see Almond, et al, 2015).

  To avoid potential problems with zero cells, the data table is
  modified by adding the prior.  That is the observed value is taken as
  \code{obs+exp}.  This should not have a big effect if the sample size
  is large.

}
\value{

  The sum of the chi-squres for all of the tables with the degrees of
  freedom.  The degrees of freedom is given as an attribute \code{df}.

}
\references{
  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and
  Yan, D. (2015) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 10.

  Sinharay, S. and Almond, R.G. (2006).  Assessing Fit of Cognitively
  Diagnostic Models:  A case study.  \emph{Educational and Psychological
    Measurement}.  \bold{67}(2), 239--257.

}
\author{Russell Almond}
\note{

  This is called a chi-square statistic, and the percentage points of
  the chi-square distribution can be used for reference. However, it is
  unclear whether the statistic is actually distributed according to the
  chi-square distribution under the hypothesis that the data come from
  the distribution given in \code{exp}.  In particular, if an expected
  table is used instead of an actual table, the distribution is likely
  to not be exactly chi-squared.
  
}
\seealso{
  \code{\link{betaci}}, \code{\link{OCP}}, \code{\link{barchart.CPF}},
  \code{\link{OCP.CPF}}

}
\examples{
  skill1l <- c("High","Medium","Low") 
  skill2l <- c("High","Medium","Low","LowerYet") 
  correctL <- c("Correct","Incorrect") 
  pcreditL <- c("Full","Partial","None")
  gradeL <- c("A","B","C","D","E") 

  cpfTheta <- calcDPCFrame(list(),skill1l,numeric(),0,rule="Compensatory",
                           link="normalLink",linkScale=.5)

  ## Compatable data  
  datTheta <- cpfTheta ## Copy to get the shape
  datTheta[1,] <- rmultinom(1,25,cpfTheta[1,])

  cptChi2(datTheta,cpfTheta)

  ## Incompatable data
  datThetaX <- cpfTheta ## Copy to get the shape
  datThetaX[1,] <- rmultinom(1,100,c(.05,.45,.5))

  cptChi2(datThetaX,cpfTheta)

  cptComp <- calcDPCFrame(list(S2=skill2l,S1=skill1l),correctL,
                          lnAlphas=log(c(1.2,.8)), betas=0,
                          rule="Compensatory")

  cptConj <- calcDPCFrame(list(S2=skill2l,S1=skill1l),correctL,
                          lnAlphas=log(c(1.2,.8)), betas=0,
                          rule="Conjunctive")

  datComp <- cptComp
  for (i in 1:nrow(datComp))
      ## Each row has a random sample size.
      datComp[i,3:4] <- rmultinom(1,rnbinom(1,mu=15,size=1),cptComp[i,3:4])

  datConj <- cptConj
  for (i in 1:nrow(datConj))
      ## Each row has a random sample size.
      datConj[i,3:4] <- rmultinom(1,rnbinom(1,mu=15,size=1),cptConj[i,3:4])

  ## Compatable
  cptChi2(datConj,cptConj)
  ## Incompatable
  cptChi2(datComp,cptConj)


  cptPC1 <- calcDPCFrame(list(S1=skill1l,S2=skill2l),pcreditL,
                         lnAlphas=log(1),
                         betas=list(full=c(S1=0,S2=999),partial=c(S2=999,S2=0)),
                         rule="OffsetDisjunctive")
  cptPC2 <- calcDPCFrame(list(S1=skill1l,S2=skill2l),pcreditL,
                         lnAlphas=log(1),
                         betas=list(full=c(S1=0,S2=999),partial=c(S2=1,S2=0)),
                         rule="OffsetDisjunctive")


  datPC1 <- cptPC1
  for (i in 1:nrow(datPC1))
      ## Each row has a random sample size.
      datPC1[i,3:5] <- rmultinom(1,rnbinom(1,mu=25,size=1),cptPC1[i,3:5])

  datPC2 <- cptPC2
  for (i in 1:nrow(datPC2))
      ## Each row has a random sample size.
      datPC2[i,3:5] <- rmultinom(1,rnbinom(1,mu=25,size=1),cptPC2[i,3:5])

  ## Compatable
  cptChi2(datPC1,cptPC1)
  ## Inompatable
  cptChi2(datPC2,cptPC1)

}
\keyword{ htest }
\keyword{ graphs }
\concept{ chi-square }
\concept{ Conditional Probability Table }

