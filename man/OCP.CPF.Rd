\name{OCP.CPF}
\alias{OCP.CPF}
\alias{OCP2.CPF}
\title{Plots a conditional probability distribuiton with data overlayed}
\description{

  The observable characteristic plot is a plot that compares a
  conditional probabiltiy distribution with a set of data nominally from
  that distribution.  The \code{exp} argument is the conditional
  distribution in \code{\link{CPF}} format.  This is plotted as a
  barchart.  The \code{obs} argument is the data in \code{\link{CPF}}
  (rows not normalized).  The data are plotted as cumulative probability
  estimates with error bars overlaid on the barplot.  The function
  \code{OCP2.CPF} does the same thing, but contrasts two different data
  sets. 

}
\usage{
OCP.CPF(obs, exp, ..., baseCol = "chocolate",
        limits = c(lower = 0.025, upper = 0.975),
        Parents = getTableParents(exp),
        States = getTableStates(exp),
        nstates=length(States),
        key=list(points=FALSE,rectangles=TRUE,text=States),
        par.settings = list(),
        point.pch = paste((nstates-1):0),
        point.col = "black", point.cex = 1.25,
        line.col = "black", line.type=1)
OCP2.CPF(obs1, obs2, exp, ..., baseCol = "chocolate",
        limits = c(lower = 0.025, upper = 0.975),
        Parents = getTableParents(exp),
        States = getTableStates(exp),
        nstates=length(States),
        key=list(points=FALSE,rectangles=TRUE,text=States),
        par.settings = list(),
        point1.pch = paste((nstates-1):0),
        point1.col = "black", point1.cex = 1.25,
        line1.col = "black", line1.type=1,
        point1.pch = paste((nstates-1):0),
        point1.col = "cyan", point1.cex = 1.25,
        line1.col = "cyan", line1.type=1)

}
\arguments{
  \item{obs, obs1, obs2}{A \code{\link{CPF}} which contains the observed
    data.  The 1 and 2 variants specify the upper and lower plotted data
    data for \code{OCP2.CPF}.}
  \item{exp}{A \code{\link{CPF}} which contains the candidate distribution.}
  \item{\dots}{Other arguments passed to \code{\link[lattice]{barchart}}.}
  \item{baseCol}{The base color to use for the color gradients in the bars.}
  \item{limits}{A vector of two probabilities representing the quantiles
    of the beta distribution to be used in error bars.}
  \item{Parents}{A character vector giving the names of the parent
    variables.}
  \item{States}{A character vector giving the names of the child states.}
  \item{nstates}{An integer giving the nubmer of states.}
  \item{par.settings}{Setting passed to \code{\link[lattice]{barchart}}.}
  \item{key}{Settings passed to \code{\link[lattice]{simpleKey}} to
    construct the key.}
  \item{point.pch,point1.pch,point2.pch}{Characters used to plot data points.}
  \item{point.col,point1.col,point2.col}{The colour to be used for the
    overlay points.} 
  \item{point.cex,point1.cex,point2.ces}{The size (cex) to be used for
    the overlay points.} 
  \item{line.col,line1.col,line2.col}{The colour to be used for the
    overlay lines.} 
  \item{line.type,line1.type,line2.type}{The type (lty) for the overlay lines.}
}
\details{

  The cannonical use for this plot is to test the conditional
  probability model used in particular Bayesian network.  The \code{exp}
  argument is the proposed table.  If the parents are fully observed,
  then the \code{\link{obs}} argument would be a contingency table with
  the rows representing parent configurations and the columns the data.
  If the parents are not fully observed, then obs can be replaced by an
  expected contingincy table (see Almond, et al, 2015).

  The bars are coloured using an intesity scale starting from a base
  colour.  The \code{baseColor} argument gives the darkest colour in the
  scale.  The base plot is very similar to \code{\link{barchart.CPF}}.

  Point and interval estimates are made for the cumulative probabilities using a
  simple Bayesian estimator (this avoids potential issues with 0 cell
  counts).  Basically, the expected values (\code{exp}) are added to the
  data as a prior.  The intervals are formued using quantiles of the
  beta distribution.  The \code{limits} argument gives the quantiles
  used.

  The point and interval estimaters are plotted over the barchart using
  a numeric label (going from 0 smallest to \eqn{k-1}, were \eqn{k} is
  the number of states of the child variable) and line running from the
  lower to upper bound. If the line overlaps the corresponding bar in
  the barchart, then the data are consistent with the model (at least in
  that cell).

  The parameters \code{point.pch} \code{point.col}, \code{point.cex},
  \code{line.col}, and \code{line.type} control the appearance of the
  points and lines. These may either be scalars or vectors which match
  the number of states of the child variable.

  The function \code{OCP2.CPF} is similar but it meant for situations in
  which two different data distributions are to be compared.  In this
  one (\code{obs1}, \code{point1.*}, \code{line1.*}) is plotted in the
  upper half of the bar, and the other (\code{obs2}, \code{point2.*},
  \code{line2.*}) in the lower half.  Two important applications of this
  particular variation:
  \itemize{
    \item{Two different data sets are related to the presence/absence of
      an additional parent variable.  Thus, this is a test of whether or
      not the additional parent is relevant.}
    \item{Two different data sets represent a focal and reference
      demographic group.  This is a test of whether or not the item
      behaves similarly for both groups.}
  }


}
\value{

  Returns an object of class
  \code{lattice::\link[lattice]{trellis.object}}.  The default print
  method will plot this function.

}
\references{
  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and
  Yan, D. (2015) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 10.

  Sinharay, S. and Almond, R.G. (2006).  Assessing Fit of Cognitively
  Diagnostic Models:  A case study.  \emph{Educational and Psychological
    Measurement}.  \bold{67}(2), 239--257.

  Sinharay, S., Almond, R. G. and Yan, D. (2004).  Assessing fit of
    models with discrete proficiency variables in educational
    assessment.  ETS Research Report.
    \url{http://www.ets.org/research/researcher/RR-04-07.html}
}
\author{Russell Almond}
\seealso{
  \code{\link{betaci}}, \code{\link{OCP}}, \code{\link{barchart.PDF}},
  \code{\link{cptChi2}}
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

  OCP.CPF(datTheta,cpfTheta)

  ## Incompatable data
  datThetaX <- cpfTheta ## Copy to get the shape
  datThetaX[1,] <- rmultinom(1,100,c(.05,.45,.5))

  OCP.CPF(datThetaX,cpfTheta)
  OCP2.CPF(datTheta,datThetaX,cpfTheta)

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
  OCP.CPF(datConj,cptConj)
  ## Incompatable
  OCP.CPF(datComp,cptConj)
  OCP2.CPF(datConj,datComp,cptConj)

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
  OCP.CPF(datPC1,cptPC1)
  ## Inompatable
  OCP.CPF(datPC2,cptPC1)
  OCP2.CPF(datPC1,datPC2,cptPC1)



}
\keyword{hplot}
\concept{Conditional Probability Table}

