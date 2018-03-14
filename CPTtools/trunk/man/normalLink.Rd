\name{normalLink}
\alias{normalLink}
\title{Link function using a normal regression.}
\description{
  
  This link function assumes that the effective theta for this
  distribution defines the mean of a normal distribution in a
  generalized regression model.  The link scale parameter describes the
  residual variance.

}
\usage{
normalLink(et, linkScale = NULL, obsLevels = NULL)
}
\arguments{
  \item{et}{
    A matrix of effective theta values.  There should be one row in this
    table for each configuration of the parent variables of the
    conditional probability table and one column for each state of the
    child variables except for the last.}
  \item{linkScale}{The residual standard deviation parameter.  This
    value must be supplied and should be a positive number; the default
    value of \code{NULL} generates an error.}
  \item{obsLevels}{An optional character vector giving the names of the
    child variable states. If supplied, it should have length
    \code{ncol(et)+1}.}
}
\details{

  This function takes care of the third step in the algorithm of
  \code{\link{calcDPCTable}}.  Its input is a matrix of effective theta
  values (comparable to the last column of the output of
  \code{\link{eThetaFrame}}), one column for each of the child variable
  states (\code{obsLevels}) except for the last one.   Each row
  represents a different configuration of the parent variables.  The
  output is the conditional probability table.  The use of this function
  makes \code{calcDPCTable} behave like \code{\link{calcDNTable}}.
  
  The idea behind this link function was first proposed in Almond
  (2010), and it is more completely described in Almond et al. (2015).
  The motivation comes from assuming that the child variable is created
  by taking cuts on an underlying continuous variable.  The marginal
  distribution of this variable is a standard normal.  The conditional
  distribution is like a regression prediction with the effective theta
  from the parent variables (the \code{et} argument) as the expected
  value and the \code{linkScale} parameter as the residual standard
  deviation. 

  The calculation works as follows:  First cut points are set related to
  the categories of the child variable.  Let \code{m} be the number of
  categories (this should be one more than the number of columns of
  \code{et}) and the length of \code{obsLevels} if that is supplied).
  Then the cut points are set at \code{cuts <- \link[stats]{qnorm}(((m -
    1):1)/m)}.

  Then for each row of the conditional probability table \code{i}, the
  probability of being in state \code{k} is calculated by
  \code{pnorm(cuts[k]-et[i, 1])/linkScale) -
    pnorm(cuts[k-1]-et[i, 1])/linkScale)} with the \code{pnorm}
  expression set to 0 or 1 at the endpoints.  Note that only the first
  column of \code{et} is used in the calculation.
  
}
\value{
  A matrix with one more column than \code{et} giving the conditional
  probabilities for each configuration of the parent variables (which
  correspond to the rows).
}
\references{
  Almond, R. G. (2010). \sQuote{I can name that Bayesian network in two
  matrixes.} \emph{International Journal of Approximate Reasoning.}
  \bold{51}, 167-178.

  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Yan, D. and Williamson, D.M.
  (2015) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 8.

  
}
\author{Russell Almond}
\note{

  The motivation for the normal link function originally came from the
  observation of odd behavior when variables given a DiBello-Samejima
  distribution (that is, using the \code{\link{gradedResponse}} link
  function) were used as parent variables for other variables with a
  DiBello-Samejima distribution.

  One potential reason for the odd behavior was that the graded response
  link function was not an inverse of the procedure used to assign the
  \code{\link{effectiveThetas}} to the parent variables.  Thus, using a
  probit link function (\code{normalLink}) was thought to be better for
  parent variables than using a logistic link function
  (\code{\link{gradedResponse}}), at the same time the convention of
  assigning parent values based on quantiles of the normal distribution
  started.  This made the \code{normalLink} and \code{effectiveThetas}
  approximate inverses (information is still lost through
  discritization).  Note that in the current implementation the scale
  factor of 1.7 has been added to both the \code{\link{partialCredit}}
  and \code{\link{gradedResponse}} functions to make the logistic
  function closer to the normal distribution and a better inverse for
  the effective theta procedure.
 
  
}

\seealso{

  Other link functions:  \code{\link{partialCredit}},
  \code{\link{gradedResponse}}.  

  Functions which directly use the link function:
  \code{\link{eThetaFrame}}, \code{\link{calcDPCTable}},
  \code{\link{mapDPC}} 

  Earlier version of the graded response link:
  \code{\link{calcDNTable}}


}
\examples{

skill1l <- c("High","Medium","Low") 
correctL <- c("Correct","Incorrect") 
pcreditL <- c("Full","Partial","None")
gradeL <- c("A","B","C","D","E") 

## Get some effective theta values.
et <- effectiveThetas(3)

normalLink(matrix(et,ncol=1),.5,correctL)
normalLink(matrix(et,ncol=1),.3,correctL)
normalLink(matrix(et,nrow=3,ncol=2),.5,pcreditL)
normalLink(matrix(et,nrow=3,ncol=2),.8,pcreditL)

normalLink(matrix(et,nrow=3,ncol=4),.5,gradeL)
normalLink(matrix(et,nrow=3,ncol=4),.25,gradeL)


}
\keyword{ distribution }

