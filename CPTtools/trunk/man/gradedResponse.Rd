\name{gradedResponse}
\alias{gradedResponse}
\title{A link function based on Samejima's graded response}
\description{
  This function converts a matrix of effective theta values into a
  conditional probability table by applying Samejima's graded response
  model to each row of the table.
}
\usage{
gradedResponse(et, linkScale = NULL, obsLevels = NULL)
}
\arguments{
  \item{et}{
    A matrix of effective theta values.  There should be one row in this
    table for each configuration of the parent variables of the
    conditional probability table and one column for each state of the
    child variables except for the last.
}
  \item{linkScale}{Unused.  For compatibility with other link functions. 
}
  \item{obsLevels}{
    A character vector giving the names of the child variable states.
    If supplied, it should have length \code{ncol(et)+1}.  
}
}
\details{
  This function takes care of the third step in the algorithm of
  \code{\link{calcDPCTable}}.  Its input is a matrix of effective theta
  values (comparable to the last column of the output of
  \code{\link{eThetaFrame}}), one column for each of the child variable
  states (\code{obsLevels}) except for the last one.   Each row
  represents a different configuration of the parent variables.  The
  output is the conditional probability table.
  
  Let \eqn{X} be the child variable of the distribution, and assume that
  it can take on \eqn{M} possible states labeled \eqn{x_1} through
  \eqn{x_M} in increasing order.  The graded response model defines a
  set of functions \eqn{Z_m(\theta_k)} for \eqn{m=2,\ldots,M},
  where \deqn{ Pr(X >= x_m | \theta_k) = logit^{-1}
    -D*Z_m(\theta_k)}
  The conditional probabilities for each child state given the effective
  thetas for the parent variables is then given by
  \deqn{Pr(X == x_m |\theta_k) \frac{\sum_{r=1}^m
      Z_r(\theta_k)}{\sum_{r=1}^M  Z_r(\theta_k)}}

  The \eqn{K \times M-1} matrix \code{et} is the values of
  \eqn{Z_m(\theta_k)}.  This function then performs the rest of the
  generalized partial credit model.  This is a generalization of Muraki
  (1992), because the functions \eqn{Z_m(\cdot)} are not restricted to
  be the same functional form for all \eqn{m}.

  If supplied \code{obsLevels} is used for the column names.
}
\value{
  A matrix with one more column than \code{et} giving the conditional
  probabilities for each configuration of the parent variables (which
  correspond to the rows).
}
\references{
  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and
  Yan, D. (Draft) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 8.

  Muraki, E. (1992).  A Generalized Partial Credit Model:  Application
  of an EM Algorithm.  \emph{Applied Psychological Measurement}, \bold{16},
  159-176.  DOI: 10.1177/014662169201600206

  Samejima, F. (1969) Estimation of latent ability using a
  response pattern of graded scores.  \emph{Psychometrika Monograph No.
  17}, \bold{34}, (No. 4, Part 2).

  I also have planned a manuscript that describes these functions in
  more detail.
}
\author{Russell Almond}
\note{
  The \code{linkScale} parameter is unused.  It is for compatibility
  with other link function choices.
}
\seealso{
  \code{\link{eThetaFrame}},\code{\link{calcDPCTable}},
  \code{\link{gradedResponse}},
}
\examples{
## Set up variables
skill1l <- c("High","Medium","Low") 
correctL <- c("Correct","Incorrect") 
pcreditL <- c("Full","Partial","None")
gradeL <- c("A","B","C","D","E") 

## Get some effective theta values.
et <- effectiveThetas(3)

gradedResponse(matrix(et,ncol=1),NULL,correctL)

gradedResponse(outer(et,c(Full=1,Partial=-1)),NULL,pcreditL)

gradedResponse(outer(et,c(A=2,B=1,C=0,D=-1)),NULL,gradeL)

}
\keyword{distribution}

