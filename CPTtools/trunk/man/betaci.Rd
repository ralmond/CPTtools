\name{betaci}
\alias{betaci}
\title{Credibility intervals for a proportion based on beta distribution}
\description{
  This generates upper and lower bounds for a highest posterior
  density credibility interval for a beta distribution by looking at
  appropriate quantiles of the beta distribution.  This is designed to
  work with sums of classification probabilities.
}
\usage{
betaci(sumData, totals = NULL, limits = c(lower = 0.025, upper = 0.975),
       a = 0.5, b = 0.5)
}
\arguments{
  \item{sumData}{Counts or averages of proportions.  Note these do not
    need to be integers, sums of classification probabilities work here.}
  \item{totals}{Total number of individuals as reference for
    \code{sumData}.  If missing or \code{NULL} then the value use is
    \code{colSums(data)}.}
  \item{limits}{The upper and lower credibility limits.}
  \item{a}{Value for the \code{shape1} parameter of the beta prior.}
  \item{b}{Value for the \code{shape2} parameter of the beta prior.}
}
\details{
  This function computes the upper and lower bounds of a credibility
  interval for a beta distribution based on \code{sumData} successes out
  of \code{totals} trials.  Note that as a beta distribution is used for
  the basic calculations, neither \code{sumData} nor \code{totals} need
  be integers.

  To avoid problems with zero cells (or cells with values equal to
  \code{totals}), a small prior is added to the beta calculations.  By
  default a Jeffrey's prior \eqn{(.5,.5)} is added to the data.  Thus the
  final returned value is:
  \deqn{\code{qbeta}(prob,sumData+a,totals-sumData+b)}
  where \code{prob} varies over the values in \code{limits}.  Note that
  \code{a} and \code{b} can be scalars or an array conformable with
  \code{totals}.  
}
\value{
  A list of the same length as \code{limits} with the same names.  Each
  component is a quantile of the posterior distribution which has the
  same shape as \code{sumData}.

  Note that \code{limits} is not limited to length 2, although this is
  the most obvious application.
}
\author{Russell Almond}
\seealso{See \code{\link{OCP}} for an application.}
\examples{

x <- matrix(c(7,4,2,31),2,2)
## Use column sums as totals
betaci(x)

## fixed totals
nn <- matrix(c(30,15,20,35),2,2)
betaci(x,nn)

## Prior varies according to cell.
pi0 <- c(.2,.2,.2,.8)
betaci(x,nn,a=pi0,b=1-pi0)

}
\keyword{tests}
