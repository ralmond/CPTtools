\name{expected}
\alias{expected}
\alias{expected.CPA}
\alias{expected.CPF}
\alias{expected.matrix}
\title{Calculate the expected values for a model to match data}
\description{

  The \code{data} argument is cell counts or expected cell counts in 
  \code{\link{CPF}}, \code{\link{CPA}} or matrix form.  The \code{model} 
  is a conditional probability table in the same form.  This scales the rows
  of the models to match the row sums of the data.

}
\usage{
expected(data, model,...)
\method{expected}{CPA}(data, model,...)
\method{expected}{CPF}(data, model,...)
\method{expected}{matrix}(data, model,...,factorsAsStrings=TRUE)
}
\arguments{
  \item{data}{A \code{\link{CPF}}, \code{\link{CPA}}, or matrix giving 
    (expected) cell counts.}
  \item{model}{A \code{\link{CPF}}, \code{\link{CPA}}, or matrix giving 
    the (unnormalized) conditional probability distribution.  The \code{model} 
    and \code{data} objects should have the same dimensions.}
  \item{...}{To allow methods to have other arguments.}
  \item{factorsAsStrings}{A logical value, if true and model must be coerced 
    to a matrix, row names will be based on state labels.}
}
\details{

  The \code{model} argument is \code{\link{normalize}} and then 
  multiplied by the row sums.

}
\value{

  The expected cell counts are returned as a matrix.  If the argument is a 
  \code{CPF} or \code{CPA}, then the row names are based on the state names for
  the conditioning variables (see \code{\link{as.matrix.CPF}}).

}
\author{Russell Almond}
\seealso{
  \code{\link{CPA}}, \code{\link{CPF}}, \code{\link{normalize}}
  
  \code{\link{resids}}, \code{\link{mosaic2}}, \code{\link{assoc2}}
}
\examples{

arr <- array(1:24,c(2,3,4),
            dimnames=list(A=c("a1","a2"),B=c("b1","b2","b3"),
                          C=c("c1","c2","c3","c4")))
arf <- as.CPF(arr)

mod <- data.frame(A=factor(rep(c("a1","a2"),each=3)),
                  B=factor(rep(c("b1","b2","b3"),2)),
                  C.c1=rep(1/4,6), C.c2=rep(1/4,6), C.c3=rep(1/4,6), 
                  C.c4=rep(1/4,6))
mod <- as.CPF(mod)

expected(arf,mod)

}
\keyword{ array }
\keyword{ category }
\concept{ expected value }
\concept{CPF}