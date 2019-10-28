\name{isMonotonic}
\alias{isMonotonic}
\alias{isIncreasing}
\alias{isNondecreasing}
\alias{isDecreasing}
\alias{isNonincreasing}
\title{Tests to see if a sequence is ascending or descending}
\description{

  These functions take a vector and check to see if it is in increasing
  or decreasing order.  The function \code{isMonotonic} returns true if
  the sequence is either increasing (nondecreasing) or decreasing
  (nonincreasing), with the parenthesized values used if \code{strict}
  is false.  For \code{isMonotonic} an attribute of the return value
  tell the direction of the series.

}
\usage{
isMonotonic(vec, strict = TRUE)
isIncreasing(vec)
isNodecreasing(vec)
isDecreasing(vec)
isNonincreasing(vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{A vector of sortable objects (i.e., ones for which the
    comparison operators are defined.  The vector should have length of
    at least 2.}
  \item{strict}{A logical value.  If true, then the function will check
    for strict monotonicity.}
}
\details{

  These function use the following definitions:

  \describe{
    \item{Increasing.}{\code{vec[1] < vec[2] < }\dots \code{ < vec[k]}.}
    \item{Nondecreasing.}{\code{vec[1] <= vec[2] <= }\dots \code{ <= vec[k]}.}
    \item{Decreasing.}{\code{vec[1] > vec[2] > }\dots \code{ > vec[k]}.}
    \item{Nonincreasing.}{\code{vec[1] >= vec[2] >= }\dots \code{ >= vec[k]}.}
  }
  
  A sequence is \emph{monotonic} if it is either nondecreasing or
  nonincreasing.  It is \emph{strictly monotonic} if it is either
  increasing or decreasing.  Note that the function \code{isMonotonic}
  by default checks for strict monotonicity; which is usually more
  useful in the context of Bayesian networks.

  It is often of interest to check if the direction is increasing or
  decreasing.  The function \code{isMonotonic} returns an attribute
  \code{"direction"} which is positive for increasing series and
  negative for decreasing series.
}
\value{

  The functions \code{isIncreasing}, \code{isNondecreasing},
  \code{isDecreasing}, and \code{isNonincreasing} a logical according to
  whether or not the corresponding condition holds for \code{vec}.

  The function \code{isMonotonic} returns a logical value with an
  additional attribute \code{"direction"}.  The expression
  \code{attr(isMonotonic(vec),"direction"} will be positive if the
  series is increasing and negative if it is decreasing.  If all values
  are equal, and \code{strict} is false, the direction will be 0.

  If \code{vec} has length less than 2, then \code{NA} will be returned.
  

}
\author{Russell Almond}
\note{

  Because \code{isMonotonic} returns an extra attribute,
  \code{isTRUE(isMonotonic(vec)} will always be false.  The expression
  \code{isTRUE(c(isMonotonic(vec)))} is likely to be more useful.
}
\seealso{
  \code{\link[base]{sort}}, \code{\link[base]{order}}
}
\examples{

isIncreasing(1:3)
isNondecreasing(c(1,1,3))
isDecreasing(3:1)
isNonincreasing(c(3,1,1))

isMonotonic(1:3)
isMonotonic(c(1,1,3))  #FALSE
isMonotonic(c(1,1,3),strict=FALSE)
isMonotonic(3:1)
isMonotonic(c(3,3,1),strict=FALSE)
isMonotonic(c(0,0,0)) #FALSE
isMonotonic(c(0,0,0),FALSE) #TRUE
isMonotonic(c(3,0,3)) # FALSE

}
\keyword{ arith }

