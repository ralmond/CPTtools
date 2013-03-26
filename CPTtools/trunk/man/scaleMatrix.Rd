\name{scaleMatrix}
\alias{scaleMatrix}
\title{Scales a matrix to have a unit diagonal}
\description{
  Creates a correlation matrix from a covariance matrix by scaling rows
  and columns to have a unit diagonal.  Also can be used to create a
  partial correlation matrix from an inverse covariance/correlation
  matrix. 
}
\usage{
scaleMatrix(X)
}
\arguments{
  \item{X}{A square, positive definite matrix (covariance matrix).}
}
\details{
  Divides rows and columns by square root of the diagonal elements.
}
\value{
  A matrix of the same size and shape as the original with a unit diagonal.
}
\author{Russell Almond}
\examples{
data(MathGrades)

## Create a correlation matrix from a covariance matrix.
round(scaleMatrix(MathGrades$var),2) == MathGrades$cor
\dontshow{
stopifnot(
 all(round(scaleMatrix(MathGrades$var),2) == MathGrades$cor)
)
}
## Create a partial correlation matrix from a correlation matrix
round(scaleMatrix(solve(MathGrades$cor)),2) == MathGrades$pcor
##Note:  Some of these tests may return false due to machine precision
##issues. 

}
\keyword{manip}

