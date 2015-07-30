\name{MathGrades}
\alias{MathGrades}
\docType{data}
\title{Grades on 5 mathematics tests from Mardia, Kent and Bibby}
\description{
  Summary statistics for a data set consisting of marks on five
  mathematics exams originally presented in Mardia, Kent and Bibby
  (1979).  The raw data are not given, but rather the summary statistics
  reported in Whittaker (1990) are provided.
}
\usage{data(MathGrades)}
\format{
  A list consisting of the following components:
  \describe{
    \item{varnames}{Names of the five tests.}
    \item{means}{Average score on each test.}
    \item{var}{Covariance matrix.}
    \item{cor}{Correlation matrix.}
    \item{icdiag}{Diagonal of the inverse covariance matrix.}
    \item{pcor}{Partial correlation (scaled inverse correlation)
      matrix.}
    \item{pvecs}{A set of marginal distributions for discrete variables
      corresponding to the five assessments.}
  }
}
\source{
  Summary statistics reported here are taken from Whittaker (1990).

  Original data on 88 students is reported in Mardia, Kent and Bibby
  (1979).
}
\references{

  Mardia, K.V. and Kent, J.T. and Bibby, J.M. (1979) \emph{Multivariate
    Analysis}. Academic Press.

  Whittaker, J. (1990).  \emph{Graphical Models in Applied Multivariate
    Statistics}.  Wiley.
}
\examples{
data(MathGrades)

##Note:  Some of these tests may return false due to machine precision
##issues. 
round(scaleMatrix(MathGrades$var),2) == MathGrades$cor
round(diag(solve(MathGrades$cor)),2) == MathGrades$icdiag
round(scaleMatrix(solve(MathGrades$cor)),2) == MathGrades$pcor

}
\keyword{datasets}
