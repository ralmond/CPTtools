\name{effectiveThetas}
\alias{effectiveThetas}
\title{Assigns effective theta levels for categorical variable}
\description{
  Calculates a vector of normal quantiles corresponding to effective
  theta levels for a categorical variable for use in a DiBello-Samejima
  distribution. 
}
\usage{
effectiveThetas(nlevels)
}
\arguments{
  \item{nlevels}{Integer giving the number of levels of the categorical
    variable.}
}
\details{
  The DiBello--Samejima models map levels of categorical values into
  effective \dQuote{theta} values, or corresponding continuous values.
  These can then be input into IRT equations to calculate cell
  probabilities.

  The default algorithm works by assuming that the categories are
  created by cutting the normal distribution into equal probability
  intervals.  The value returned for each interval is the midpoint (wrt
  the normal measure) of that interval.
}
\value{
  A vector of normal quantiles of length \code{nlevels}.
}
\references{
  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Yan, D. and Williamson, D.M.
  (2015). \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 8.
}
\author{Russell Almond}
\examples{
  effectiveThetas(5)
}
\keyword{distribution}

