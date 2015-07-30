\name{parseProbVec}
\alias{parseProbVec}
\alias{parseProbVecRow}
\title{Parses Probability Vector Strings}
\description{
This takes a bunch of strings of the form \code{"[High:.3,Med:.5,Low:.2]"}
and parses it into a vector \code{c(High=.3,Med=.5,Low=.2)}.  
}
\usage{
parseProbVec(pVec)
parseProbVecRow(splitrow)
}
\arguments{
  \item{pVec}{A string of the form \code{"[High:.3,Med:.5,Low:.2]"}}
  \item{splitrow}{A collection of strings \code{"High:.3"},
    \code{"Med:.5"}, \code{"Low:.2"}.}
}
\details{
  StatShop outputs marginal distributions in the format
  \code{[\var{state0}:\var{val0},\var{state1}:\var{val1},...]}.  This
  function takes a vector of strings containing probability vectors and
  parses them, returning a matrix of the values, with column names given
  by the names of the states.

  The function \code{parseProbVecRow()} is
  an internal function which parses a single row (after it has been
  split on the commas).
}
\value{
  A matrix containing the values.  The rows correspond to the elements
  of \code{pVec}.  The columns correspond to the state names.
}
\references{
  \url{http://research.ets.org/~ralmond/StatShop/dataFormats.html}
}
\author{Russell Almond}
\seealso{\code{\link{readHistory}}}
\examples{
  parseProbVec(c(Good = "[High:.8,Med:.15,Low:.05]",
                 Bad = "[High:.15,Med:.35,Low:.5]",
                 Ugly = "[High:.01,Med:.09,Low:.9]"))
}
\keyword{interface}
