\name{areaProbs}
\alias{pvecToCutpoints}
\alias{pvecToMidpoints}
\alias{areaProbs}
\title{Translates between normal and categorical probabilities}
\description{
  Maps between a continuous (normal) variable and a discrete variable by
  establishing a set of bins to maintain a particular probability
  vector.  The \code{pvecToCutpoints} function returns the cut points
  separating the bins, the \code{pvecToMidpoints} returns a central
  point from each bin, and the \code{areaProbs} calculates the fraction
  of a normal curve falling into a particular bin.
}
\usage{
pvecToCutpoints(pvec, mean = 0, std = 1)
pvecToMidpoints(pvec, mean = 0, std = 1)
areaProbs(pvec, condmean, condstd, mean = 0, std = 1)
}
\arguments{
  \item{pvec}{A vector of marginal probabilities for the categories of
    the discrete variable.  Elements should be ordered from smallest to
    largest.}
  \item{mean}{The mean of the continuous variable.}
  \item{std}{The standard deviation of the continuous variable.}
  \item{condmean}{The conditional mean of the continuous variable.}
  \item{condstd}{The conditional standard devaiation of the continuous
    variable.}
}
\details{
  Let \eqn{S} be a discrete variable whose states
  \eqn{s_k} are given by \code{names(pvec)[k]} and for which the
  marginal probability \eqn{Pr(S=s_k) = p_k} is given by \code{pvec[k]}.
  Let \eqn{Y} be a continous normal variable with mean \code{mean} and
  standard deviation \code{std}.  These function map between \eqn{S} and
  \eqn{Y}.

  The function \code{pvecToCutpoints} produces a series of cutpoints,
  \eqn{c_k}, such that setting \eqn{s_k} to \eqn{S} when \eqn{c_k \le Y
    \le c_{k+1}}{c_k \le Y \le c_(k+1)} produces the marginal probability
  specified by \code{pvec}.  Note that \eqn{c_1} is always \code{-Inf}
  and \eqn{c_{K+1}}{c_(K+1)} is always \code{Inf} (where \eqn{K} is
  \code{length(pvec)}).

  The function \code{pvecToMidpoints} produces the midpoints (with
  respect to the normal density) of the intervals defined by
  \code{pvecToCutpoints}.  In particular, if \eqn{Pr(S \ge s_k) = P_k},
  then the values returned are \eqn{\code{qnorm}(P_k + p_k / 2)}.

  The function \code{areaProbs} inverts these calculations.  If
  \code{condmean} is \eqn{E[Y|x]} and \code{condstd} is
  \eqn{\sqrt{var(Y|x)}}{sqrt{var(Y|x)}}, then this function calculates
  \eqn{Pr(S|x)} by calculating the area under the normal curve.
}
\value{
  For \code{pvecToCutpoints}, a vector of length one greater than
  \code{pvec} giving the endpoints of the bins.  Note that the first and
  last values are always infinite.

  For \code{pvecToCutpoints}, a vector of length the same length as
  \code{pvec} giving the midpoint of the bins.

  For \code{areaProbs} a vector of probabilities of the same length as
  \code{pvec}.  
  
}
\references{
  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and
  Yan, D. (Draft) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 8.

  Almond, R.G. (Draft) Building Bayesian Network Proficiency Models from
Factor Analysis Results.  ETS Research Memorandum (not yet submitted).

}
\author{Russell Almond}
\note{ 
  The function \code{effectiveThetas} does something similar, but
  assumes all probability values are equally weighted.
}
\section{Warning}{
  Variables are given from \emph{lowest} to \emph{highest} state, for
  example \sQuote{Low}, \sQuote{Medium}, \sQuote{High}.  StatShop
  expects variables in the opposite order.
} 

\seealso{\code{\link{effectiveThetas}} }
\examples{
probs <- c(Low=.05,Med=.9,High=.05)
cuts <- pvecToCutpoints(probs)
mids <- pvecToMidpoints(probs)

\dontshow{
stopifnot(
all(round(diff(pnorm(cuts)),2) == probs),
all(round(pnorm(mids),4) == cumsum(probs) - probs/2)
)
}
areaProbs(probs,1,.5)

\dontshow{
stopifnot(all(round(areaProbs(probs,0,1),3) == probs))
}
}
\keyword{manip}% at least one, from doc/KEYWORDS

