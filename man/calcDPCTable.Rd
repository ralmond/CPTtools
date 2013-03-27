\name{calcDPCTable}
\alias{calcDPCTable}
\alias{calcDPCFrame}
\title{Creates the probability table for the discrete partial credit model}
\description{
  The \code{calcDPCTable} function takes a description of input and
  output variables for a Bayesian network distribution and a colleciton
  of IRT-like parameter (discrimination, difficulty) and caluclates a
  conditional probability table using the discrete partial credit distribution
  (see Details).  The \code{calcDPCFrame} function
  returns the value as a data frame with labels for the parent states.

}
\usage{
calcDPCTable(skillLevels, obsLevels, lnAlphas, betas, rules = "Compensatory")
calcDPCFrame(skillLevels, obsLevels, lnAlphas, betas, rules = "Compensatory")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{skillLevels}{A list of character vectors giving names of levels
    for each of the condition variables.}
  \item{obsLevels}{A character vector giving names of levels for the
    output variables from highest to lowest.  As a special case, can
    also be a vector of integers.}
  \item{lnAlphas}{A list of vectors of log slope parameters.  Its
    length should be 1 or \code{length(obsLevels)-1}.  The required
    length of the individual component vectors depends on the choice of
    \code{rule} (and is usually either 1 or the length of
    \code{skillLevels}).}
  \item{betas}{A list of vectors of difficulty (-intercept) parameters.  Its
    length should be 1 or \code{length(obsLevels)-1}.  The required
    length of the individual component vectors depends on the choice of
    \code{rule} (and is usually either 1 or the length of
    \code{skillLevels}).}
  \item{rules}{A list of functions for computing effective theta (see
    Details). Its length should be \code{length(obsLevels)-1} or 1
    (implying that the same rule is applied for every gap.}
}
}
\details{
  The discrete graded response model is a generalization of the
  DiBello--Samejima mechanism for creating conditional
  probability tables for Bayesian network models using IRT-like
  parameters (\code{\link{calcDSTable}}).  The basic procedure unfolds
  in three steps. 
  \enumerate{
    \item{}{Each level of each input variable is assigned an
      \dQuote{effective theta} value --- a normal value to be used in
      calculations.}
    \item{}{For each possible skill profile (combination of states of
      the parent variables) the effective thetas are combined using a
      combination function.  This produces an \dQuote{effective theta}
      for that skill profile.}
    \item{}{The effective theta is input into the generalized partial
      credit link function (Muraki, 1992) to produce a probability
      distrubution over the states of the outcome variables.}
  }

  The parent (conditioning) variables are described by the
  \code{skillLevels} argument which should provide for each parent
  variable in order the names of the states ranked from highest to
  lowest value.  The current implementation uses the function
  \code{\link{effectiveThetas}} to caluclate equally spaced points on
  the normal curve.

  Let \eqn{X} be the child variable of the distribution, and assume that
  it can take on \eqn{M} possible states labeled \eqn{x_1} through
  \eqn{x_M} in increasing order.  The generalized partial
  credit model defines a set of functions
  \eqn{Z_m(\theta_1,\ldots,\theta_K)} for \eqn{m=2,...M}, where \deqn{
    Pr(X >= x_m | X >=x_{m-1}, \theta_1,...,\theta_K) = logit^{-1}
    -D*Z_m(\theta_1,\ldots,\theta_K)}
  The conditional probabilities for each child state given the effective
  thetas for the parent variables is then given by
  \deqn{Pr(X == x_m |\theta_1,...,\theta_K) \frac{\sum_{r=1}^m
  Z_r(\theta_1,\ldots,\theta_K)}{\sum_{r=1}^M
  Z_r(\theta_1,\ldots,\theta_K)}}

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (skillLevels, obsLevels, lnAlphas, betas, rules = "Compensatory") 
{
    k <- length(obsLevels)
    if (!is.list(lnAlphas)) 
        lnAlphas <- list(lnAlphas)
    if (length(lnAlphas) != k - 1) 
        lnAlphas <- rep(lnAlphas, k - 1)
    if (!is.list(betas)) 
        betas <- list(betas)
    if (length(betas) != k - 1) 
        betas <- rep(betas, k - 1)
    if (!is.list(rules)) 
        rules <- list(rules)
    if (length(rules) != k - 1) 
        rules <- rep(rules, k - 1)
    pdims <- sapply(skillLevels, length)
    tvals <- lapply(pdims, effectiveThetas)
    thetas <- do.call("expand.grid", tvals)
    pt <- matrix(0, nrow(thetas), k - 1)
    for (kk in 1:(k - 1)) {
        etheta <- do.call(rules[[kk]], list(thetas, exp(lnAlphas[[kk]]), 
            betas[[kk]]))
        pt[, kk] <- exp(1.7 * etheta)
    }
    partialCredit(pt, k, obsLevels)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
