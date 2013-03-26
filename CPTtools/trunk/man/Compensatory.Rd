\name{Compensatory}
\alias{Compensatory}
\alias{Conjunctive}
\alias{Disjunctive}
\title{DiBello--Samejima combination function}
\description{
  These functions take a vector of \dQuote{effective theta} values for a
  colletion of parent variables and calculates the effective theta value
  for the child variable according to the named rule.  Used in
  calculating DiBello--Samejima and DiBello--Normal probability tables.
  These all have one slope parameter (alpha) per parent variable.
}
\usage{
Compensatory(theta, alphas, beta)
Conjunctive(theta, alphas, beta)
Disjunctive(theta, alphas, beta)
}
\arguments{
  \item{theta}{A matrix of effective theta values whose columns correspond to
    parent variables and whos rows correspond to possible skill profiles.}
  \item{alphas}{A vector of discrimination parameters in the same order
    as the columns of \code{theta}.  (Note these function expect discrimination
    parameters and not log discrimination parameters as used in
    \code{calcDSTable}.)} 
  \item{beta}{A difficulty (-intercept) parameter.}
}
\details{
  For \code{Compensatory}, the combination function for each row is:
  \deqn{(alphas[1]*theta[1] + ... + alphas[K]*theta[K])/sqrt(K) - beta}
  where \eqn{K} is the number of parents.  (The \eqn{\sqrt{K}}{sqrt(K)}
  is a variance stabilization parameter.)

  For \code{Conjunctive}, the combination function for each row is:
  \deqn{min(alphas[1]*theta[1], ..., alphas[K]*theta[K]) - beta}
  
  For \code{Disjunctive}, the combination function for each row is:
  \deqn{max(alphas[1]*theta[1], ..., alphas[K]*theta[K]) - beta}

}
\value{
  A vector of normal deviates corresponding to the effective theta
  value.  Length is the number of rows of \code{thetas}.
}
\references{
  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and
  Yan, D. (Draft) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 8.

  Almond, R.G., DiBello, L., Jenkins, F., Mislevy, R.J.,  
Senturk, D., Steinberg, L.S. and Yan, D. (2001) Models for Conditional
Probability Tables in Educational Assessment.  \emph{Artificial
Intelligence and Statistics 2001}  Jaakkola and Richardson (eds).,
Morgan Kaufmann, 137--143.  

}
\author{Russell Almond}
\seealso{\code{\link{effectiveThetas}},\code{\link{calcDSTable}},
  \code{\link{calcDNTable}}, \code{\link{calcDPCTable}},
  \code{\link{calcDGRTable}}, \code{\link{OffsetConjunctive}},
  \code{\link{eThetaFrame}}
}
\note{
  These functions are meant to be used as structure functions in the
  DiBello--Samejima and DiBello--Normal models.  Other structure
  functions are possible and can be excepted by those functions as long
  as they have the same signature as these functions.
}
\examples{
  thetas <- expand.grid(list(S1=(3:1 -2), S2 = (3:1 -2)))
  Compensatory(thetas, c(S1=1.25,S2=.75), 0.33)
  Conjunctive(thetas, c(S1=1.25,S2=.75), 0.33)
  Disjunctive(thetas, c(S1=1.25,S2=.75), 0.33)

  skill <- c("High","Medium","Low")
  eThetaFrame(list(S1=skill,S2=skill), c(S1=1.25,S2=.75), 0.33, "Compensatory")
  eThetaFrame(list(S1=skill,S2=skill), c(S1=1.25,S2=.75), 0.33, "Conjunctive")
  eThetaFrame(list(S1=skill,S2=skill), c(S1=1.25,S2=.75), 0.33, "Disjunctive")
}
\keyword{distribution}% at least one, from doc/KEYWORDS

