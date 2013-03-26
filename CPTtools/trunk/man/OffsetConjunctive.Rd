\name{OffsetConjunctive}
\alias{OffsetDisjunctive}
\alias{OffsetConjunctive}
\title{
  Conjunctive combination function with one difficulty per parent.
}
\description{
  These functions take a vector of \dQuote{effective theta} values for a
  colletion of parent variables and calculates the effective theta value
  for the child variable according to the named rule.  Used in
  calculating DiBello--Samejima and DiBello--Normal probability tables.
  These versions have a single slope parameter (alpha) and one
  difficulty parameter per parent variable.
}
\usage{
OffsetConjunctive(theta, alpha, betas)
OffsetDisjunctive(theta, alpha, betas)
}
\arguments{
  \item{theta}{A matrix of effective theta values whose columns correspond to
    parent variables and whos rows correspond to possible skill profiles.}
  \item{alpha}{A single common discrimination parameter.  (Note these
    function expect discrimination parameters and not log discrimination
    parameters as used in \code{\link{calcDSTable}}.)} 
  \item{betas}{A vector of difficulty (-intercept) parameters.  Its
    length should be the same as the number of columns in \code{theta}.}
}
\details{
  For \code{OffsetConjunctive}, the combination function for each row is:
  \deqn{alpha*min(theta[1]-betas[1], ..., theta[K]-beta[K])}

  For \code{OffsetDisjunctive}, the combination function for each row is:
  \deqn{alpha*max(theta[1]-betas[1], ..., theta[K]-beta[K])}

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
\seealso{
  \code{\link{effectiveThetas}},\code{\link{calcDSTable}},
  \code{\link{calcDNTable}},\code{\link{calcDPCTable}},
  \code{\link{calcDGRTable}}, \code{\link{Compensatory}},
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
  OffsetDisjunctive(thetas, 1.0, c(S1=0.25,S2=-0.25))
  OffsetConjunctive(thetas, 1.0, c(S1=0.25,S2=-0.25))
  eThetaFrame(list(S1=skill,S2=skill), 1.0, c(S1=0.25,S2=-0.25),
              "OffsetConjunctive")
  eThetaFrame(list(S1=skill,S2=skill), 1.0, c(S1=0.25,S2=-0.25),
              "OffsetDisjunctive")
}
% R documentation directory.
\keyword{distribution}

