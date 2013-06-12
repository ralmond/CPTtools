\name{eThetaFrame}
\alias{eThetaFrame}
\title{Constructs a data frame showing the effective thetas for each
  parent combination.}
\description{
  This evaluates the combination function but not the link function of
  of an effective theta distribution.  It produces a table of effective
  thetas one for each configuration of the parent values acording to the
  combination function given in the \code{model} argument.
}
\usage{
eThetaFrame(skillLevels, lnAlphas, beta, rule = "Compensatory")
}
\arguments{
  \item{skillLevels}{A list of character vectors giving names of levels
    for each of the condition variables.}
  \item{lnAlphas}{A vector of log slope parameters.  Its length should
    be either 1 or the length of \code{skillLevels}, depending on the
    choice of \code{rule}.}
  \item{beta}{A vector of difficulty (-intercept) parameters. Its length
    should be either 1 or the length of \code{skillLevels}, depending on
    the choice of \code{rule}.}
  \item{rule}{Function for computing effective theta (see Details).}
}
\details{
  The DiBello framework for creating for creating conditional
  probability tables for Bayesian network models using IRT-like
  parameters unfolds in three steps.
  \enumerate{
    \item{}{Each level of each input variable is assigned an
      \dQuote{effective theta} value --- a normal value to be used in
      calculations.}
    \item{}{For each possible skill profile (combination of states of
      the parent variables) the effective thetas are combined using a
      combination function.  This produces an \dQuote{effective theta}
      for that skill profile.  The function \code{rule} determines the
      rule for combination.}
    \item{}{The effective theta is input into a link function (e.g.,
      Samejima's graded-response function) to produce a probability
      distrubution over the states of the outcome variables.}
  }
  This function applies the first two of those steps and returns a data
  frame with the original skill levels and the effective thetas.

    The parent (conditioning) variables are described by the
  \code{skillLevels} argument which should provide for each parent
  variable in order the names of the states ranked from highest to
  lowest value.  The original method (Almond et al., 2001)
  used equally spaced points on the interval \eqn{[-1,1]} for the
  effective thetas of the parent variables.  The current implementation
  uses the function \code{\link{effectiveThetas}} to caluclate equally
  spaced points on the normal curve.

    The combination of the individual effective theta values into a joint
  value for effective theta is done by the function reference by
  \code{rule}.  This should be a function of three arguments:
  \code{theta} --- the vector of effective theta values for each parent,
  \code{alphas} --- the vector of discrimination parameters, and
  \code{beta} --- a scaler value giving the difficulty.  The initial
  distribuiton supplies five functions appropriate for use with
  \code{calcDSTable}:  \code{\link{Compensatory}},
  \code{\link{Conjunctive}}, and \code{\link{Disjunctive}},
  \code{\link{OffsetConjunctive}}, and \code{\link{OffsetDisjunctive}}.
  The last two have a slightly different parameterization:  \code{alpha}
  is assumed to be a scalar and \code{betas} parameter is vector
  valued. Note that the discrimination and difficulty parameters are
  built into the structure function and not the IRT curve.

}
\value{
  For a data frame with one column for each parent variable and an
  additional column for the effective theta values.  The number of rows
  is the product of the number of states in each of the componense of
  the \code{skillLevels} argument.
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
\code{\link{effectiveThetas}},\code{\link{Compensatory}},
  \code{\link{OffsetConjunctive}},\code{\link{calcDNTable}},
  \code{\link{calcDSTable}}, 
  \code{\link{calcDPCTable}}, 
  \code{\link[base]{expand.grid}}
}
\examples{
  skill <- c("High","Medium","Low")
  eThetaFrame(list(S1=skill,S2=skill), log(c(S1=1.25,S2=.75)), 0.33,
             "Compensatory") 
  eThetaFrame(list(S1=skill,S2=skill), log(c(S1=1.25,S2=.75)), 0.33,
              "Conjunctive")
  eThetaFrame(list(S1=skill,S2=skill), log(c(S1=1.25,S2=.75)), 0.33,
              "Disjunctive")
  eThetaFrame(list(S1=skill,S2=skill), log(1.0), c(S1=0.25,S2=-0.25),
              "OffsetConjunctive")
  eThetaFrame(list(S1=skill,S2=skill), log(1.0), c(S1=0.25,S2=-0.25),
              "OffsetDisjunctive")
}
\keyword{ distribution }

