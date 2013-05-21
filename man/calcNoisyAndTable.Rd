\name{calcNoisyAndTable}
\alias{calcNoisyAndTable}
\alias{calcNoisyMinTable}
\alias{calcNoisyAndFrame}
\alias{calcNoisyMinFrame}
\title{Calculate the conditional probability table for a Noisy-And or
  Noisy-Min distribution}
\description{
  Calculates the conditional probability table for a noisy-and
  distribution.  This follows a logical model where all inputs must be
  true for the output to be true; however, some "noise" is allowed that
  produces random deviations from the pure logic.  The noisy-min is a
  generalization in which all variables are ordered, and the weakest of
  the parent variables drives the conditional probabilities of the child
  variable. 
}
\usage{
calcNoisyAndTable(skillLevels, obsLevels = c("True", "False"),
                  bypass = rep(0, length(skillLevels)), noslip=1,
                  thresholds = sapply(skillLevels, function(states) states[1]))
calcNoisyAndFrame(skillLevels, obsLevels = c("True", "False"),
                  bypass = rep(0, length(skillLevels)), noslip=1,
                  thresholds = sapply(skillLevels, function(states) states[1]))
}
\arguments{
  \item{skillLevels}{A list of character vectors giving names of levels
    for each of the condition variables.}
  \item{obsLevels}{A character vector giving names of levels for the
    output variables from highest to lowest.  As a special case, can
    also be a vector of integers.  Its length should be 2, and the first
    value is considered to be logically equivalent to "true".}
  \item{noSlip}{A scalar value between 0 and 1.  This represents the
    probability that the output will be true when all of the inputs are
    true (e.g., 1 - the probability that an examinee will make a
    careless error).} 
  \item{bypass}{A vector of the same length as \code{skillLevels}.  For
    each parent variable, this represents the probabity that the process
    will act as if that input condition is met, even if it is not met.}
  \item{thresholds}{If the input variables have more than two states,
    values that are equal to or higher than this threshold are
    considered true.  It is assumed that the states of the variables are
    ordered from highest to lowest.}
}
\details{

  The noisy-and distribution assumes that both the input and output
  variables are binary.  Basically, the output should be true if all of
  the inputs are true.  Let \eqn{S_k = 1} if the \eqn{k}th input is
  true, and let \eqn{r_k} be the \code{bypass} parameter corresponding
  to the \eqn{k}th input variable.  (If the \eqn{S_k}'s represent a
  skill, then \eqn{r_k} represents the probability that an examinee who
  lacks that skill will bypass the need for that skill in solving the
  problem.) Then the probability of the true state for the output
  variable will be:
  \deqn{\Pr(X=True|{\bf S}) = r_0 \prod_k r_k^{1-S_k},}
  where \eqn{r_0} (the \code{noSlip} parameter) is the probability that
  the output will be true even when all of the inputs are true.

  It is assumed that all variables are ordered from highest to lowest
  state, so that the first state corresponds to "true" the others to
  false.  If the input variable has more than two states, then it can be
  reduced to a binary variable by using the \code{threshold} argument.
  Any values which are equal to or higher than the \code{threshold} for
  that variable are assumed to be true.  (In this case, higher means
  closer to the the begining of the list of possible values.)

  The noisy-min is a generalization

  
}
\value{

  For \code{calcNoisyAndTable}, a matrix whose rows correspond configurations
  of the parent variable states (\code{skillLevels}) and whose columns
  correspond to \code{obsLevels}.  Each row of the table is a
  probability distributon, so the whole matrix is a conditional
  probability table.  The order of the parent rows is the same as is
  produced by applying \code{expand.grid} to \code{skillLevels}.

  For \code{calcNoisyAndFrame} a data frame with additional columns
  corresponding to the entries in \code{skillLevels} giving the parent
  value for each row.

}
\references{
  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and
  Yan, D. (Draft) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 8.

  Pearl, J. (1988) \emph{Probabilistic Reasoning in Intelligent Systems:
  Networks of Plausible Inference.}  Morgan Kaufmann.

  Diez, F. J. (1993) Parameter adjustement in Bayes networks.  The
  generalized noisy OR-gate.  In Heckerman and Mamdani (eds)
  \emph{Uncertainty in Artificial Intelligence 93.}  Morgan Kaufmann.
  99--105. 

  Srinivas, S. (1993) A generalization of the Noisy-Or model, the
  generalized noisy OR-gate.  In Heckerman and Mamdani (eds)
  \emph{Uncertainty in Artificial Intelligence 93.}  Morgan Kaufmann.
  208--215. 

}
\author{Russell Almond}
\note{
  This is related to the DINA and NIDA models, but uses a slightly
  different parameterization.  In particular, if the \code{noSlip}
  parameter is omitted, it is a noisy input deterministic and-gate
  (NIDA), and if the \code{bypass} parameters are omitted, it is similar
  to a deterministic input noisy and-gate (DINA), except is lacks a
  guessing parameter.
}
\seealso{
  \code{\link{calcDSTable}}, \code{\link{calcDNTable}},
  \code{\link{calcDPCTable}}, \code{\link[base]{expand.grid}},
  \code{\link{calcNoisyOrTable}}, \code{\link{calcNoisyGateTable}}
}
\examples{

## Logical and table
and <- calcNoisyAndFrame(list(c("True","False"),c("True","False")),
                                 c("Right","Wrong"))
stopifnot (all(and$Right==c(1,0,0,0)))


## DINA, logical-and except that is allows for a small chance of slippling.
dina <- calcNoisyAndFrame(list(c("True","False"),c("True","False")),
                          noSlip=.9)
stopifnot (all(abs(dina$Right-c(.9,0,0,0))<.0001))

##NIDA, logical-and except that inputs can randomly be bypassed
nida <- calcNoisyAndFrame(list(c("True","False"),c("True","False")),
                          bypass=c(.3,.4))
stopifnot (all(abs(nida$Right-c(1,.3,.4,.12))<.0001))

##Full Noisy And distribution
noisyAnd <- calcNoisyAndFrame(list(c("True","False"),c("True","False")),
                          noSlip=.9,bypass=c(.3,.4))

stopifnot (all(abs(noisyAnd$Right-c(1,.3,.4,.12)*.9)<.0001))


thresh <- calcNoisyAndFrame(list(c("H","M","L"),c("H","M","L")),
                                 c("Right","Wrong"),
                                 threshold=c("M","H"))
stopifnot (all(thresh$Right==c(1,1,0, 0,0,0, 0,0,0)))

}
\keyword{ distribution }

