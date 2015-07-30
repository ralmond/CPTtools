\name{calcNoisyOrTable}
\alias{calcNoisyOrTable}
\alias{calcNoisyOrFrame}
\title{Calculate the conditional probability table for a Noisy-Or distribution}
\description{
  Calculates the conditional probability table for a noisy-and
  distribution.  This follows a logical model where at least one inputs
  must be true for the output to be true; however, some "noise" is
  allowed that produces random deviations from the pure logic. 
}
\usage{
calcNoisyOrTable(skillLevels, obsLevels = c("True", "False"),
                 suppression = rep(0, length(skillLevels)), noGuess = 1,
                 thresholds = sapply(skillLevels, function(states) states[1]))
calcNoisyOrFrame(skillLevels, obsLevels = c("True", "False"),
                 suppression = rep(0, length(skillLevels)), noGuess = 1,
                 thresholds = sapply(skillLevels, function(states) states[1]))
}
\arguments{
  \item{skillLevels}{A list of character vectors giving names of levels
    for each of the condition variables.}
  \item{obsLevels}{A character vector giving names of levels for the
    output variables from highest to lowest.  As a special case, can
    also be a vector of integers.  Its length should be 2, and the first
    value is considered to be logically equivalent to "true".}
  \item{suppression}{A vector of the same length as \code{skillLevels}.  For
    each parent variable, this represents the probability that the process
    will act as if that input condition is not met, even if it is met.}
  \item{noGuess}{A scalar value between 0 and 1.  This represents the
    probability that the the output will be false even when all of the
    inputs are false (e.g., 1-guessing probability).}
  \item{thresholds}{If the input variables have more than two states,
    values that are equal to or higher than this threshold are
    considered true.  It is assumed that the states of the variables are
    ordered from highest to lowest.}
}
\details{
  The noisy-or distribution assumes that both the input and output
  variables are binary.  Basically, the output should be true if any of
  the inputs are true.  Let \eqn{S_k = 1} if the \eqn{k}th input is
  true, and let \eqn{q_k} be the \code{suppression} parameter
  corresponding to the \eqn{k}th input variable.  (If the \eqn{S_k}'s
  represent a skill, then \eqn{q_k} represents the probability that an
  examinee who has that skill will fail to correctly apply it.)
  Then the probability of the true state for the output
  variable will be:
  \deqn{\Pr(X=True|{\bf S}) = 1 - q_0 \prod_k q_k^{1-S_k},}
  where \eqn{q_0} (the \code{noGuess} parameter) is the probability that
  the output will be false even when all of the inputs are false.

  It is assumed that all variables are ordered from highest to lowest
  state, so that the first state corresponds to "true" the others to
  false.  If the input variable has more than two states, then it can be
  reduced to a binary variable by using the \code{threshold} argument.
  Any values which are equal to or higher than the \code{threshold} for
  that variable are assumed to be true.  (In this case, higher means
  closer to the the beginning of the list of possible values.)

}
\value{
  For \code{calcNoisyOrTable}, a matrix whose rows correspond configurations
  of the parent variable states (\code{skillLevels}) and whose columns
  correspond to \code{obsLevels}.  Each row of the table is a
  probability distribution, so the whole matrix is a conditional
  probability table.  The order of the parent rows is the same as is
  produced by applying \code{expand.grid} to \code{skillLevels}.

  For \code{calcNoisyOrFrame} a data frame with additional columns
  corresponding to the entries in \code{skillLevels} giving the parent
  value for each row.

}
\references{
  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and
  Yan, D. (Draft) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 8.

  Pearl, J. (1988) \emph{Probabilistic Reasoning in Intelligent Systems:
  Networks of Plausible Inference.}  Morgan Kaufmann.

  Diez, F. J. (1993) Parameter adjustment in Bayes networks.  The
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
  This is related to the DINO and NIDO models, but uses a slightly
  different parameterization.  In particular, if the \code{noSlip}
  parameter is omitted, it is a noisy input deterministic and-gate
  (NIDO), and if the \code{bypass} parameters are omitted, it is similar
  to a deterministic input noisy and-gate (DINO), except is lacks a
  slip parameter.
}
\seealso{
  \code{\link{calcDSTable}}, \code{\link{calcDNTable}},
  \code{\link{calcDPCTable}}, \code{\link[base]{expand.grid}},
  \code{\link{calcNoisyOrTable}}
}
\examples{

## Logical or table
or <- calcNoisyOrFrame(list(c("True","False"),c("True","False")),
                                 c("Right","Wrong"))
stopifnot (all(or$Right==c(1,1,1,0)))


## DINO, logical-or except that is allows for a small chance of slipping.
dino <- calcNoisyOrFrame(list(c("True","False"),c("True","False")),
                          noGuess=.9)
stopifnot (all(abs(dino$True-c(1.0,1.0,1.0,.1))<.0001))


##NIDO, logical-or except that inputs can randomly be bypassed
nido <- calcNoisyOrFrame(list(c("True","False"),c("True","False")),
                          suppression=c(.3,.4))
stopifnot (all(abs(nido$True-c(.88,.6,.7,0))<.0001))

##Full Noisy Or distribution
noisyOr <- calcNoisyOrFrame(list(c("True","False"),c("True","False")),
                          noGuess=.9,suppression=c(.3,.4))

stopifnot (all(abs(noisyOr$False-c(.12,.4,.3,1)*.9)<.0001))


thresh <- calcNoisyOrFrame(list(c("H","M","L"),c("H","M","L")),
                                 c("Right","Wrong"),
                                 threshold=c("M","H"))
stopifnot (all(thresh$Right==c(1,1,1, 1,1,0, 1,1,0)))

}
\keyword{ distribution }


