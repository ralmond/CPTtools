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
calcDPCTable(skillLevels, obsLevels, lnAlphas, betas,
             rules = "Compensatory", link="partialCredit", linkScale=NULL)
calcDPCFrame(skillLevels, obsLevels, lnAlphas, betas, 
             rules = "Compensatory", link="partialCredit", linkScale=NULL)
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
    (implying that the same rule is applied for every gap.)}
  \item{link}{The function that converts a table of effective thetas to
    probabilities}
  \item{linkScale}{An optional scale parameter for the \code{link}
    function.  This is only used with certain choices of \code{link}
    function.}  
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
      one of the \code{rule} functions.  This produces an
      \dQuote{effective theta} for that skill profile.}
    \item{}{The effective theta table is input into the \code{link}
      function to produce a probability
      distrubution over the states of the outcome variables.}
  }

  The parent (conditioning) variables are described by the
  \code{skillLevels} argument which should provide for each parent
  variable in order the names of the states ranked from highest to
  lowest value.  The current implementation uses the function
  \code{\link{effectiveThetas}} to caluclate equally spaced points on
  the normal curve.  Applying this rule to each parameter value produces
  a table of rows with values \eqn{\theta_1,\ldots,\theta_K},
  corresponding to all possible combinations of the parent variables
  (using \code{\link[base]{expand.grid}}).

  Let \eqn{X} be the child variable of the distribution, and assume that
  it can take on \eqn{M} possible states labeled \eqn{x_1} through
  \eqn{x_M} in increasing order.  (Note:  that \code{calcDPCTable}
  assumes variable states are ordered the other direction:  from higest
  to lowest.)   For each state but the lowest state (the last one in the
  input order) defines a combination rule
  \eqn{Z_m(\theta_1,\ldots,\theta_K;alphas,betas)}.  Applying these
  functions to the rows of the table produces a table of effective
  thetas for each configuration of the parent variables and each child
  state except for the lowest.  (The metaphor is this theta represents
  the ``ability level'' required to reach that output state.)

  Note that the \eqn{Z_m(\cdot)}s do not need to have the same
  parameters or even the same functional form.  The argument
  \code{rules} should contain a list of the names of the combination
  functions, the first one corresponding to \eqn{Z_M(\cdot)}, and so
  forth in decending order.  As a special case, if \code{rules} has only
  one element, than it is used for all of the transitions.  Similarly,
  the \code{lnAlphas} and \code{betas} should also be lists of the
  parameters of the combination functions corresponding to the
  transitions between the levels.  The \code{betas[[m]]} represent
  difficulties (negative intercepts) and the \code{exp(lnAlphas[[m]])}
  represent slopes for the transition to level \eqn{m} (following the
  highest to lowest order).  Again if these lists have length one, the
  same value is used for all transitions.

  The length of the elements of \code{lnAlphas} and \code{betas} is
  determined by the specific choice of combination function.  The
  functions \code{\link{Compensatory}}, \code{\link{Conjunctive}}, and
  \code{\link{Disjunctive}} all assume that there will be one
  \code{lnAlpha} for each parent variable, but a single \code{beta}.
  The functions \code{\link{OffsetConjunctive}}, and
  \code{\link{OffsetDisjunctive}} both assume that there will be one
  \code{beta} for each parent variable, but a single \code{lnAlpha}.

  The code \code{link} function is then applied to the table of
  effective theta values to produce a conditional probability
  distribution.  Two link functions are currently supported:
  \code{\link{partialCredit}} is based on the generalized partial credit
  model (Muraki, 1992), \code{\link{gradedResponse}} is a modified
  version of the graded response model (Samejima, 1969).  (The
  modification corrects for problems when the curves cross.)  A third
  planned link function is based on a normal error model, this will
  require the extra \code{linkScale} parameter.

  Normally \code{obslevel} should be a character vector giving state
  names.  However, in the special case of state names which are integer
  values, R will \dQuote{helpfully} convert these to legal variable
  names by prepending a letter.  This causes other functions which rely
  on the \code{names()} of the result being the state names to break.
  As a special case, if the value of \code{obsLevel} is of type numeric,
  then \code{calcDSFrame()} will make sure that the correct values are
  preserved. 

}
\value{
  For \code{calcDPCTable}, a matrix whose rows correspond configurations
  of the parent variable states (\code{skillLevels}) and whose columns
  correspond to \code{obsLevels}.  Each row of the table is a
  probability distributon, so the whole matrix is a conditional
  probability table.  The order of the parent rows is the same as is
  produced by applying \code{expand.grid} to \code{skillLevels}.

  For \code{calcDPCFrame} a data frame with additional columns
  corresponding to the entries in \code{skillLevels} giving the parent
  value for each row.
}
\references{
  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and
  Yan, D. (Draft) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 8.

  Muraki, E. (1992).  A Generalized Partial Credit Model:  Application
  of an EM Algorithm.  \emph{Applied Psychological Measurement}, \bold{16},
  159-176.  DOI: 10.1177/014662169201600206

Samejima, F. (1969) Estimation of latent ability using a
response pattern of graded scores.  \emph{Psychometrika Monograph No.
17}, \bold{34}, (No. 4, Part 2).

  I also have planned a manuscript that describes these functions in
  more detail.

}
\author{Russell Almond}
\note{
  The framework set up by this function is completely expandable.  The
  \code{link} and the elements of \code{rules} can be any value that is
  suitable for the first argument of \code{\link[base]{do.call}}.

  Elements of \code{rules} are called with the expression
  \code{do.call(rules[[kk]],list(thetas,exp(lnAlphas[[kk]]),betas[[kk]]))}
  where \code{thetas} is the matrix of effective theta values produced
  in the first step of the algorithm, and the return function should be
  a vector of effective thetas, one for each row of \code{thetas}.  

  The \code{link} function is called with the expression 
  \code{do.call(link,list(et,linkScale,obsLevels))} where \code{et} is
  the matrix of effective thetas produced in the second step.  It should
  return a conditional probability table with the same number of rows
  and one more column than \code{et}.  All of the rows should sum to 1.0.

}
\seealso{
  \code{\link{effectiveThetas}},\code{\link{Compensatory}},
  \code{\link{OffsetConjunctive}},\code{\link{eThetaFrame}},
  \code{\link{calcDNTable}}, \code{\link{calcDSTable}},
  \code{\link[base]{expand.grid}}, \code{\link{gradedResponse}},
  \code{\link{partialCredit}} 
}
\examples{
## Set up variables
skill1l <- c("High","Medium","Low") 
skill2l <- c("High","Medium","Low","LowerYet") 
correctL <- c("Correct","Incorrect") 
pcreditL <- c("Full","Partial","None")
gradeL <- c("A","B","C","D","E") 

## Simple binary model, these three should be the same.
cptCorrect <- calcDPCTable(list(S1=skill1l,S2=skill2l),correctL,
                          log(c(S1=1,S2=.75)),1.0,rule="Compensatory",
                          link="partialCredit")
cptCorrect2 <- calcDPCTable(list(S1=skill1l,S2=skill2l),correctL,
                          log(c(S1=1,S2=.75)),1.0,rule="Compensatory",
                          link="gradedResponse")
cptCorrect1 <- calcDSTable(list(S1=skill1l,S2=skill2l),correctL,
                          log(c(S1=1,S2=.75)),1.0,rule="Compensatory")
stopifnot (all (abs(cptCorrect2-cptCorrect1) <.001))
stopifnot (all (abs(cptCorrect-cptCorrect1) <.001))

## Conjunctive uses multiple betas, not multiple alphas.
cptConj <- calcDPCTable(list(S1=skill1l,S2=skill2l),correctL,
                        log(1),c(S1=0.5,S2=.7),rule="OffsetConjunctive")


## Simple model, Skill 1 needed for step 1, Skill 2 for Step 2.
cptPC1 <- calcDPCFrame(list(S1=skill1l,S2=skill2l),pcreditL,
                        lnAlphas=log(1),
                        betas=list(full=c(S1=0,S2=999),partial=c(S2=999,S2=0)),
                        rule="OffsetDisjunctive")


## Complex model, different rules for different levels
cptPC2 <- calcDPCTable(list(S1=skill1l,S2=skill2l),pcreditL,
                          list(full=log(1),partial=log(c(S1=1,S2=.75))),
                          betas=list(full=c(0,999),partial=1.0),
                          rule=list("OffsetDisjunctive","Compensatory"))

## Graded Response Model, typically uses different difficulties
cptGraded <- calcDPCTable(list(S1=skill1l),gradeL,
                          log(1),betas=list(A=2,B=1,C=0,D=-1),
                          rule="Compensatory",link="gradedResponse")

## Partial credit link is somewhat different
cptPC5 <- calcDPCTable(list(S1=skill1l),gradeL,
                          log(1),betas=list(A=2,B=1,C=0,D=-1),
                          rule="Compensatory",link="partialCredit")
cptPC5a <- calcDPCTable(list(S1=skill1l),gradeL,
                          log(1),betas=1,
                          rule="Compensatory",link="partialCredit")

## Need to be careful when using different slopes (or non-increasing
## difficulties) with graded response link as curves may cross.

cptCross <- calcDPCTable(list(S1=skill1l),pcreditL,
                          log(1),betas=list(full=-1,partial=1),
                          rule="Compensatory",link="gradedResponse")
stopifnot (all(abs(cptCross[,"Partial"])<.001))

}
\keyword{distribution}
