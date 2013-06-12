\name{calcDSTable}
\alias{calcDSTable}
\alias{calcDSFrame}
\title{Creates the probability table for DiBello--Samejima distribution}
\description{
  The \code{calcDSTable} function takes a description of input and
  output variables for a Bayesian network distribution and a colleciton
  of IRT-like parameter (discrimination, difficulty) and caluclates a
  conditional probability table using the DiBello-Samejima distribution
  (see Details).  The \code{calcDSFrame} function
  returns the value as a data frame with labels for the parent states.
}
\usage{
calcDSTable(skillLevels, obsLevels, lnAlphas, beta, dinc = 0,
            rule = "Compensatory")
calcDSFrame(skillLevels, obsLevels, lnAlphas, beta, dinc = 0,
            rule = "Compensatory")
}
\arguments{
  \item{skillLevels}{A list of character vectors giving names of levels
    for each of the condition variables.}
  \item{obsLevels}{A character vector giving names of levels for the
    output variables from highest to lowest.  As a special case, can
    also be a vector of integers.}
  \item{lnAlphas}{A vector of log slope parameters.  Its length should
    be either 1 or the length of \code{skillLevels}, depending on the
    choice of \code{rule}.}
  \item{beta}{A vector of difficulty (-intercept) parameters. Its length
    should be either 1 or the length of \code{skillLevels}, depending on
    the choice of \code{rule}.}
  \item{dinc}{Vector of difficulty increment parameters (see Details).}
  \item{rule}{Function for computing effective theta (see Details).}
}
\details{
  The DiBello--Samejima model is a mechanism for creating conditional
  probability tables for Bayesian network models using IRT-like
  parameters.  The basic procedure unfolds in three steps.
  \enumerate{
    \item{}{Each level of each input variable is assigned an
      \dQuote{effective theta} value --- a normal value to be used in
      calculations.}
    \item{}{For each possible skill profile (combination of states of
      the parent variables) the effective thetas are combined using a
      combination function.  This produces an \dQuote{effective theta}
      for that skill profile.}
    \item{}{The effective theta is input into Samejima's graded-response
      model to produce a probability distrubution over the states of the
      outcome variables.}
  }

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

  The Samejima graded response link function describes a series of
  curves:  \deqn{P_m(\theta) = Pr(X >= x-m | \theta) = logit^{-1} (\theta
  - d_m)}%{P_m(theta) = Pr(X >= x_m | theta) = logit^-1 D*(theta -d_m)}
  for \eqn{m>1}, where \eqn{D=1.7} (a scale factor to make the logistic
  curve match more closely with the probit curve).  The probability for
  any given category is then the difference between two adjacent
  logistic curves.  Note that because a difficulty parameter was
  included in the structure function, we have the further constraint
  that \eqn{\sum d_m =0}{sum(d_m) = 0}.

  To remove the parameter restriction we work with the difference
  between the parameters:  \eqn{d_m-d_{m-1}}.  The value of \eqn{d_2}
  is set at \code{-sum(dinc)/2} to center the d values.  Thus the
  \code{dinc} parameter (which is required only if
  \code{length(obsLevels)>2}) should be of length
  \code{length(obsLevels)-2}.  The first value is the difference between
  the d values for the two highest states, and so forth.

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
  For \code{calcDSTable}, a matrix whose rows correspond configurations
  of the parent variable states (\code{skillLevels}) and whose columns
  correspond to \code{obsLevels}.  Each row of the table is a
  probability distributon, so the whole matrix is a conditional
  probability table.  The order of the parent rows is the same as is
  produced by applying \code{expand.grid} to \code{skillLevels}.

  For \code{calcDSFrame} a data frame with additional columns
  corresponding to the entries in \code{skillLevels} giving the parent
  value for each row.
}
\note{
  This distribution class is not suitable for modeling relationship
  among proficiency variable, primarily because the normal mapping used
  in the effective theta calculation and the Samejima graded response
  models are not inverses.  For those model, the function 
  \code{\link{calcDNTable}}, which uses a probit link function, is
  recommended instead. 
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

Samejima, F. (1969) Estimation of latent ability using a
response pattern of graded scores.  \emph{Psychometrika Monograph No.
17}, \bold{34}, (No. 4, Part 2).

}
\author{Russell Almond}
\seealso{\code{\link{effectiveThetas}},\code{\link{Compensatory}},
  \code{\link{OffsetConjunctive}},\code{\link{eThetaFrame}},
  \code{\link{calcDNTable}}, \code{\link{calcDSllike}},
  \code{\link{calcDPCTable}}, \code{\link[base]{expand.grid}}
}
\examples{
## Set up variables
skill1l <- c("High","Medium","Low") 
skill2l <- c("High","Medium","Low","LowerYet") 
correctL <- c("Correct","Incorrect") 
gradeL <- c("A","B","C","D","E") 

cptCorrect <- calcDSTable(list(S1=skill1l,S2=skill2l),correctL,
                          log(c(S1=1,S2=.75)),1.0,rule="Conjunctive")

cpfCorrect <- calcDSFrame(list(S1=skill1l,S2=skill2l),correctL,
                          log(c(S1=1,S2=.75)),1.0,rule="Conjunctive")


cptGraded <- calcDSTable(list(S1=skill1l),gradeL, 0.0, 0.0, dinc=c(.3,.4,.3))



}
\keyword{distribution}

