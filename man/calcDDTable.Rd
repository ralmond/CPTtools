\name{calcDDTable}
\alias{calcDDTable}
\alias{calcDDFrame}
\title{Calculates DiBello--Dirichlet model probability and parameter tables}
\description{
  The DiBello--Dirichlet model creates a hyper-Dirichlet prior
  distribution by interpolating between an \code{masterProfile} and a
  \code{noviceProfile}.  This function builds the hyper-Dirichlet
  parameter table, or with normalization, the conditional probability
  table for this distribution type.  
}
\usage{
calcDDTable(skillLevels, obsLevels, skillWeights, masterProfile,
            noviceProfile = 0.5, rule = "Compensatory")
calcDDFrame(skillLevels, obsLevels, skillWeights, masterProfile,
            noviceProfile = 0.5, rule = "Compensatory")
}
\arguments{
  \item{skillLevels}{A list of character vectors giving names of levels
    for each of the condition variables.}
  \item{obsLevels}{A character vector giving names of levels for the
    output variables from highest to lowest.  As a special case, can
    also be a vector of integers.}
  \item{skillWeights}{A numeric vector of the same length as
    \code{skillLevels} giving the weight to be applied to each skill.}
  \item{masterProfile}{The Dirichlet prior for \dQuote{experts} (see
    Details). Its length should match \code{obsLevels}.}
  \item{noviceProfile}{The Dirichlet prior for \dQuote{novices} (see
    Details).  Its length should match \code{obsLevels} or as a special
    case a scalar quantity gives a uniform prior.  Default is uniform
    prior with weight 1/2.}
  \item{rule}{Function for computing effective theta (see Details).}
}
\details{
  Assume for the moment that there are two possible skill profiles:
  \dQuote{expert} and \dQuote{novice}.  This model presumes a
  conditional probability table for the outcome given skill profile with
  two rows each of which is an independent categorical distribution.
  The natural conjugate prior is an independent Dirichlet distribution
  for each row.  The parameters for this distribution are given in the
  \code{masterProfile} and \code{noviceProfile} arguments.

  If there is more than one parent variable or if the parent variable
  has more than one state, the situation becomes muddier.  The
  \dQuote{expert} state is obviously the one with all the variables at
  the highest levels and the \dQuote{novice} is the one with all
  variables at the lowest levels.  If we can assign an integer between 0
  and 1 to each of the intermediate states, then we can interpolate
  between them to produce Dirichlet priors for each row.

  This distribution type uses the DiBello effective theta technique to
  come up with the interpolation.  Each parent variable state is
  assigned a \sQuote{theta} value using the
  \code{\link{effectiveThetas}} function to assign a numeric value to
  each one.  These are then combined using the function \code{rule} in
  the rule argument.  The resulting theta values are then scaled to a
  range of 0--1.  The prior for that row is a weighted combination of
  the \code{masterProfile} and \code{noviceProfile}.

    The combination of the individual effective theta values into a joint
  value for effective theta is done by the function reference by
  \code{rule}.  This should be a function of three arguments:
  \code{theta} --- the vector of effective theta values for each parent,
  \code{alphas} --- the vector of discrimination parameters, and
  \code{beta} --- a scalar value giving the difficulty.  The initial
  distribution supplies three functions appropriate for use with
  \code{\link{calcDSTable}}:  \code{\link{Compensatory}},
  \code{\link{Conjunctive}}, and \code{\link{Disjunctive}}.  Note that
  the \code{beta} argument is effectively ignored because of the later
  scaling of the output.  

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
  For \code{calcDDTable}, a matrix whose rows correspond configurations
  of the parent variable states (\code{skillLevels}) and whose columns
  correspond to \code{obsLevels}.  Each row of the table is the
  parameters of a Dirichlet distribution, so the whole matrix is the
  parameters for a hyper-Dirichlet distribution.  The order of the
  parent rows is the same as is produced by applying \code{expand.grid}
  to \code{skillLevels}. 

  For \code{calcDDFrame} a \code{\link{CPF}}, a data frame with additional columns
  corresponding to the entries in \code{skillLevels} giving the parent
  value for each row.
}
\note{
  Unlike \code{\link{calcDSTable}}, there is not a corresponding
  DiBello-Dirichlet distribution support in StatShop.  This function is
  used to model the parameters of an unconstrained hyper-Dirichlet
  distribution.

  This was originally designed for use in Situational Judgment Tests
  where experts might not agree on the \dQuote{key}.

  Note:  Zeros in the \code{masterProfile} indicate responses that a
  master would \strong{never} make.  They will result in zero probability
  of mastery for any response which yields that outcome.
}
\references{
  Almond, R.G. and Roberts, R. (Draft) Bayesian Scoring for Situational
  Judgment Tests.  Unpublished white paper.

  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Yan, D. and Williamson, D.M.
  (2015) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 8.

  Almond, R.G., DiBello, L., Jenkins, F., Mislevy, R.J.,  
Senturk, D., Steinberg, L.S. and Yan, D. (2001) Models for Conditional
Probability Tables in Educational Assessment.  \emph{Artificial
Intelligence and Statistics 2001}  Jaakkola and Richardson (eds).,
Morgan Kaufmann, 137--143.  
}
\seealso{
\code{\link{effectiveThetas}},\code{\link{Compensatory}},
  \code{\link{calcDNTable}}, \code{\link{calcDSTable}},
  \code{\link[base]{expand.grid}}
}
\examples{

  skill1l <- c("High","Medium","Low") 
  skill2l <- c("High","Low")
  option5L <- c("A","B","C","D","E") 

  ## Expert responses
  eProfile <- c(A=7,B=15,C=3,D=0,E=0)

  paramT <- calcDDTable(list(S1=skill1l,S2=skill2l), option5L,
                        c(S1=2,S2=1), masterProfile=eProfile+0.5)

  paramF <- calcDDFrame(list(S1=skill1l,S2=skill2l), option5L,
                        c(S1=2,S2=1), masterProfile=5*eProfile+0.5,
                        noviceProfile=2)

}
\keyword{distribution}
