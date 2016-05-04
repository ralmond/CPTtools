\name{isOffsetRule}
\alias{isOffsetRule}
\alias{getOffsetRules}
\alias{setOffsetRules}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Distinguishes Offset from ordinary rules.}
\description{
  An \emph{offset rule} is one where there is one intercept ($\beta$)
  parameter for each parent and there is a single slope
  parameters. As opposed to a regression-style rule where there is a
  different slope ($\alpha$) for each parent and single intercept.  This
  function distinguishes between the twho types of rules.
}
\usage{
isOffsetRule(rl)
getOffsetRules()
setOffsetRules(newval)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rl}{A character vector of rule names to test to see if these are
    offset rules.}
  \item{newval}{A character vector of rule names to be considered as
    offset rules.}
}
\details{

  The \code{\link{Compensatory}} rule acts more or less like a
  regression, with a slope (or discrimination) parameter for each parent
  variable, and a single intercept or difficulty parameter.  The
  \code{Conjunctive} and \code{Disjunctive} follow the same pattern. In
  contrast the \code{\link{OffsetConjunctive}} rule, has a different
  intercept for each parent and a single slope parameter.  The
  \code{OffsetDisjunctive} rule follows the same pattern.

  The \code{isOffsetRule()} is true if the argument references a
  function which follows the offset parameterization and false if it
  follow the regression parameterization.  Currently it returns true
  only for \dQuote{OffsetConjunctive}, and \dQuote{OffsetDisjunctive},
  but using this test should continue to work if the number of rules in
  CPTtools expands.

  The expression \code{getOffsetRules()} returns the list of currently
  known offset-style rules.  The function \code{setOffsetRules()} allows
  this list to be manipulated to add a new rule to the list of
  offset-style rules.
}
\value{
  The expression \code{isOffsetRule(rl)} returns a logical vector of the
  same length as \code{rl}, with each element \code{TRUE} or
  \code{FALSE} depending on whether or the corresponding element of
  \code{rl} is the name of an offset rule.  If \code{rl} is not a
  character vector, then the function returns \code{FALSE}.

  The expression \code{getOffsetRule()} returns the names of the current
  offset rules.
}
\author{Russell G. Almond}
\note{
  It makes sense in certain situation to use anonymous function objects
  as rules, however, it is impossible to test whether or not a function
  is in the offset rule list.  Therefore, it is recommended that only
  rule names be used with this function.

  One consequence of this rule is that when given a function argument,
  \code{isOffsetRule} returns \code{FALSE}.
}
\seealso{
  \code{\link{Compensatory}}, \code{\link{OffsetConjunctive}}, 
}
\examples{

stopifnot(
all(isOffsetRule(c("OffsetConjunctive","Conjunctive"))==c(TRUE,FALSE)),
isOffsetRule(OffsetDisjunctive)==TRUE,
all(getOffsetRules() == c("OffsetConjunctive","OffsetDisjunctive"))
)

setOffsetRules(c(getOffsetRules(),"myOffsetRule"))
stopifnot (isOffsetRule("myOffsetRule"))


}
\keyword{ manip }

