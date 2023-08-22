\name{defaultAlphas}
\alias{defaultAlphas}
\alias{defaultBetas}
\title{Generates default values for Alpha and Beta parameters}
\description{

  The expected shape of the Alpha and Beta parameters depends on
  the type of rule. If \code{\link{isOffsetRule}(rule)} is true, 
  then the betas should be a vector of the same length as 
  \code{pnames} and alphas should be a scalar.  If it is false,
  then alphas is the vector and betas is the scalar.

}
\usage{
defaultAlphas(rule, pnames, states=c("Yes","No"), 
             link="partialCredit")
defaultBetas(rule, pnames, states=c("Yes","No"), 
             link="partialCredit")
}
\arguments{
  \item{rule}{A rule (e.g., \code{\link{Compensatory}}, 
    \code{\link{OffsetConjunctive}} or the name of a rule.}
  \item{pnames}{A character vector giving the names of the parent
    variables.}
  \item{states}{A character vector giving the names of the states
    of the child variable.}
  \item{link}{A link function (e.g., \code{\link{partialCredit}})
    or the name of a link function.}
}
\details{

  Rules come in two styles:  
  \describe{
    \item{Linear rules, e.g., \code{\link{Compensatory}}}{These 
      take multiple slopes (alphas) and a single intercept (beta).}
    \item{Offset rules, e.g., \code{\link{OffsetConjunctive}}}{These
      take multiple intercepts (betas) and a single slope (alpha).}
  } 
  The value of \code{\link{getOffsetRules}()} determines which
  rules use which style.  These functions are useful for getting 
  the shape right when the parameters have not yet been established.

  There may or may not be different values for alpha or beta for 
  each state transition depending on the link function.  If 
  multiples are allowed then a list of alphas or betas is returned,
  corresponding to the _transitions_ between the states (so the
  length of the list is one less than the length of \code{states}).
  
  The shape of the result is as follows:
  \describe{
    \item{partialCredit}{With this link function both alphas and 
      betas will be lists if the number of states is greater 
      than 2.}
    \item{gradedResponse}{With this link function, the betas are 
      a list and the alphas are a single vector.  Note that the 
      betas should be in increasing order, but this is not 
      enforced.}
    \item{normalLink}{Both alphas and betas are vectors.}
    } 

}
\value{
  Either a named vector (the names are \code{pnames}) of the 
  default value (1 for alphas, 0 for betas) or scalar value, or a 
  named list of such.  If a list, then the names follow the names
  of the states with the last one omitted.
}
\author{Russell Almond}
\note{

  As more link functions are added, this needs to be expanded.

}
\seealso{
  \code{\link{calcDPCTable}}, \code{\link{getOffsetRules}},
  \code{\link{partialCredit}}, \code{\link{gradedResponse}},
  \code{\link{normalLink}}
}
\examples{

  defaultAlphas("Compensatory",c("S1","S2"),c("Yes","No"))
  defaultBetas("Compensatory",c("S1","S2"),c("Yes","No"))

  defaultAlphas("Compensatory",c("S1","S2"),c("Yes","Maybe","No"))
  defaultBetas("Compensatory",c("S1","S2"),c("Yes","Maybe","No"))

  defaultAlphas("Compensatory",c("S1","S2"),c("Yes","Maybe","No"),
                "gradedResponse")
  defaultBetas("Compensatory",c("S1","S2"),c("Yes","Maybe","No"),
               "gradedResponse")
  
  defaultAlphas("Compensatory",c("S1","S2"),c("Yes","Maybe","No"),
                "normalLink")
  defaultBetas("Compensatory",c("S1","S2"),c("Yes","Maybe","No"),
               "normalLink")

  defaultAlphas("OffsetConjunctive",c("S1","S2"),c("Yes","No"))
  defaultBetas("OffsetConjunctive",c("S1","S2"),c("Yes","No"))
  
  defaultAlphas("OffsetConjunctive",c("S1","S2"),
                c("Yes","Maybe","No"),"gradedResponse")
  defaultBetas("OffsetConjunctive",c("S1","S2"),
                  c("Yes","Maybe","No"),"gradedResponse")
}
\keyword{manip}

