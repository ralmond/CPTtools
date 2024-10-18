\name{expTable}
\alias{expTable}
\alias{pvecTable}
\alias{catTable}
\title{Builds expected contingency tables.}
\description{

  The \code{\link[base]{table}} function in base-R builds contingency tables from cross-classifying factors.  If the factor is not known, but instead estimated using a Bayesian classifier, then instead of a single column with the factor value, there will be several columns with giving the probability that the individual is in each state.  The table built is then an expected contingency table.  
}
\usage{
expTable(data, pvecVars, facVars, 
         pvecregex = "<var>\\\\.<state>")
pvecTable(data, pvarName, 
         regx = sub("<var>",pvarName,"<var>\\\\.<state>"))
catTable(data, fvarName, 
         cc = contrasts(as.factor(dplyr::pull(data, fvarName)),
              contrasts = FALSE))
}
\arguments{
  \item{data}{This is a data frame where columns corresponding to probability vectors have a regular naming pattern.}
  \item{pvecVars, pvarName}{The names (or name for \code{pvecName})  of the estimated variable(s).}
  \item{facVars,fvarName}{The names (or name for \code{catTable}) of the factor variable(s).}
  \item{pvecregex, regx}{A regular expression template for finding the column names.  The string \dQuote{<var>} is replaced with the variable name (\code{pvecName}) and the string \dQuote{<state>} is replaced with a regular expression that extract the state name.  }
  \item{cc}{This is a contrasts matrix (see \code{\link[stats]{contrasts}} which shows how to convert the factor variable to columns.}
}
\details{

  For an individual, $i$, let $Y_i$ be a fully observed variable 
  which takes on states $\{ y_1, \ldots, y_m\}$. Let $S_i$ be a
  latent with states $\{s_1, \ldots, s_k\}$, and let $P(S_i)$ is an
  estimate of $S_i$, so it is a vector over the states.
  
  The expected matrix is formed as follows:
  \enumerate{
    \item{Initialize a $k$ by $m$ (rows correspond to states of
      $S$ and columns to states of $Y$) matrix with 0.}
    \item{For each individual, add $P(S_i)$ to the column
      corresponding to $Y_i$}
  } 

  The result is the expected value of the contingency table.
  The general case can be handled with an Einstein sum (see 
  \code{\link[einsum]{einsum}}), with a rule \dQuote{za,zb,zc,... -> abc...}.
  
  The assumption is that the estimates of the latent variable are 
  saved in columns with a regular naming convention.  For example,
  the \link{ACED} data set uses the names \code{P.cr..H}, 
  \code{P.cr..M} and \code{P.cr..L} are the high, medium and 
  low probabilities for the common ratio variable.  The regular
  expression which will capture the names of the states is 
  \dQuote{P\\.cr\\.\\.(\\w+)}, where \dQuote{\\w+} is one or more 
  word constituent characters.  The parentheses around the
  last part are used to extract the state names.  
  
  Internally, the function substitutes the value of
  \code{pvecName} for \dQuote{<var>}, and the \dQuote{(\\w+)} is
  substituted for \dQuote{<state>}.  If this regular expression 
  doesn't work for grabbing the state names, a better expression 
  can be substituted, but it should be the first sub-expression
  marked with parentheses.  Note also that
  the period has a special meaning in regular expressions so it
  needs to be quoted.  Note also, that the backslash needs to be
  quoted in R strings.
  

}
\value{
  
  The functions \code{pvecTable} and \code{catTable} return a 
  matrix with rows corresponding to the original data, and columns
  to the states of the variable.
  
  The function \code{expTable} produces an array whose dimensions
  correspond to the states of probability and factor variables.
  
}
\references{

  Observable Analysis paper (in preparation).

}
\author{Russell Almond}
\seealso{
  \code{\link{mutualInformation}}, \code{\link{gkGamma}},
  \code{\link[base]{table}}, \code{\link[stats]{contrasts}},
  \code{\link[einsum]{einsum}}
}
\examples{

data(ACED)
ACED.joined <- dplyr::inner_join(ACED.scores,ACED.items,by="SubjID")
head(pvecTable(ACED.joined,"cr",regx="P\\.cr\\.\\.<state>"))
head(catTable(ACED.joined,"tCommonRatio1a"))
expTable(ACED.joined,"cr","tCommonRatio1a",
         pvecregex="P\\.<var>\\.\\.<state>")

}
\keyword{ manip }
\keyword{ multivariate }
\concept{ contigency table }
\concept{ probability vector }
