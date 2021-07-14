\name{gkGamma}
\alias{gkGamma}
\title{Calculates the Goodman and Kruskal gamma measure of association.}
\description{

  Goodman and Kruskal introduce a second coefficient, gamma
  (\code{gkGamma}) as a measure of association for two ordered
  categorical variables which do not necessarily have the same number of
  states.  Thus it is useful as a measure of association between an
  observed variable and a latent proficiency variable.

}
\usage{
gkGamma(tab)
}
\arguments{
  \item{tab}{A matrix whose rows and columns represent rating
    two different classifications and whose cells represent
    observed (or expected) counts.}
}
\details{

  Let \eqn{A} and \eqn{B} be ordinal variables (both oriented in the
  same direction either high to low or low to high).  Then \code{tab} is
  a cross-tabulation of the two variables (e.g., what is produced by
  the \code{\link[base]{table}} funciton).  In the case that one of the
  variables is a classification by a Bayesian network, this could also
  be an expected tabulation.  It also could be normalized by dividing by
  the total to give probabilities.

  Let \eqn{(a,b)} and \eqn{(a',b')} be two randomly chosen data points.
  Classify this pair according to the following rule:
  \describe{
    \item{Concordant (same)}{If both relationships point the same way,
      that is either both \eqn{a>a`} and \eqn{b>b'} or
      both \eqn{a<a`} and \eqn{b<b'}, then the relationship is
      \emph{concordant}.}
    \item{Discordant (different)}{If both relationships point in
      opposite ways,
      that is either both \eqn{a>a`} and \eqn{b<b'} or
      both \eqn{a<a`} and \eqn{b>b'}, then the relationship is
      \emph{discordant}.}
    \item{Discordant (different)}{If both relationships point in
      opposite ways,
      that is either both \eqn{a>a`} and \eqn{b<b'} or
      both \eqn{a<a`} and \eqn{b>b'}, then the relationship is
      \emph{discordant}.}
    \item{Tied}{If one or both variables are the same, 
      that is either \eqn{a = a`} or \eqn{b =b'} or
      both, then the relationship is \emph{tied}.}
  }

  Let \eqn{\Pi_s} be the proportion of pairs which are concordant,
  \eqn{\Pi_d} be the proportion of pairs which are discordant, and
  \eqn{\Pi_t} be the proportion of pairs which are tied.  Then gamma is
  defined as the proportion of concordant pairs minus the proportion of
  discordant pairs normalized by the number of non-tied pairs, that is:

  \deqn{\gamma = \frac{\Pi_s - \Pi_d}{1-\Pi_i}}

  Like a correlation coefficient it runs from -1 (perfect negative
  associations) through 0 (no association) to +1 (perfect association).
  It is comparable to Kendall's tau (see
  \code{\link[stats]{cor}(,method="kendall")}). 

}
\value{

  A numeric scalar.  
  Like a correlation coefficient it runs from -1 (perfect negative
  associations) through 0 (no association) to +1 (perfect association).
  It is comparable to Kendall's tau (see
  \code{\link[stats]{cor}(,method="kendall")}). 


}
\references{

  {Goodman, Leo A., Kruskal, William H.} (1954).
  {Measures of Association for Cross Classifications}.
  \emph{Journal of the American Statistical Association}.
  \bold{49} (268), 732--764.


}
\author{Russell Almond}
\seealso{
  Other similar statistics (useful for square tables):
  \code{\link{fcKappa}}, \code{\link{gkLambda}}

  Other measures of correlation:  \code{\link[stats]{cor}}.

  Building contingency tables: \code{\link[base]{table}}.
  
}
\examples{

## Example from Goodman and Kruskall (1963)
nab <- matrix(c(8,0,0,5,8,4,3,1,14,3,0,4),3,4)
stopifnot(all.equal(.6122,gkGamma(nab),tolerance=.0001))

## Using built-in data set US Judges, rounding to produce ordered
##   categories. 

table(round(USJudgeRatings[,1:2]))
gkGamma(table(round(USJudgeRatings[,1:2])))
## Kendall's Tau for comparison
cor(round(USJudgeRatings[,1:2]),method="kendall")

}
\keyword{ classif}
\concept{ Contingency Table }
\concept{ Measures Of Association }

