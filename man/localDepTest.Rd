\name{localDepTest}
\alias{localDepTest}
\alias{ciTest}
\title{Tests for conditional independence between two variables given a third
}
\description{

  The function \code{ciTest} takes a 3-way contingency table and tests
  for the conditional independence of the first two variables given the
  third.  The function \code{localDepTest} is a wrapper which builds the
  table from factor variables.  In psychometrics, when the third
  variable represents a latent proficiency and the first two item
  responses, this is sometimes called \emph{local independence}.
}
\usage{
localDepTest(obs1, obs2, prof)
ciTest(tab)
}
\arguments{
  \item{obs1}{A factor variable representing the first observable outcome.}
  \item{obs2}{A factor variable representing the second observable outcome.}
  \item{prof}{A factor variable representing the proficiency level, or
    any variable that is thought to render \code{obs1} and \code{obs2}
    independent.} 
  \item{tab}{A three-way table (see \code{\link[base]{table}}) where the
  first two dimensions represent the observable variables and the third
  the proficiency variable.}
}
\details{
  Let 1 and 2 represent \code{obs1} and \code{obs2} respectively and let
  3 represent \code{prof}.  In the case of \code{ciTest}, 1, 2 and 3
  represent the first second and 3rd dimensions of \code{tab}.  These
  function then compare the undirected model [13][23] (1 and 2 are
  conditionally independent given 3) to the unrestricted model [123].
  The result is a chi-square statistic comparing the two models, high
  values of the chi-square statistic indicate a better fit of the
  unrestricted model compared to the conditional independence model.

  Note that the Cochran-Mantel-Haenszel statistic (see
  \code{\link[stats]{mantelhaen.test}}) is similar, but it assumes that
  there is no three-way interaction, so it essentially tests [13][23]
  versus [12][13][23].
}
\value{
  A list with three elements:
  \item{G2}{The chi-square comparison between the two models.}
  \item{df}{The degrees of freedom for the test.}
  \item{p}{The percentage point for \code{G2} in a central chi-square
  distribution with \code{df} degrees of freedom, i.e., the p-value.}
}
\references{
  Bishop, Feinberg and Holland (1975).  \emph{Discrete Multivariate
  Analysis:  Theory and Practice.}  MIT Press.
}

\author{Russell Almond}
\seealso{
  \code{\link{buildFactorTab}}, \code{\link[stats]{mantelhaen.test}},
  \code{\link[datasets]{UCBAdmissions}}
}
\examples{

data(UCBAdmissions)
ciTest(UCBAdmissions)

data(ACED)
table(ACED.items$tCommonRatio1a.1,ACED.items$tCommonRatio2a.1,
      cut(ACED.items$Correct,3))
localDepTest(ACED.items$tCommonRatio1a.1,ACED.items$tCommonRatio2a.1,
      cut(ACED.items$Correct,3))


}
\keyword{ htest }
