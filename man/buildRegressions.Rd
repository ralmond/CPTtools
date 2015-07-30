\name{buildRegressions}
\alias{buildRegressions}
\title{Creates a series of regressions from a covariance matrix}
\description{
  This function takes a covariance matrix and list of variables and
  their parents and returns a collection of regression model parameters
  for each variable regressed on its parents.  This is a compact
  representation of a normal graphical model.
}
\usage{
buildRegressions(Sigma, means = 0,
                 parents = buildParentList(structMatrix(Sigma)))
}
\arguments{
  \item{Sigma}{A covariance matrix among a collection of continuous variables.}
  \item{means}{The means of those variables}
  \item{parents}{A named list of length equal to \code{ncol(Sigma)}
    whose elements should correspond to the rows/columns of
    \code{Sigma}.  Each element should be a character vector giving the
    names of the parents of the given node.}
}
\details{
  This function performs one regression for each element of the
  \code{parents} list. The name of the dependent variable for each
  regression is given by \code{names(parents)} and the independent
  variables is given by the values of \code{parents}.  (The function
  \code{\link{buildParentList}()} builds a suitable list of elements.)

  If \code{means} is not supplied, then the variables are assumed to be
  centered, otherwise the given vector is used as the means.  
}
\value{
  A list of length equal to \code{parents} whose elements are also a
  list having the following structure
  \item{b}{A vector of slopes for the regression with names equal to the
    names of the parent variables.  Note that if there are no parents,
    this will be \code{numeric(0)}.}
  \item{a}{The intercept from the regression.}
  \item{std}{The residual standard deviation from the regression.}
}
\references{
  Almond, R. G. (2010). \sQuote{I can name that Bayesian network in two
  matrixes.} \emph{International Journal of Approximate Reasoning.}
  \bold{51}, 167-178.
}
\author{Russell Almond}
\seealso{\code{\link{buildParentList}},
  \code{\link{buildRegressionTables}}
}
\examples{
data(MathGrades)
pl <- buildParentList(structMatrix(MathGrades$var),"Algebra")
rt <- buildRegressions(MathGrades$var,MathGrades$mean,pl)
\dontshow{
stopifnot(
identical(lapply(rt[2:5],function(r) colnames(r$b)),pl[2:5]),
length(rt[[1]]$b) == 0
)
}
}
\keyword{manip}

