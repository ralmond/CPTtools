\name{buildRegressionTables}
\alias{buildRegressionTables}
\title{Builds conditional probability tables from regressions}
\description{
  Takes a description of a normal graphical model as a series of
  regressions and a description of the corresponding discrete variables
  and builds a series of conditional probability tables for the
  corresponding Bayesian network.
}
\usage{
buildRegressionTables(regs, pvecs, mean = 0, std = 1)
}
\arguments{
  \item{regs}{A list with names corresponding to the variables in the
    model giving a series of regression coefficients (see Details).}
  \item{pvecs}{A list with names corresponding to the variables in the
    model giving a series of probability vectors in order from highest
    state to lowest state (see Details).}
  \item{mean}{A vector of means for the continuous variables.}
  \item{std}{A vector of standard deviations for the continuous variables.}
}
\details{
  The \code{regs} argument should be a list whose names are the names of
  the variables.  Each element should have the following fields:
  \describe{
    \item{b}{A vector of slopes for the regression with names equal to the
      names of the parent variables.  Note that if there are no parents,
      this should be \code{numeric(0)}.}
    \item{a}{The intercept from the regression.}
    \item{std}{The residual standard deviation from the regression.}
  }
  The function \code{buildRegressions()} creates an appropriate list.

  The \code{pvecs} should be a list whose names are the names of the
  variables.  Each element should be a named vector of probabilities in
  order from the \emph{Highest} to the \emph{Lowest} state, e.g.
  \code{c(High=.2,Med=.5,Low=.3)}.

  The values \code{mean} and \code{std} should either be scalars or
  vectors of the same length as the number of elements in \code{regs}
  and \code{pvecs}.  If vectors, they should have names corresponding to
  the variable names.  Note that the order of the elements does not need
  to be the same in all four arguments, but that the names of all four
  arguments need to be identical (unless \code{mean} or \code{std} are
  given as scalars, in which case they will be appropriately
  replicated.)

}
\value{
  A list of conditional probability tables whose names correspond to
  \code{regs}.  Each conditional probability table is expressed as a
  data frame whose columns correspond to either parent variables or
  states of the child variable and whose rows correspond to
  configurations of the parent variable.
}
\references{

  Almond, R. G. (2010). \sQuote{I can name that Bayesian network in two
  matrixes.} \emph{International Journal of Approximate Reasoning.}
  \bold{51}, 167-178.

  Whittaker, J. (1990).  \emph{Graphical Models in Applied Multivariate
    Statistics}.  Wiley.
}
\author{Russell Almond}
\section{Warning}{
  Variables are given from \emph{highest} to \emph{lowest} state, for
  example \sQuote{High}, \sQuote{Medium}, \sQuote{Low}.  This is the
  order expected by StatShop.  Note that \code{\link{pvecToCutpoints}}
  expects probability vectors in the opposite order.
} 
\seealso{\code{\link{buildRegressions}} }
\examples{
data(MathGrades)
pl <- buildParentList(structMatrix(MathGrades$var),"Algebra")
rt <- buildRegressions(MathGrades$var,MathGrades$means,pl)
tabs <- buildRegressionTables(rt, MathGrades$pvecs, MathGrades$means,
                              sqrt(diag(MathGrades$var)))

}
\keyword{manip}

