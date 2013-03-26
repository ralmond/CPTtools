\name{getTableStates}
\alias{getTableParents}
\alias{getTableStates}
\title{Gets meta data about a conditional probability table.}
\description{
  Fetches the names of the parent variables, or the names of the states
  of the child variable from a conditional probability table.
}
\usage{
getTableStates(table)
}
\arguments{
  \item{table}{A conditional probability table expressed as a data frame.}
}
\details{
  These functions assume that \code{table} is a conditional probability
  table (or a set of hyper-Dirichlet parameters) which is shaped like a
  data frame.  Columns in the data frame which are factors are assumed
  to hold values for the parent (conditioning) variables.  Columns in
  the data frame which are numeric are assumed to correspond to possible
  states of the child (dependent) variable.
}
\value{
  For \code{getTableParents()}, a character vector giving the names of
  the parent variables (factor columns).

  For \code{getTableStates()}, a character vector giving the names of
  the child states (numeric columns).
}
\author{Russell Almond}
\note{ 
  StatShop usually assumes that the states are ordered from the
  \emph{highest} to the \emph{lowest} possible values, for example
  \sQuote{High}, \sQuote{Med}, \sQuote{Low}.
}
\seealso{\code{\link{rescaleTable}} }
\examples{
#conditional table
X2.ptf <- data.frame(Theta=c("Expert","Novice"),
                     correct=c(4,2),
                     incorrect=c(2,4))
#Unconditional table
Theta.ptf <- data.frame(Expert=3,Novice=3)

stopifnot(
identical(getTableStates(X2.ptf), c("correct","incorrect")),
identical(getTableStates(Theta.ptf), c("Expert","Novice")),
identical(getTableParents(X2.ptf), "Theta"),
identical(getTableParents(Theta.ptf), character(0))
)
}
\keyword{manip}

