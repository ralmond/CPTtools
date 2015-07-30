\name{structMatrix}
\alias{structMatrix}
\title{Finds graphical structure from a covariance matrix}
\description{
  This function finds an undirected graphical representation of a
  multivariate normal distribution with the given covariance matrix, by
  associating edges with non-zero entries.  Graphical structure is given
  as an adjacency matrix.
}
\usage{
structMatrix(X, threshold = 0.1)
}
\arguments{
  \item{X}{A variance matrix.}
  \item{threshold}{A numeric value giving the threshold for a value to
    be considered \dQuote{non-zero}.}
}
\details{
  For a multivariate normal model, zero entries in the inverse
  covariance matrix correspond to conditional independence statements
  true in the multivariate normal distribution (Whitaker, 1990;
  Dempster, 1972).  Thus, every non-zero entry in the inverse
  correlation matrix corresponds to an edge in an undirected graphical
  model for the structure.

  The \code{threshold} parameter is used to determine how close to zero
  a value must be to be considered zero.  This allows for both
  estimation error and numerical precision when inverting the covariance
  matrix. 
}
\value{
  An adjacency matrix of the same size and shape as \code{X}.  In this
  matrix \code{result[i,j]} is \code{TRUE} if and only if Node \eqn{i}
  and Node \eqn{j} are neighbors in the graph.
}
\references{
  Dempster, A.P. (1972) Covariance Selection.  \emph{Biometrics},
  \strong{28},  157--175.
  
  Whittaker, J. (1990).  \emph{Graphical Models in Applied Multivariate
    Statistics}.  Wiley.
}
\author{Russell Almond}
\note{Models of this kind are known as \dQuote{Covariance Selection
    Models} and were first studied by Dempster (1972).
}
\seealso{\code{\link{scaleMatrix}}, \code{\link{mcSearch}},
  \code{\link{buildParentList}}
}
\examples{
data(MathGrades)

MG.struct <- structMatrix(MathGrades$var)
\dontshow{
stopifnot(all(rowSums(structMatrix(MathGrades$var)) == c(3,3,5,3,3)))
}
}
\keyword{manip}

