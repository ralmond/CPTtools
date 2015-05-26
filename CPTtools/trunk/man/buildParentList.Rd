\name{buildParentList}
\alias{buildParentList}
\title{Builds a list of parents of nodes in a graph}
\description{
  Takes an incidence matrix describing a graph, and an order of the
  nodes, and builds a list of parents for each node.  If the \code{ord}
  argument is not supplied, the ordering is constructed through maximum
  cardinality search (see \code{\link{mcSearch}}).
}
\usage{
buildParentList(sm, start = colnames(sm)[1], ord = mcSearch(sm, start))
}
\arguments{
  \item{sm}{A logical matrix whose rows and columns correspond to nodes
    (variables) and a true value indicates an edge between the variables.}
  \item{start}{The name of the first element.}
  \item{ord}{A vector of size equal to the number of columns of
    \code{sm} giving the ordering of nodes.}
}
\details{
  The \code{sm} argument should be an incidence matrix for a graph, with
  row and column names set to the names of the nodes/variables.
  
  A node \eqn{i} is a \dfn{parent} of node \eqn{j} if
  \itemize{
    \item{a}{They are neighbors, that is \code{sm[i,j] == TRUE}.}
    \item{b}{The node \eqn{i} appears before node \eqn{j} in \code{ord},
      that is \code{ord[i] < ord[j]}.}
  }

  The argument \code{start} is used only if \code{ord} is not supplied,
  in which case it is passed to \code{mcSearch}.
}
\value{
  A list with as many elements as there are columns in \code{sm}, and
  whose elements appear in the order specified by \code{ord}.  Each
  element of that list is a character vector giving the names of the
  parents.  
}
\references{
  Almond, R. G. (2010). \sQuote{I can name that Bayesian network in two
  matrixes.} \emph{International Journal of Approximate Reasoning.}
  \bold{51}, 167-178.
}
\author{Russell Almond}
\seealso{\code{\link{mcSearch}}, \code{\link{structMatrix}}}
\examples{
data(MathGrades)
MG.struct <- structMatrix(MathGrades$var)

parents <- buildParentList(MG.struct)   # Arbitrary start
parentsa <- buildParentList(MG.struct, "Algebra") # Put algebra first.
\dontshow{
stopifnot(
all(sapply(parents,length) == c(0,1,2,1,2)),
all(sapply(parentsa,length) == c(0,1,2,1,2))
)
}

}
\keyword{manip}

