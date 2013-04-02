\name{mcSearch}
\alias{mcSearch}
\title{Orders variables using Maximum Cardinality search}
\description{
  Takes a graph described by an incidence matrix, and creates an
  ordering of the nodes using maximum cardinality search (Tarjan and
  Yannakakis, 1984).  A primary application of this method is to chose
  an ordering of nodes in an undirected graph to make a corresponding
  directed graph. 
}
\usage{
mcSearch(sm, start = colnames(sm)[1])
}
\arguments{
  \item{sm}{A logical matrix whose rows and columns correspond to nodes
    (variables) and a true value indicates an edge between the variables.}
  \item{start}{The name of the first element.}
}
\details{
  The \code{sm} argument should be an incidence matrix for a graph, with
  row and column names set to the names of the nodes/variables.

  The function returns an ordering of the nodes where each node is
  chosen so that it has a maximum number of neighbors among those nodes
  higher in the order.

  Ties are broken by chosing the first node in the graph (using the
  order of the columns) matching the criteria.  One special case is the
  first node which is always an arbitrary choice.  The \code{start}
  argument can be used to force a particular selection.
}
\value{
  A vector of lenght equal to the number of rows whose values correspond
  to the order of the variable in the final order.
}
\references{
  Almond, R.G. (1995).  \emph{Graphical Belief Modeling}.  Chapman and
  Hall. 
  
  Lauritzen, S.L. and D.J. Spiegelhalter (1988).  Local Computation with
  Probabilities on Graphical Structures and their Application to Expert
  Systems (with discussion).  \emph{Journal of the Royal Statistical
    Society,Series B}, \bold{50}, {205-247}.
  
  Tarjan, R.E. and M. Yannakakis (1984).  Simple Linear-Time Algorithms
  to test Chordality of Graphs, Test Acyclicity of Hypergraphs, and
  Selectively Reduce Acyclic Hypergraphs.  \emph{Siam J. Comput.}
  \bold{13}, {566-579}.
}
\author{Russell Almond}
\note{
  If the graph is triangulated, then the ordering produced by
  \code{mcSearch} should be \dfn{perfect} --- for each node in the
  order, the set of neighbors of that node which preceed it in the
  ordering is completely connected.  Perfect node orderings are useful
  in going from undirected to directed graphical representations.  If
  we take an undirected graph and a perfect ordering, a define as the
  parents of an node all of its neighbors which are previous in
  the order and define as its children all of the nodes which are later
  in the order, then when converting the graph back to the undirected
  form no additional \dQuote{moralization} edges will be required.
  Thus, this function can be used to generate orders for
  \code{\link{buildParentList}}.  

  Graphical models generally exist only over triangulated graphs.
  Therefore, and incidence matrix which has been produced through the
  use of the \code{\link{structMatrix}} function should alway work.
  Tarjan and Yannakakis (1984) prove that the maximum cardinality
  search always produces a perfect ordering when the graph is
  triangulated.

  When the graph is not triangulated, the maximum cardinality search
  algorithm can be used to generate \dQuote{fill-ins} to triangulate the
  graph.  Lauritzen and Spiegelhalter (1988) note this use.  While
  maximum cardinality search will produce an ordering quickly, the
  ordering itself has now particular optimality properties as far as the
  triangulated graph which it creates (Almond, 1995).
}
\seealso{\code{\link{structMatrix}}, \code{\link{buildParentList}} }
\examples{
data(MathGrades)
MG.struct <- structMatrix(MathGrades$var)

ord <- mcSearch(MG.struct)   # Arbitrary start
orda <- mcSearch(MG.struct, "Algebra") # Put algebra first.
\dontshow{
stopifnot (all(ord == 1:5), all(orda == c(2,3,1,4,5)))
}
names(sort(orda)) # names of the variables in the chosen order.

# Sort rows and columns of structure matrix by MC order
MG.struct[order(orda), order(orda)]
}
\keyword{manip}

