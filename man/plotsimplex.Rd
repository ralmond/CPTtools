\name{plotsimplex}
\alias{plotsimplex}
\alias{simplex_vertex_projection}
\title{Produces a simplex plot of probability vectors.}
\description{

  A \eqn{K} dimensional probability vector likes on a \eqn{(K-1)}-simplex.
  This simplex can be projected onto 2-dimension, with vertices
  corresponding to the state of the variable
  (\code{simplex_vertex_projection} calculates these co-ordinates).
  The \code{simplexplot} function produces the plot.

}
\usage{
plotsimplex(data, radius = 1, order = NULL, 
  labels_cex = 1, labels = colnames(data), show_labels = TRUE,
  points_col = "#00000044", points_pch = 19, points_cex = 1,
  label_offset = 0.9, 
  projection_vertexes = simplex_vertex_projection(ncol(data), radius), 
  show_points = TRUE, show_circle = TRUE, 
  circle_col = "lightgray", show_edges = TRUE, 
  edges_col = "lightgray", ...)
simplex_vertex_projection(nvert, r=1)
}
\arguments{
  \item{data}{A data frame or matrix with rows representing individuals, and columns states.  The row sums should be one.}
  \item{radius, r}{The radius of the circle on which to plot.}
  \item{order}{Order of the states.}
  \item{labels_cex}{Graphical parameter controlling label size.}
  \item{labels}{Labels for the vertices.}
  \item{show_labels}{Logical, should labels be shown.}
  \item{points_col}{Colours for the points.}
  \item{points_pch}{Glyph for the points.}
  \item{points_cex}{Size for the points.}
  \item{label_offset}{Location for the labels.}
  \item{projection_vertexes}{A function for computing the
  vertex locations.}
  \item{show_points}{Logical, should the points be plotted.}
  \item{show_circle}{Logical, should the reference cicle be plotted.}
  \item{circle_col}{Logical, colour for the circle.}
  \item{show_edges}{Logical, should the edges be shown.}
  \item{edges_col}{The colour for the edges.}
  \item{\dots}{extra arguments are ignored.}
  \item{nvert}{The number of vertices.}
}
\details{

  This function is adapted from the \code{archetypes::simplexplot}
  function.  As the probability vector is already a simplex, we
  don't need the archetypes overhead.

}
\value{
  The function \code{simplex_vertex_projection} returns a matrix
  giving the co-ordinates of the vertices.
  
  The function \code{simplexplot} is normally called for its 
  side-effects, however, it invisibly returns a data structure 
  containing plot details.
}
\references{

See Section 6 in "Probabilistic Archetypal Analysis" by Seth and Eugster (2014), http://arxiv.org/abs/1312.7604.

This is a simplified version of the code in the archetypes package.  https://cran.r-project.org/package=archetypes

}
\author{Russell Almond}
\seealso{
  \code{\link{pvecTable}}
}
\examples{
  data(ACED)
  ptab <- pvecTable(ACED.scores,"sgp")
  plotsimplex(ptab)
}
\keyword{ hplot }
\concept{ simplex }
\concept{ probability vector }
