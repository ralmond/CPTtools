\name{numericPart}
\alias{numericPart}
\title{Converts a mixed data frame to a numeric matrix.}
\description{
  Converts a \code{matrix} to a \code{data.frame}, by dropping columns
  which contain non-numeric data.
}
\usage{
numericPart(table)
}
\arguments{
  \item{table}{A \code{data.frame} object.}
}
\value{
  A matrix containing just the numeric columns of the data frame.
}
\author{Russell Almond}
\seealso{ \code{\link[base]{data.frame}}, \code{\link[base]{matrix}},
      \code{\link[base]{data.matrix}}}
\examples{
name <-c("Shahrazad", "Marguerite")
height <- c(34, 36)
weight <- c(28, 26)
twins <- data.frame(name=I(name),height=height, weight=weight)
numericPart(twins)
}
\keyword{utilities}
