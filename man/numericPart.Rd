\name{numericPart}
\alias{numericPart}
\title{Converts a mixed data frame to a numeric matrix.}
\description{
  Converts a \code{data.frame} to a \code{matrix}, by dropping columns
  which contain non-numeric data.
}
\usage{
numericPart(table)
}
\arguments{
  \item{table}{A \code{data.frame} object.}
}
\details{
  The primary purpose is to convert a conditional probability
  distribution in data frame format (with a set of factor rows
  identifying the states of the parent variables) to a table of just the
  numbers.  It may have other uses as well.

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
