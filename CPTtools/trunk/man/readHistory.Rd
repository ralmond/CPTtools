\name{readHistory}
\alias{readHistory}
\title{Reads a file of histories of marginal distributions.}
\description{
  In running a typical Bayes net engine, as each piece of evidence comes
  in, updated marginal distributions for several variables are output.
  This function reads a such a log, expressed as a comma separated value
  file, and creates a data structure suitable for doing weight of
  evidence analyses.
}
\usage{
readHistory(csvfile)
}
\arguments{
  \item{csvfile}{A name of a CSV file containing StatShop marginal
    distributions, one per row.  Expects a column named "Item" from
    which the names are taken and "Result" which contains the values.}
}
\details{
  This works with an excerpted log from a StatShop/ACED interaction.  In
  this case the \var{Item} column should contain the name of the item
  presented at this iteration, possibly with an \code{.xml} suffix.  The
  \var{Results} column should contain a probability vector of the form:
  \code{[High:0.527,Medium:0.447,Low:0.025]}.  This function parses the
  CSV file and creates a matrix with rows corresponding to the rows in
  the CSV file and values from the probability vectors.
}
\value{
  A matrix whose column names are taken from the probability vectors and
  row names are taken from the \var{Item} field with the \code{.xml}
  suffix removed.
}
\references{
  \url{http://research.ets.org/~ralmond/StatShop/dataFormats.html}
}
\author{Russell Almond}
\seealso{\code{\link{parseProbVec}}, \code{\link{woeHist}}}
\examples{
\dontrun{
  allcorrect <- parseProbVec("CorrectSequence.csv")
}
}
\keyword{interface}
