\name{readHistory}
\alias{readHistory}
\title{Reads a file of histories of marginal distributions.}
\description{

  In running a typical Bayes net engine, as each piece of 
  evidence comes in, updated marginal distributions for several
  variables are output. This function reads a such a log, 
  expressed as a data frame, and creates a data structure 
  suitable  for doing weight of evidence analyses.  The 
  probabilities are in the pvec (\code{\link{parseProbVec}}) 
  format. 
}
\usage{
readHistory(histdat, obscol = "Item", valcol = "Result",
  probcol="Probability")
}
\arguments{
  \item{histdat}{A data frame which has columns corresponding to 
    \code{obscol}, \code{valcol} and \code{probcol}}.
  \item{obscol}{Name of the column with the name of the observable at each time point.}
  \item{valcol}{Name of the column with the value of the observable at each time point.}
  \item{probcol}{Name of the column with the probabity vectors.}
}
\details{
  
  The assumption is that the \code{histdat} data frame contains
  a history of measurements on a latent variable.  The 
  \var{probcol} column should contain a probability vector of 
  the  form:  \code{[High:0.527,Medium:0.447,Low:0.025]}.  This 
  function parses this column (see \code{\link{parseProbVec}}) and 
  builds a matrix with columns corresponding to the states of the 
  latent variable.
  
  The rows are given names of the form, \code{<obscol>=<valcol>}, 
  where \code{<obscol>} and \code{<valcol>} are the values in 
  the respective columns.
}
\value{
  A matrix whose column names are taken from the probability
  vectors and row names are taken from the \code{obscol} and 
  \code{valcol} fields.
}
\note{

  The previous version (a) directly read the CSV file, and (b) 
  had the names of the columns hard coded.  This version will
  break any code that relied on the old version.  However, it is
  hopefully more generally useful.

}
\references{
  \url{http://research.ets.org/~ralmond/StatShop/dataFormats.html}
}
\author{Russell Almond}
\seealso{\code{\link{parseProbVec}}, \code{\link{woeHist}}}
\examples{
  testFiles <- system.file("testFiles",package="CPTtools")
  allcorrect <- readHistory(read.csv(file.path(testFiles,
     "CorrectSequence.csv"),as.is=TRUE),
     probcol="Margin.sequences.")
  woeHist(allcorrect,"High",c("Medium","Low"))
  allincorrect <- readHistory(read.csv(file.path(testFiles, 
    "InCorrectSequence.csv"),as.is=TRUE),
     probcol="Margin.sequences.")
}
\keyword{manip}
\concept{weight of evidence}
