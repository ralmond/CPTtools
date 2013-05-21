\name{woeBal}
\alias{woeBal}
\title{Weight of Evidence Balance Sheet}
\description{
  Creates a weight of evidence balance sheet from a history of marginal
  distributions.  
}
\usage{
woeBal(hist, pos, neg, title = "Evidence Balance Sheet",
       col = rev(SlateGray), lcex = 0.65)
}
\arguments{
  \item{hist}{A matrix whose rows represent time points (after tests)
    and columns represent probabilities.}
  \item{pos}{Names or numbers of states which should be regarded as
    \dQuote{postivie}}
  \item{neg}{Names or numbers of states which should be regarded as
    \dQuote{negative}}
  \item{title}{Title for plot}
  \item{col}{A list of color values for probability bars}
  \item{lcex}{Character expansion size for labels.}
}
\details{
  This constructs a weight of evidence balance sheet (Madigan,
  Musorski, and Almond, 1997) showing the changes
  to the probability distribution and weight of evidence for each
  change in the probability.  The probabilities are given in the
  \code{hist} argument in which each row should be a probability
  distribution for the target variable.  The labels for the plot are
  taken from the row labels of the \code{hist} argument.

  Madigan, Musorski and Almond (1997) note that the definition of weight
  of evidence is somewhat problematic if the hypothesis variable is not 
  binary.  In that case, they recommend partitioning the states into a
  \emph{postive} and \emph{negative} set.  The \code{pos} and \code{neg}
  are meant to describe that partition.  They can be any expression
  suitable for selecting columns from the \code{hist} matrix.  This
  function calls \code{woeHist()} to calculate weights of evidence.
  
}
\value{
  The output of running \code{woeHist(hist,pos,neg)} is returned invisibly.
}
\references{
  Good, I. (1971) The probabilistic explication of information,
  evidence, surprise, causality, explanation and utility.
  In \emph{Proceedings of a Symposium on the Foundations of Statistical
    Inference}.  Holt, Rinehart and Winston, 108-141.

  Madigan, D.,  Mosurski, K. and Almond, R. (1997) Graphical explanation
  in belief networks.  \emph{Journal of Computational Graphics and
    Statistics}, \bold{6}, 160-181.
  }
\author{Russell Almond}
\section{Side Effects}{
  Starts a new plotting page and creates three side-by-side plots, one
  for the labels, one for the probability bars and one for the weight of
  evidence bars.
}
\seealso{\code{\link{readHistory}}, \code{\link{woeHist}},
  \code{\link[base]{barplot}}, \code{Colors}}
\examples{
\dontrun{
  allcorrect <- parseProbVec("CorrectSequence.csv")
  woeBal(allcorrect,c("High"),c("Medium","Low"))
  woeBal(allcorrect,1:2,3,col=rev(sienna))
}
}
\keyword{hplot}% at least one, from doc/KEYWORDS

