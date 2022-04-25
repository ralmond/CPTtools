\name{woeBal}
\alias{woeBal}
\title{Weight of Evidence Balance Sheet}
\description{
  Creates a weight of evidence balance sheet from a history of marginal
  distributions.  
}
\usage{
woeBal(hist, pos, neg, obs=NULL, title = "Evidence Balance Sheet",
       col = rev(colorspread("slategray",ncol(hist),maxsat=TRUE)),
       posCol="cyan", negCol="red", stripCol=c("white","lightgray"),
       lcex = 0.65)
}
\arguments{
  \item{hist}{A matrix whose rows represent time points (after tests)
    and columns represent probabilities.}
  \item{pos}{Names or numbers of states which should be regarded as
    \dQuote{positive}}
  \item{neg}{Names or numbers of states which should be regarded as
    \dQuote{negative}}
  \item{obs}{An optional character vector of the same length as the
    number of rows of \code{hist} giving the value observed at each step}
  \item{title}{Title for plot}
  \item{col}{A list of color values for probability bars.}
  \item{posCol}{The color to be used for bars showing positive weights
    of evidence.} 
  \item{negCol}{The color to be used for bars showing negative weights
    of evidence.}
  \item{stripCol}{The colors to be used for the time step labels.
    Setting this to a vector of two colors creates alternate color
    stripes.  Set this to "white" to disable that effect.}
  \item{lcex}{Character expansion size for labels.}
}
\details{
  This constructs a weight of evidence balance sheet (Madigan,
  Mosurski, and Almond, 1997) showing the changes
  to the probability distribution and weight of evidence for each
  change in the probability.  The probabilities are given in the
  \code{hist} argument in which each row should be a probability
  distribution for the target variable.  The labels for the plot are
  taken from the row labels of the \code{hist} argument.

  Madigan, Mosurski and Almond (1997) note that the definition of weight
  of evidence is somewhat problematic if the hypothesis variable is not 
  binary.  In that case, they recommend partitioning the states into a
  \emph{positive} and \emph{negative} set.  The \code{pos} and \code{neg}
  are meant to describe that partition.  They can be any expression
  suitable for selecting columns from the \code{hist} matrix.  This
  function calls \code{woeHist()} to calculate weights of evidence.

  The row names of \code{hist} are printed left-justified in the
  leftmost column.  If observed values (\code{obs}) are supplied, they
  are printed right justified in the same column.
  
}
\value{
  The midpoints of the bars (see \code{\link[graphics]{barplot}}) are
  returned invisibly.
}
\references{
  Good, I. (1971) The probabilistic explication of information,
  evidence, surprise, causality, explanation and utility.
  In \emph{Proceedings of a Symposium on the Foundations of Statistical
    Inference}.  Holt, Rinehart and Winston, 108-141.

  Madigan, D.,  Mosurski, K. and Almond, R. (1997) Graphical explanation
  in belief networks.  \emph{Journal of Computational Graphics and
    Statistics}, \bold{6}, 160-181.

  Almond, R. G., Kim, Y. J., Shute, V. J. and  Ventura, M. (2013).
  Debugging the Evidence Chain.  In Almond, R. G. and  Mengshoel,
  O. (Eds.)  \emph{Proceedings of the 2013 UAI Application Workshops:
  Big Data meet Complex Models and Models for Spatial, Temporal and
  Network Data (UAI2013AW)}, 1-10.
  \url{http://ceur-ws.org/Vol-1024/paper-01.pdf} 

  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and
  Yan, D. (2015) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 7.

  }
\author{Russell Almond}
\section{Side Effects}{
  Starts a new plotting page and creates three side-by-side plots, one
  for the labels, one for the probability bars and one for the weight of
  evidence bars.
}
\seealso{\code{\link{readHistory}}, \code{\link{woeHist}},
  \code{\link[graphics]{barplot}}, \code{Colors}}
\examples{

sampleSequence <- read.csv(system.file("testFiles","SampleStudent.csv",
                                       package="CPTtools"),
                           header=TRUE,row.names=1)

woeBal(sampleSequence[,c("H","M","L")],c("H"),c("M","L"),lcex=1.25)
woeBal(sampleSequence[,c("H","M","L")],c("H"),c("M","L"),
      obs=sampleSequence[,"Acc"],lcex=1.25)


}
\keyword{hplot}% at least one, from doc/KEYWORDS

