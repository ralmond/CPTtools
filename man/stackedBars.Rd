\name{stackedBars}
\alias{stackedBars}
\title{ Produces a stacked, staggered barplot}
\description{
  This produces a series of stacked bar plots staggered so that the
  baseline corresponds to a particular state level.  This is primariliy
  designed for producing plots of probability vectors coming out of
  Bayes net scoring.
}
\usage{
stackedBars(data, profindex, ...,
            ylim = c(min(offsets) - 0.25, max(1 + offsets)),
            cex.names = par("cex.axis"), percent=TRUE,
            digits = 2*(1-percent), labrot=FALSE)
}
\arguments{
  \item{data}{A \code{data.frame} where each column is a probability vector.}
  \item{profindex}{The index of the proficiency which should be used as
    a baseline. }
  \item{\dots}{Graphical arguments passed to \code{barplot}. }
  \item{ylim}{ Default limits for Y axis.}
  \item{cex.names}{ Magnification for names. }
  \item{percent}{Logical value.  If true data values are treated as
    percentages instead of probabilities.}
  \item{digits}{ Number of digits for overlayed numeric variables.}
  \item{labrot}{If true, labels are rotated 90 degrees.}
}
\details{
  This plot type assumes that each column in its first argument is a
  probability vector.  It then produces a stacked bar for each column.
  The baseline of the bar is offset by the probility for being in the
  category marked by \code{profindex} or below.

  The probability values are overlayed on the bars.
}
\references{ This plot type was initially developed in Jody Underwood's
  Evolve project.

  Almond, R. G.,  Shute, V. J., Underwood, J. S., and Zapata-Rivera,
  J.-D (2009). Bayesian Networks: A Teacher's View. \emph{International
  Journal of Approximate Reasoning.} \bold{50}, 450-460.
}
\author{Russell Almond}
\seealso{\code{\link{compareBars}}, \code{\link{colorspread}},
  \code{\link{buildMarginTab}}, \code{\link{marginTab}},
  \code{\link[graphics]{barplot}},\code{\link{stackedBarplot}}}
\examples{
margins <- data.frame (
 Trouble=c(Novice=.19,Semester1=.24,Semester2=.28,Semseter3=.20,Semester4=.09),
 NDK=c(Novice=.01,Semester1=.09,Semester2=.35,Semseter3=.41,Semester4=.14),
 Model=c(Novice=.19,Semester1=.28,Semester2=.31,Semseter3=.18,Semester4=.04)
)

stackedBars(margins,3,
            main="Margininal Distributions for NetPASS skills",
            sub="Baseline at 3rd Semester level.",
            cex.names=.75, col=hsv(223/360,.2,0.10*(5:1)+.5))

stackedBars(margins,3,
            main="Margininal Distributions for NetPASS skills",
            sub="Baseline at 3rd Semester level.",
            percent=FALSE,digits=2,
            cex.names=.75, col=hsv(223/360,.2,0.10*(5:1)+.5))
}
\keyword{hplot}% at least one, from doc/KEYWORDS

