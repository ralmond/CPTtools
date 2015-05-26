\name{stackedBarplot}
\alias{stackedBarplot}
\title{Produces a hanging barplot}
\description{
  This produces a series of stacked bar plots staggered so that the
  baseline corresponds to a particular state level.  This is primariliy
  designed for producing plots of probability vectors coming out of
  Bayes net scoring.
}
\usage{
stackedBarplot(height, width = 1, space = 0.2, offset = 0, names.arg = NULL,
               legend.text = NULL, horiz = FALSE, density = NULL,
               angle = 45, col = NULL, border = par("fg"),
               main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
               xlim = NULL, ylim = NULL, xpd = TRUE, axis = TRUE,
               axisnames = TRUE, cex.axis = par("cex.axis"),
               cex.names = par("cex.axis"), newplot = TRUE,
               axis.lty = 0, ...)
}
\arguments{
  \item{height}{A matrix giving the heights of the bars.
    The columns represent bars, and the rows represent groups within
    each bar.}
  \item{width}{A numeric vector of length equal to the number of columns
    in \code{height} giving the width of the bars. If value is shorter than
    number of columns in \code{height} it is recycled to produce the
    correct length.}
  \item{space}{A numeric vector of length equal to the number of columns
    in \code{height} giving the space between the bars. If value is shorter than
    number of columns in \code{height} it is recycled to produce the
    correct length.
  }
  \item{offset}{A numeric vector of length equal to the number of
    columns in \code{height} giving distance by which the bars should be
    offset from the zero line on the axis.  Setting this to a non-zero
    value produces a hanging barplot.}
  \item{names.arg}{If not missing, used as axis labels (see
    \code{\link[graphics]{axis}}).} 
  \item{legend.text}{If node null, a lengend is generated (see
    \code{\link[graphics]{legend}}).} 
  \item{horiz}{A logical value.  If true, stacked bars are printed
    horizontially instead of vertically.}
  \item{density}{Density of shading lines (see
    \code{\link[graphics]{rect}}).  This should be a scalar or a vector
    of length equal to the number of rows of \code{height}.}
  \item{angle}{Angle of shading lines (see
    \code{\link[graphics]{rect}}). This should be a scalar or a vector
    of length equal to the number of rows of \code{height}.}
  \item{col}{Color used for each bar.  This should be a scalar or a vector
    of length equal to the number of rows of \code{height}. If not
    supplied a greyscale gradient is built.}
  \item{border}{Color for the rectangle borders (see
    \code{\link[graphics]{rect}}). This should be a scalar or a vector
    of length equal to the number of rows of \code{height}.}
  \item{main}{Main title for plot (see \code{\link[graphics]{title}}).}
  \item{sub}{Subtitle for plot (see \code{\link[graphics]{title}}).}
  \item{xlab}{X-axis label for plot (see \code{\link[graphics]{title}}).}
  \item{ylab}{Y-axis label for plot (see \code{\link[graphics]{title}}).}
  \item{xlim}{Limits in user co-ordinates for the x-axis.  Should be a
    vector of length 2.}
  \item{ylim}{Limits in user co-ordinates for the y-axis.  Should be a
    vector of length 2.}
  \item{xpd}{A logical value controling clippling.  (see
    \code{\link[graphics]{par}}).} 
  \item{axis}{A logical value.  If true, a numeric scale is printed on
    the appropriate axis.}
  \item{axisnames}{A logical value.  If true, column names are printed
    on the appropriate axis.}
  \item{cex.axis}{Character size used for the numeric axis labels (see
    \code{\link[graphics]{axis}}).}
  \item{cex.names}{Character size used for the text (column names) axis
    labels (see \code{\link[graphics]{axis}}).}
  \item{newplot}{A logical value.  If true a new graphics region is
    created.  If false, the plot is placed on top of the existing
    graphics region.}
  \item{axis.lty}{A value passed as the \code{lty} argument to
    \code{\link[graphics]{axis}} when plotting the text (column name)
    axis.} 
  \item{\dots}{Other graphics parameters passed to
    \code{\link[graphics]{rect}}, \code{\link[graphics]{axis}} and
    \code{\link[graphics]{title}}.}
}
\details{
  This is a more detailed version of the \code{\link{stackedBars}}
  graph which allows finer control.  It is used mainly by
  \code{\link{compareBars}}.

  There are some differences from \code{\link{stackedBars}}.  First,
  \code{height} can be any value, not just a vector of probability.
  Second, \code{offset} is given as a numeric value in the units of
  height, rather than as an index into the array of heights.  Most of
  the rest of the arguments merely expose the graphical arguments to the
  user.
  
}
\value{
  The midpoints of the bars are returned invisibly.
}
\author{Russell Almond}
\seealso{\code{\link{compareBars}}, \code{\link{colorspread}},
  \code{\link{buildMarginTab}}, \code{\link{marginTab}},
  \code{\link[graphics]{barplot}},\code{\link{stackedBars}}
}
\examples{
margins <- data.frame (
 Trouble=c(Novice=.19,Semester1=.24,Semester2=.28,Semseter3=.20,Semester4=.09),
 NDK=c(Novice=.01,Semester1=.09,Semester2=.35,Semseter3=.41,Semester4=.14),
 Model=c(Novice=.19,Semester1=.28,Semester2=.31,Semseter3=.18,Semester4=.04)
)
margins <- as.matrix(margins)
baseline <- apply(margins[1:2,],2,sum)

stackedBarplot(margins,offset=-baseline,
            main="Margininal Distributions for NetPASS skills",
            sub="Baseline at 2nd Semester level.",
            col=hsv(223/360,.2,0.10*(5:1)+.5))

}
\keyword{ hplot }
