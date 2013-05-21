\name{colorspread}
\alias{colorspread}
\title{Produces an ordered palate of colours with the same hue.}
\description{
  This takes a colour specification, and produces an ordered series of
  colours by manipulating the saturate (and possibly value) of the
  color, leaving the hue constant.  This produces a colour palate
  suitable for plotting ordered factors, which looks good on a colour
  display, but also reproduces well on a grayscale printer (or for
  persons with limited colour perception).
}
\usage{
colorspread(col, steps, maxsat = FALSE, rampval = FALSE)
}
\arguments{
  \item{col}{
    A color in any format suitable as input to
    \code{\link[grDevice]{col2rgb}}. 
}
  \item{steps}{
    A integer describing the number of colors to create.
}
  \item{maxsat}{
    A logical value.  If true, the final color in the series will have
    saturation 1, instead of whatever is appropriate for the input.
}
  \item{rampval}{
    A logical value.  If ture, the value as well as the saturation of
    the color is rampe.
}
}
\details{
  The colour is converted to a RGB value using
  \code{\link[grDevice]{col2rgb}} and then to an HSV value using
  \code{\link[grDevice]{rgb2hsv}}.  The saturation is then scaled into
  \code{steps} equal intervals.  If requested, the \code{value} is
  scaled as well.
}
\value{
  A character vectors of length \code{steps} giving the colour palate
  from lowest to highest intensity.  This is suitable to passing to the
  \code{col} argument of most graphics functions.
}
\author{Russell Almond}
\note{
  Many of the built-in colours come with 4 intensity variants are meant to
  work well together.  In some cases an expression like
  \code{paste("firebrick",1:4,sep="")} may work better than
  colorspread.  To see the built-in colours, use the
  \code{\link{colors}} function. 
}
\seealso{
  \code{\link{compareBars}}, \code{link{stackedBars}}
}
\examples{
barplot(rep(1,4),col=colorspread("slategray",4))
barplot(rep(1,4),col=colorspread("slategray",4,maxsat=TRUE))
barplot(rep(1,4),col=colorspread("violetred",4))
barplot(rep(1,4),col=colorspread("violetred",4,rampval=TRUE))
}
\keyword{graphics}

