\name{proflevelci}
\alias{proflevelci}
\title{Produce cumulative sum credibility intervals}
\description{
  Produces credibility intervals for hanging barplots.  Assumes that
  each column represents a sum of proportions and produces corresponding
  intervals for the cumulative sums.  Values hanging below and above the
  reference line are treated separately, and returned values below the
  reference are negative.
}
\usage{
proflevelci(data, profindex, limits=list(lower=.025,upper=.975),a=.5, b=.5)
}
\arguments{
  \item{data}{A matrix of data values where each column refers to a
    bar in barplot.  The values should be scaled so that the column sum
    is the number of individuals in that group.}
  \item{profindex}{The level in the chart which corresponds to the
    reference (proficiency) line.  This should be a positive integer
    less than \code{nrow(data)}.}
  \item{limits}{The upper and lower credibility limits.}
  \item{a}{Value for the \code{shape1} parameter of the beta prior.}
  \item{b}{Value for the \code{shape2} parameter of the beta prior.}
}
\details{
  For a stacked bar plot, the natural comparisons involve not category
  probabilities but the sum of the category probabilities up through the
  current bar.  For hanging bar plots, this should go in both
  directions.  So for example, if the categories are \dQuote{Below
    Basic}, \dQuote{Basic}, \dQuote{Proficient}, and \dQuote{Advanced},
  and the zero line is to be put between \dQuote{Basic} and
  \dQuote{Proficient}, then we need credibility intervals for
  Pr(\dQuote{Basic} or \dQuote{Below Basic}), Pr(\dQuote{Basic}),
  Pr(\dQuote{Proficient}), Pr(\dQuote{Proficient} or \dQuote{Advanced}).

  The \code{proflevelci} function splits the states up into those above
  the line and below the line using \code{profindex}.  It then generates
  credibility intervals using \code{\link{betaci}} for the cumulative
  sums in each group.  The primary purpose is to create confidence
  intervals for stacked bar charts (see \code{\link{compareBars2}}).
  
}
\value{
  A list of data sets of the same length as the \code{limits} argument.
  Each data set has the same shape as the \code{data} argument and
  represents a quantile of the data associated with the value in limits.
  With the default limits of \code{lower} and \code{upper}, the result
  is a list of two elements
  \item{lower}{Gives the lower bounds of the confidence interval.}
  \item{upper}{Gives the upper bounds of the confidence interval.}
}
\author{Russell Almond}
\seealso{\code{\link{betaci}}, \code{\link{compareBars2}} }
\examples{

margins <- data.frame (
 Trouble=c(Novice=19,Semester1=24,Semester2=28,Semseter3=20,Semester4=9),
 NDK=c(Novice=1,Semester1=9,Semester2=35,Semseter3=41,Semester4=14),
 Model=c(Novice=19,Semester1=28,Semester2=31,Semseter3=18,Semester4=4)
)

proflevelci(margins,3,limits=c(lower=.025,upper=.975))


}
\keyword{ distribution }

