\name{compareBars}
\alias{compareBars}
\title{ Produces comparison stacked bar charts for two sets of groups}
\description{
  This produces set of stacked bar charts grouped for comparison between
  two groups.  For example, if suppose that there is a set of
  probabilities over a collection of proficiency variables measures both
  before and after obtaining a certain piece of evidence.  The
  \code{compareBars} function would produce stacked bar charts which
  compare the prior and posterior probabilities for each variable.
}
\usage{
compareBars(data1, data2, profindex, groupNames = c("Prior", "Post"), ..., ylim = c(min(offsets) - 0.25, max(1 + offsets)), cex.names = par("cex.axis"), digits = 2)
}

\arguments{
  \item{data1}{ Data set with first (prior) values}
  \item{data2}{ Data set with second (posterior) values }
  \item{profindex}{ Index of one of the proficiency levels which will be
  used as the baseline for the stacked bar charts.}
  \item{groupNames}{ Names of the groups represented by \code{data1} and
    \code{data2} respectively.}
  \item{\dots}{ Other arguments to \code{barplot}.}
  \item{ylim}{ Default limits for Y axis.}
  \item{cex.names}{ Magnification for names. }
  \item{digits}{ Number of digits for overlayed numeric variables.}
}
\author{Russell Almond }
\seealso{\code{\link{stackedBars}}, \code{\link{Colors}},
  \code{\link{buildFactorTab}}, \code{\link[graphics]{barplot}}}
\examples{
margins.prior <- data.frame (
 Trouble=c(Novice=.19,Semester1=.24,Semester2=.28,Semseter3=.20,Semester4=.09),
 NDK=c(Novice=.01,Semester1=.09,Semester2=.35,Semseter3=.41,Semester4=.14),
 Model=c(Novice=.19,Semester1=.28,Semester2=.31,Semseter3=.18,Semester4=.04)
)

margins.post <- data.frame(
 Trouble=c(Novice=.03,Semester1=.15,Semester2=.39,Semseter3=.32,Semester4=.11),
 NDK=c(Novice=.00,Semester1=.03,Semester2=.28,Semseter3=.52,Semester4=.17),
 Model=c(Novice=.10,Semester1=.25,Semester2=.37,Semseter3=.23,Semester4=.05))

foo <-
compareBars(netpass.margins.prior,netpass.margins.post,3,c("Prior","Post"),
            main="Margins before/after Medium TroubleShooting Task",
            sub="Observables:  cfgCor=Medium, logCor=High, logEff=Medium",
            legend.loc = "topright",
            cex.names=.75, col1=hsv(h=.1,s=.2*1:5-.1,gamma=1),
            col2=hsv(h=.6,s=.2*1:5-.1,gamma=1))

compareBars2(netpass.margins.prior,25*netpass.margins.post,3,c("Prior","Post"),
            main="Margins before/after Medium TroubleShooting Task",
            sub="Observables:  cfgCor=Medium, logCor=High, logEff=Medium",
            legend.loc = "topright",
            cex.names=.75, col1=hsv(h=.1,s=.2*1:5-.1,gamma=1),
            col2=hsv(h=.6,s=.2*1:5-.1,gamma=1))




}
\keyword{hplot}% at least one, from doc/KEYWORDS

