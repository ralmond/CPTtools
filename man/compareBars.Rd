\name{compareBars}
\alias{compareBars}
\alias{compareBars2}
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
compareBars(data1, data2, profindex,
            groupNames = c(deparse(data1), deparse(data2)), ...,
            ylim = c(min(offsets) - 0.25, max(1 + offsets)),
            cex.names = par("cex.axis"), digits = 2, legend.loc = c(0,1),
            legend.cex = par("cex"), col = par("col"), col1 = NULL,
            col2 = NULL, main = NULL, sub = NULL, xlab = NULL,
            ylab = NULL, rotlab = FALSE)

compareBars2(data1, data2, profindex,
             groupNames=c("Prior","Post"), error.bars=2, scale=100,
             err.col="gray20", ..., ylim = NULL) 

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
  \item{cex.names}{Character magnification for names. }
  \item{digits}{ Number of digits for overlaid numeric variables.}
  \item{legend.loc}{Location for legend, see
    \code{\link[graphics]{legend}}.}
  \item{legend.cex}{Character magnification for legend. }
  \item{col}{The normal graphics \code{col} parameter (see
    \code{\link[graphics]{par}}, passed through to other graphics operators
  using \code{...}.  Is also the default for \code{col1} and \code{col2}
  if those values are not supplied.}
  \item{col1}{Color scale for the first data set.  This should be a vector
  of colors equal to the number of groups.}
  \item{col2}{Color scale for the second data set.  This should be a vector
  of colors equal to the number of groups.}
  \item{main}{Character scalar giving main title (see
    \code{\link[graphics]{title}}). 
}
  \item{sub}{Character scalar giving sub title (see
    \code{\link[graphics]{title}}). 
}
  \item{xlab}{Character scalar giving x-axis label (see
    \code{\link[graphics]{title}}). 
}
  \item{ylab}{Character scalar giving x-axis label (see
    \code{\link[graphics]{title}}). 
  }
  \item{rotlab}{If \code{TRUE} labels are rotated 90 degrees.}
  \item{error.bars}{The number of standard errors for error bars.}
  \item{err.col}{The color for error bars.}
  \item{scale}{Scales data as probabilities (\code{scale=1}) or
  percentages (\code{scale=100}).}
}
\section{Note}{
  The function \code{compareBars2} is a somewhat experimental extension
  to \code{compareBars} which adds error bars to the posterior.  The
  result is not entirely satisfactory, and this function may change
  with future releases.
}
\references{
  Almond, R. G.,  Shute, V. J., Underwood, J. S., and Zapata-Rivera,
  J.-D (2009). Bayesian Networks: A Teacher's View. \emph{International
  Journal of Approximate Reasoning.} \bold{50}, 450-460.
}
\author{Russell Almond }
\seealso{\code{\link{stackedBars}}, \code{\link{colorspread}},
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
compareBars(margins.prior,margins.post,3,c("Prior","Post"),
            main="Margins before/after Medium Trouble Shooting Task",
            sub="Observables:  cfgCor=Medium, logCor=High, logEff=Medium",
            legend.loc = "topright",
            cex.names=.75, col1=hsv(h=.1,s=.2*1:5-.1,alpha=1),
            col2=hsv(h=.6,s=.2*1:5-.1,alpha=1))

compareBars2(margins.prior,25*margins.post,3,c("Prior","Post"),
            main="Margins before/after Medium Trouble Shooting Task",
            sub="Observables:  cfgCor=Medium, logCor=High, logEff=Medium",
            legend.loc = "topright",
            cex.names=.75, col1=hsv(h=.1,s=.2*1:5-.1,alpha=1),
            col2=hsv(h=.6,s=.2*1:5-.1,alpha=1))




}
\keyword{hplot}% at least one, from doc/KEYWORDS

