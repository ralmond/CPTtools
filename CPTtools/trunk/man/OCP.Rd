\name{OCP2}
\alias{OCP}
\alias{OCP2}
\title{Observable Characteristic Plot
}
\description{

  The observable characteristic plot is an analogue of the item
  characteristic curve for latent class models.  Estimates of the class
  success probability for each latent is plotted.  Reference lines are
  added for various groups which are expected to have roughly the same
  probability of success.
  
}
\usage{
OCP(x, n, lclabs, pi, pilab = names(pi), lcnames = names(x),
    a = 0.5, b = 0.5, reflty = 1, ..., newplot = TRUE,
    main = NULL, sub = NULL, xlab = "Latent Classes", ylab = "Probability",
    cex = par("cex"), xlim = NULL, ylim = NULL, cex.axis = par("cex.axis"))
OCP2(x, n, lclabs, pi, pilab = names(pi), lcnames = names(x),
     set1 = seq(1, length(x)-1, 2), setlabs = c("Set1", "Set2"),
     setat = -1, a = 0.5, b = 0.5, reflty = 1, ..., newplot = TRUE,
     main = NULL, sub = NULL, xlab = "Latent Classes", ylab = "Probability",
     cex = par("cex"), xlim = NULL, ylim = NULL, cex.axis = par("cex.axis")) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Vector of success counts for each latent class.
}
  \item{n}{Vector of of the same size as \code{x} of latent class sizes. 
}
  \item{lclabs}{Character vector of plotting symbols for each latent class
}
  \item{pi}{Vector of probability estimates for various levels.
}
  \item{pilab}{Character vector of names for each level.
}
  \item{lcnames}{Character vector of names for each latent class for axis.
}
  \item{set1}{For \code{OCP2}, a vector of indexes of the elements of
    \code{x} that form the first set.
}
  \item{setlabs}{Character vectors of set labels for \code{OCP2}.
}
  \item{setat}{Numeric scalar giving the x-coordinate for set labels.
}
  \item{a}{The first parameter for beta pseudo-prior, passed to
    \code{\link{betaci}}.  This should either be a scalar or the same
    length as \code{x}.
}
  \item{b}{The second parameter for beta pseudo-prior, passed to
    \code{\link{betaci}}.  This should either be a scalar or the same
    length as \code{n}.
}
  \item{reflty}{Provides the line type (see
    \code{\link[graphics]{par}(lty)}) for reference lines.  Should be
    scalar or same length as \code{pi}.
}
  \item{\dots}{Additional graphics parameters passed to
    \code{\link[graphics]{plot.window}} and \code{\link[graphics]{title}}.
}
  \item{newplot}{Logical.  If true, a new plotting window is created.
    Otherwise, information is added to existing plotting window.
}
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
  \item{cex}{Scalar giving character expansion for plotting symbols (see
    \code{\link[graphics]{par}(cex)}).
}
  \item{xlim}{Plotting limits for x-axis (see
    \code{\link[graphics]{plot.window}}). 
}
  \item{ylim}{Plotting limits for y-axis (see
    \code{\link[graphics]{plot.window}}). 
}
  \item{cex.axis}{Scalar giving character expansion for latent class
    names (axis tick labels; \code{cex} values passed to
    \code{\link[graphics]{par}(axis)}). 
}
}
\details{

  Most cognitively diagostic models for assessments assume that students
  with different patterns of skills will have different patterns of
  success and failures.  The goal of this plot type is to empirically
  check to see if the various groups conform to their expected
  probability.

  The assumption is that the population is divided into a number of
  \emph{latent classes} (or groups of latent classes) and that the class
  label for each member of the class is known.  The latent classes are
  partitioned into \emph{levels}, where each level is assumed to have
  roughly the same proportion of success.  (This is often
  \code{c("-","+")} for negative and positive skill patterns, but there
  could be multiple levels.)

  The key idea of the observable characteristic plot is to compare a
  credibility interval for the success probability in each latent class,
  to the modelled success probability given by its level.  A beta
  credibility interval is computed for each latent class using
  \code{\link{betaci}(x,n,a,b)}, with the expected value set at
  \eqn{(x+a)/(n+a+b)}.  The plotting symbols given in \code{lclabs} are
  plotted at the mean (usually, these correspond to the latent class is
  in), with vertical error bars determined by \code{betaci}.  Horizontal
  reference lines are added at the values given by \code{pi}.  The idea
  is that if the error bars cross the reference line for the appropriate
  level, then the latent class fits the model, if not it does not.

  The variant \code{OCP2} is the same except that the latent classes are
  further partitioned into two sets, and the labels for the latent
  classes for different sets are plotted on different lines.  This is
  particularly useful for testing whether attributes which are not
  thought to be relevant to a given problem are actually irrelvant.

}
\value{
  The output of \code{\link{betaci}} is returned invisibly.
}
\references{
  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and
  Yan, D. (Draft) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 10.

  Sinharay, S. and Almond, R.G. (2006).  Assessing Fit of Cognitively
  Diagnostic Models:  A case study.  \emph{Educational and Psychological
    Measurement}.  \bold{67}(2), 239--257.

  Sinharay, S., Almond, R. G. and Yan, D. (2004).  Assessing fit of
    models with discrete proficiency variables in educational
    assessment.  ETS Research Report.
    \url{http://www.ets.org/research/researcher/RR-04-07.html}
}
\author{Russell Almond}
\seealso{
  \code{\link{betaci}}
}
\examples{

nn <- c(30,15,20,35)
pi <- c("+"=.15,"-"=.85)
grouplabs <- c(rep("-",3),"+")
x <- c("(0,0)"=7,"(0,1)"=4,"(1,0)"=2,"(1,1)"=31)
OCP (x,nn,grouplabs,pi,c("-","+"),ylim=c(0,1), reflty=c(2,4),
     main="Data that fit the model")

x1 <- c("(0,0)"=7,"(0,1)"=4,"(1,0)"=11,"(1,1)"=31)
OCP (x1,nn,grouplabs,pi,c("-","+"),ylim=c(0,1), reflty=c(2,4),
     main="Data that don't fit the model")

nnn <- c("(0,0,0)"=20,"(0,0,1)"=10,
         "(0,1,0)"=10,"(0,1,0)"=5,
         "(1,0,0)"=10,"(1,0,1)"=10,
         "(1,1,1)"=10,"(1,1,1)"=25)
xx <- c("(0,0,0)"=5,"(0,0,1)"=2,
       "(0,1,0)"=2,"(0,1,1)"=2,
       "(1,0,0)"=2,"(1,0,1)"=0,
       "(1,1,0)"=9,"(1,1,1)"=21)
grouplabs1 <- rep(grouplabs,each=2)

OCP2 (xx,nnn,grouplabs1,pi,c("-","+"),ylim=c(0,1), reflty=c(2,4),
     setlabs=c("Low Skill3","High Skill3"),setat=-.8,
     main="Data for which Skill 3 is irrelevant")

xx1 <- c("(0,0,0)"=2,"(0,0,1)"=5,
       "(0,1,0)"=1,"(0,1,1)"=3,
       "(1,0,0)"=0,"(1,0,1)"=2,
       "(1,1,0)"=5,"(1,1,1)"=24)
OCP2 (xx1,nnn,grouplabs1,pi,c("-","+"),ylim=c(0,1), reflty=c(2,4),
      setlabs=c("Low Skill3","High Skill3"),setat=-.8,
      main="Data for which Skill 3 is relevant")

}
\keyword{graphics}

