\name{proflevelci}
\alias{proflevelci}
\title{Produce cumulative sum credability intervals}
\description{
  Produces credibility intervals for hanging barplots.  Assumes that
  each column represents a sum of proportions and produces corresponding
  intervals for the cumulative sums.  Values hanging below and above the
  reference line are treated separately, and returned values below the
  reference are negative.
}
\usage{
proflevelci(data, profindex, levels=list(lower=.025,upper=.975),a=.5, b=.5)
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
  ~~Todo~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{Russell Almond}
\note{ 

}
\seealso{\code{\link{betaci}},  }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(data,profindex) {
  ss <- matrix(colSums(data),nrow(data),ncol(data),byrow=TRUE)
  if (profindex == 1) {
    datlow <- data[profindex,]
    sslow <- ss[profindex,]
    ci.ll.low <- -qbeta(.025,datlow+.5,sslow-datlow+.5)
    ci.ul.low <- -qbeta(.925,datlow+.5,sslow-datlow+.5)
    dathigh <- apply(data[-1,],2,cumsum)
    sshigh <- ss[-1,]
    ci.ll.high <- qbeta(.025, dathigh+.5, sshigh - dathigh + .5)
    ci.ul.high <- qbeta(.925, dathigh+.5, sshigh - dathigh + .5)
  } else if (profindex == nrow(data)-1) {
    ## Need a reverse cumsum here.
    datlow <- apply(data[profindex:1,],2,cumsum)
    datlow <- datlow[profindex:1,]
    sslow <- ss[1:profindex,]
    ci.ll.low <- -qbeta(.025,datlow+.5,sslow-datlow+.5)
    ci.ul.low <- -qbeta(.925,datlow+.5,sslow-datlow+.5)
    dathigh <- data[profindex+1,]
    sshigh <- ss[profindex+1,]
    ci.ll.high <- qbeta(.025,dathigh+.5,sshigh-dathigh+.5)
    ci.ul.high <- qbeta(.925,dathigh+.5,sshigh-dathigh+.5)
  } else {
    offsets <- -apply(data[1:profindex,],2,sum)
    ## Need a reverse cumsum here.
    datlow <- apply(data[profindex:1,],2,cumsum)
    datlow <- datlow[profindex:1,]
    sslow <- ss[1:profindex,]
    ci.ll.low <- -qbeta(.025,datlow+.5,sslow-datlow+.5)
    ci.ul.low <- -qbeta(.925,datlow+.5,sslow-datlow+.5)
    dathigh <- apply(data[(profindex+1):nrow(data),],2,cumsum)
    sshigh <- ss[(profindex+1):nrow(data),]
    ci.ll.high <- qbeta(.025,dathigh+.5,sshigh-dathigh+.5)
    ci.ul.high <- qbeta(.925,dathigh+.5,sshigh-dathigh+.5)
  }
  list(lower=rbind(ci.ll.low,ci.ll.high),upper=rbind(ci.ul.low,ci.ul.high))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
