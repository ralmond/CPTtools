\name{mutualInformation}
\alias{mutualInformation}
\title{Calculates Mutual Information for a two-way table.}
\description{
  Calculates the mutual information for a two-way table of observed
  counts or a joint probability distribution.  The mutual information is
  a measure of association between two random variables.
}
\usage{
mutualInformation(table)
}
\arguments{
  \item{table}{A two way table or probability distribution.  Possibly
    the output of the \code{table} command.}
}
\details{
  This is basically the Kullback-Leibler distance between the joint
  probability distribution and the probability distribution created by
  assuming the marginal distributions are independent.  This is given in
  the following formula:

  \deqn{I[X;Y] = \sum_{x}\sum{y} \Pr(X=x, Y=y) \log
    \frac{\Pr(X=x, Y=y)}{\Pr(X=x)\Pr(Y=y)}}{
    I[X;Y] = sum Pr(X=x, Y=y) log [Pr(X=x,Y=y)/Pr(X=x)Pr(Y=y)]
    where the sum is taken over all possible values of x and y.
    }
  
}
\references{
  \code{http://planetmath.org/encyclopedia/MutualInformation.html}
  
  Shannon (1948) ``A Mathematical Theory of Communication.''
}
\author{Russell Almond}
\seealso{\code{\link[base]{table}}}
\examples{
## UCBAdmissions is a three way table, so we need to
## make it a two way table.
mutualInformation(apply(UCBAdmissions,c(1,2),sum))
apply(UCBAdmissions,3,mutualInformation)
apply(UCBAdmissions,2,mutualInformation)
apply(UCBAdmissions,1,mutualInformation)
}
\keyword{multivariate}% at least one, from doc/KEYWORDS

