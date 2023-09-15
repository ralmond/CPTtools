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

  \deqn{I[X;Y] = \sum_{x}\sum_{y} \Pr(X=x, Y=y) \log
    \frac{\Pr(X=x, Y=y)}{\Pr(X=x)\Pr(Y=y)}}{
    I[X;Y] = sum Pr(X=x, Y=y) log [Pr(X=x,Y=y)/Pr(X=x)Pr(Y=y)]
    where the sum is taken over all possible values of x and y.
    }
  
}
\note{

  For square matrixes, the maximum mutual information, which should
  should correspond to a diagnoal matrix, is \code{log(d)} where 
  \code{d} is the number of dimensions.
}
\references{
  \code{https://en.wikipedia.org/wiki/Mutual_information}
  
  Shannon (1948) ``A Mathematical Theory of Communication.''
}
\author{Russell Almond}
\seealso{\code{\link[base]{table}}, \code{\link{CPF}}, 
         \code{\link{ewoe.CPF}}, \code{\link{expTable}}
}
\examples{
## UCBAdmissions is a three way table, so we need to
## make it a two way table.
mutualInformation(apply(UCBAdmissions,c(1,2),sum))
apply(UCBAdmissions,3,mutualInformation)
apply(UCBAdmissions,2,mutualInformation)
apply(UCBAdmissions,1,mutualInformation)

print(c(mutualInformation(matrix(1,2,2)),0))
print(c(mutualInformation(diag(2)),
        mutualInformation(1-diag(2)), log(2)))
print(c(mutualInformation(diag(3)),log(3)))

}
\keyword{multivariate}
\concept{measures of agreement}
