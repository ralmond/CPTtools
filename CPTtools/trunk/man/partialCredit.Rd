\name{partialCredit}
\alias{partialCredit}
\title{A link function based on the generalized partial credit model}
\description{
  This function converts a matrix of effective theta values into a
  conditional probability table by applying the generalized partial
  credit model to each row of the table.
}
\usage{
partialCredit(et, linkScale = NULL, obsLevels = NULL)
}
\arguments{
  \item{et}{
    A matrix of effective theta values.  There should be one row in this
    table for each configuration of the parent variables of the
    conditional probability table and one column for each state of the
    child variables except for the last.
}
  \item{linkScale}{Unused.  For compatability with other link functions. 
}
  \item{obsLevels}{
    A character vector giving the names of the child variable states.
    If supplied, it should have length \code{nrow(et)+1}.  
}
}
\details{

  Let \eqn{X} be the child variable of the distribution, and assume that
  it can take on \eqn{M} possible states labeled \eqn{x_1} through
  \eqn{x_M} in increasing order.  The generalized partial
  credit model defines a set of functions
  \eqn{Z_m(\theta_1,\ldots,\theta_K)} for \eqn{m=2,...M}, where \deqn{
    Pr(X >= x_m | X >=x_{m-1}, \theta_1,...,\theta_K) = logit^{-1}
    -D*Z_m(\theta_1,\ldots,\theta_K)}
  The conditional probabilities for each child state given the effective
  thetas for the parent variables is then given by
  \deqn{Pr(X == x_m |\theta_1,...,\theta_K) \frac{\sum_{r=1}^m
  Z_r(\theta_1,\ldots,\theta_K)}{\sum_{r=1}^M
  Z_r(\theta_1,\ldots,\theta_K)}}

%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (et, k = nrow(et) + 1, linkScale = NULL, obsLevels = NULL) 
{
    zt <- t(apply(et, 1, function(x) rev(cumsum(rev(x)))))
    pt <- cbind(exp(1.7 * zt), 1)
    pt <- sweep(pt, 1, apply(pt, 1, sum), "/")
    probs <- pt[, 1:k]
    probs <- ifelse(probs < 0, 0, probs)
    if (!is.null(obsLevels)) {
        dimnames(probs) <- list(NULL, obsLevels)
    }
    probs
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
