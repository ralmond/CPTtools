\name{calcDSllike}
\alias{calcDSllike}
\alias{calcDNllike}
\title{Calculates the log-likelihood for data from a DiBello--Samejima
  (Normal) distribution}
\description{
  These functions take \code{data} which represent draws from a
  categorical data with the given DiBello--Samejima distribution and
  returns the log-likelihood of the given data.
}
\usage{
calcDSllike(data, parents, skillLevels, child, obsLevels,
            lnAlphas, beta, dinc = 0, rule = "Compensatory")
calcDNllike(data, parents, skillLevels, child, obsLevels, 
            lnAlphas, beta, std, rule = "Compensatory") 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A data frame whose columns contain variables corresponding
    to \code{parent} and \code{child}.}
  \item{parents}{A vector of names for the columns in \code{data}
    corresponding to the parent variables.}
  \item{child}{The name of the child variable, should refer to a column
    in \code{data}.}
  \item{skillLevels}{A list of character vectors giving names of levels
    for each of the condition variables.}
  \item{obsLevels}{A character vector giving names of levels for the
    output variables from highest to lowest.}
  \item{lnAlphas}{A vector of log slope parameters.  Its length should
    be either 1 or the length of \code{skillLevels}, depending on the
    choice of \code{rule}.}
  \item{beta}{A vector of difficulty (-intercept) parameters. Its length
    should be either 1 or the length of \code{skillLevels}, depending on
    the choice of \code{rule}.}
  \item{dinc}{Vector of difficulty increment parameters (see
    \code{\link{calcDSTable}}).} 
  \item{rule}{Function for computing effective theta (see
    \code{\link{calcDSTable}}).} 
  \item{std}{The log of the residual standard deviation (see Details).}
}
\details{
  This function assumes that the observed data are independent draws
  from a Bayesian network.  This function calculates the log-likelihood
  of a single conditional probability table.  First, it calculates a
  table of counts corresponding states of the parent and child variables
  using the function \code{\link{dataTable}}.  Next it calculates the
  conditional probability for each cell using the function
  \code{\link{calcDSTable}} or \code{\link{calcDNTable}}.

  It then calculates the log-likelihood as the sum of
  \eqn{count(cell)*log(Pr(cell))} where this value is set to zero if
  \eqn{count(cell)} is zero (this allows cells with zero probability as
  long as the count is also zero).  
}
\value{
  A real giving the log-likelihood of the observed data.
}
\references{
  \url{http://comet.research.ets.org/~ralmond/StatShop}
}
\author{Russell Almond}
\note{
  This function is primarily about testing the log likelihood
  calculations used internally in StatShop.
  
  This function is largely superceeded by the likelihood calculation internal
  to \code{\link{mapDPC}}.  In particular, if \code{probs} is the result of the 
  call to \code{\link{calcDPCTable}}, and \code{postTable} is the expected 
  contingency table (e.g., the output of \code{\link{expTable}}).  Then the 
  log likelihood is
  \code{-2*sum(as.vector(postTable)*as.vector(log(probs)))}.
  
}
\seealso{\code{\link{dataTable}}, \code{\link{calcDSTable}},
  \code{\link{Compensatory}},\code{\link{OffsetConjunctive}},
  \code{\link{eThetaFrame}}, \code{\link{calcDNTable}}
}
\examples{
  skill1l <- c("High","Medium","Low") 
  skill3l <- c("High","Better","Medium","Worse","Low") 
  correctL <- c("Correct","Incorrect") 

  x <- read.csv(system.file("testFiles", "randomPinned100.csv", 
                package="CPTtools"),
              header=TRUE, as.is=TRUE)
  x[,"Skill1"] <- ordered(x[,"Skill1"],skill1l)
  x[,"Skill3"] <- ordered(x[,"Skill3"],skill3l)
  x[,"Comp.Correct"] <- ordered(x[,"Comp.Correct"],correctL)


  like <- calcDSllike(x,c("Skill1","Skill3"),
                      list(Skill1=skill1l, Skill3=skill3l),
                      "Comp.Correct", correctL,
                      log(c(0.45,-0.4)),-1.9,rule="Compensatory")
  \dontshow{
   ##stopifnot(round(like,4) == -43.2538)
   }

}
\keyword{distribution}% at least one, from doc/KEYWORDS
