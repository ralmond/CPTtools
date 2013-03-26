\name{rescaleTable}
\alias{rescaleTable}
\alias{normalizeTable}
\title{Rescales the numeric part of the table}
\description{
  Takes a table representing a conditional probability distribution or a
  set of hyper-Dirichlet parameters and rescales the numeric part of the
  table.  The function \code{rescaleTable()} scales the table by
  \code{scaleFactor}, the function \code{normalizeTable()} scales the
  function by the sum of the rows, making the result a conditional
  probability table.
}
\usage{
rescaleTable(table, scaleFactor)
normalizeTable(table)
}
\arguments{
  \item{table}{A data frame describing a conditional probability table.
    Assumes that the conditions are expressed as factor variables, and
    all numeric columns represent states of the child variable.}
  \item{scaleFactor}{A scaler or vector of length equal to the number of
    rows of \code{table}.}
}
\details{
  For \code{rescaleTable()}, every numeric column of \code{table} is
  multiplied by \code{scaleFactor}.  This can be used to create a set of
  hyper-Drichlet parameters by multiplying a conditional probability
  table by the effective sample size.

  For \code{normalizeTable()}, the \code{scaleFactor} is set to be
  \code{1/rowSums(table)} (excluding the factor variables) so that the
  resulting table is a proper conditional probability table.
  
}
\value{
  A data frame of the same shape as \code{table} with the numeric
  enteries suitably scaled.
}
\author{Russell Almond}
\note{ 
  The function \code{\link{scaleTable}} does a similar rescaling, only
  it works with a separate \sQuote{Sum} and \sQuote{Scale} columns in
  the table.
}
\seealso{\code{\link{getTableStates}}, \code{\link{scaleTable}} }
\examples{

#conditional table
X2.ptf <- data.frame(Theta=c("Expert","Novice"),
                     correct=c(4,2),
                     incorrect=c(2,4))

X2.t99 <- rescaleTable(X2.ptf,99/6)  #Reweight to effective samples size of 99
X2.t31 <- rescaleTable(X2.ptf,c(3,1))  #Weight expert prior 3 times more than
                             #novice prior.
X2.dtf <- normalizeTable(X2.ptf)
\dontshow{
stopifnot(
identical(dim(X2.t99),dim(X2.ptf)),
identical(dim(X2.t31),dim(X2.ptf)),
identical(dim(X2.dtf),dim(X2.ptf)),
identical(X2.t99$Theta,X2.ptf$Theta),
identical(X2.t31$Theta,X2.ptf$Theta),
identical(X2.dtf$Theta,X2.ptf$Theta),
all(as.vector(numericPart(X2.t99)) == c(66,33,33,66)),
all(as.vector(numericPart(X2.t31)) == c(12,2,6,4)),
all(rowSums(numericPart(X2.dtf)) == 1)
)
}

#Unconditional table
Theta.ptf <- data.frame(Expert=3,Novice=3)
Theta.t100 <- rescaleTable(Theta.ptf,100/6)  #Reweight to effective
                                             #sample size of 100
Theta.dtf <- normalizeTable(Theta.ptf)
\dontshow{
stopifnot(
identical(dim(Theta.t100),dim(Theta.ptf)),
identical(dim(Theta.dtf),dim(Theta.ptf)),
all(Theta.t100 == 50),
all(Theta.dtf == .5)
)
}
}
\keyword{manip}

