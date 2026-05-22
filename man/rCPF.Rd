\name{rCPF}
\alias{rCPF}
\alias{rpCPF}
\title{Generate random data from a CPF model}
\description{

  Using the \code{model} argument, generates random data from the model.
  The value \code{size} is the number of cases for each row (replicated as 
  needed).  If \code{rpCPF} is called, then the number of cases per row 
  follows a Poisson distribution with mean \code{size}.

}
\usage{
rCPF(size, model)
rpCPF(size, model)
}
\arguments{
  \item{size}{The number of cases to draw for each row of the table, recycled 
    as necessary.  In the case of \code{rpCPF} this is the expected number of 
    cases per row.}
  \item{model}{A \code{\link{CPF}} which contains the conditional probability
    distribution for each row.  This is normalized by the function 
    \code{\link{normalize}()}.}
}
\details{
  
  The normalized rows of \code{model} are probabilities for a (conditional)
  multinomial distribution.  Each row of the resulting table is
  \code{\link[stats]{rmultinom}(1,size[irow],model[irow,])}.  Note that the 
  \code{size} argument is replicated to the number of rows of models.  
  In particular, if a single value is given, then all of the rows will have
  the same size.
  
  For \code{rpCPF} random sizes for the rows are drawn from a Poisson 
  distribution (\code{\link[stats]{rpois}()}).  Because of replication, if \code{size} 
  is a scalar all rows will have the same expected size, but actual sizes will
  differ.

}
\value{

  A new \code{\link{CPF}} of the same shape as \code{model} whose elements 
  give cell counts for the drawn states.  The row sums will match 
  \code{size}, or expected row sums for \code{rpCPF}.

}
\author{Russell Almond}
\seealso{

  \code{\link{CPF}}, \code{\link[stats]{rmultinom}}, 
  \code{\link[stats]{rpois}}

}
\examples{

## Set up variables
skill3 <- c("High","Medium","Low") 
correctL <- c("Correct","Incorrect") 

## Simple binary model, these three should be the same.
cptCorrect <- calcDPCFrame(list(S1=skill3,S2=skill3),correctL,
                          log(c(S1=1,S2=.75)),1.0,rule="Compensatory",
                          link="partialCredit")

rCPF(25,cptCorrect)
rpCPF(25,cptCorrect)
rCPF(c(50,25,10,25,50,25,10,25,50),cptCorrect)

}
\keyword{ matrix }
\keyword{ datagen }
\keyword{ distribution }
\concept{ CPF }
