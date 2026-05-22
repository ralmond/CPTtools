\name{g2test}
\alias{g2test}
\title{A chi-squared test for whether observed data are consistent with model}
\description{

  Calculates a chi-squared statistic describing the distance between 
  the observed \code{data} and the expected value under a \code{model}.
}
\usage{
g2test(data, model, type = c("deviance", "Pearson", "FT"), cc = TRUE)
}
\arguments{
  \item{data}{A \code{\link{CPF}}, \code{\link{CPA}} or matrix giving the 
    observed cell counts.  Note that if the parent variables are latent these 
    could be the expected observed counts.}
  \item{model}{A \code{\link{CPF}}, \code{\link{CPA}} or matrix of the same
    size as \code{data} giving the conditional probabilities under the model.
    Note that this will be normalized (\code{\link{normalize}}).}
  \item{type}{A character value indicating the specific statistic to be 
    calculated; default is \dQuote{deviance}.}
  \item{cc}{A logical value; if true, the normalized model will be added to
    the data to eliminate zero cells.}
}
\details{

   Call the elements of the \code{data} matrix, $x_i$.
  The \code{model} is coverted to an expected matrix using 
  \code{\link{expected}()}, call its elements $m_i$.  The residual is based 
  on the contribution to the chi-squared measure of agreement.  The
  three residual types are:
  
  \emph{Deviance.} \deqn{G^2 = \sum 2 x_i\log(x_i/m_i)}
  
  \emph{Pearson.} \deqn{\chi^2=\frac{(x_i-m_i)^2}{m_i}}
  
  \emph{Freedman--Tukey.} \deqn{K^2 = 4(\sqrt{x_i}-\sqrt{m_i})^2}
  
  The degrees of freedom returned with the statistic is based on the raw data, 
  that is, it is \code{nrow(data)*(ncol(data) - 1)}.  Generally, the number of 
  parameters of the model must be subtracted to get the right degrees of 
  freedom for a chi-square test.

}
\value{
  
  Returns a numeric value giving the deviance by whatever measure with an 
  attribute giving the raw degrees of freedom.
}
\references{

  Bishop, Y. M., Fienberg, S. E., & Holland, P. W. (1975). 
  \emph{Discrete Multivariate Analysis.} MIT Press. p 513.

}
\author{Russell Almond}
\seealso{
  \code{\link{CPA}}, \code{\link{CPF}}, \code{\link{normalize}}
  
  \code{\link{expected}}, \code{\link{resids}}, 
  \code{\link{mosaic2}}, \code{\link{assoc2}}

  \code{\link{cptChi2}} and \code{\link{cptG2}} are depricated equivalent 
  functions.
}
\examples{
## Set up variables
skill3 <- c("High","Medium","Low") 
correctL <- c("Correct","Incorrect") 

cptCorrect <- calcDPCFrame(list(S1=skill3,S2=skill3),correctL,
                          log(c(S1=1,S2=.75)),1.0,rule="Compensatory",
                          link="partialCredit")

data <- rpCPF(25,cptCorrect)

g2test(data,cptCorrect)
g2test(data,cptCorrect,type="Pearson")
g2test(data,cptCorrect,type="FT")
g2test(data,cptCorrect,cc=FALSE)


}
\keyword{ htest }
\concept{ CPF }
