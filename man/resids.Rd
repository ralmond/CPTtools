\name{resids}
\alias{resids}
\alias{resids.array}
\alias{resids.CPF}
\title{Calculate residuals for differences between model and data}
\description{

  Calculates a matrix of the same shape as the inputs giving the 
  contribution of that cell to discrepency between \code{model} and 
  \code{data}.

}
\usage{
resids(data, model, type = c("deviance", "Pearson", "FT"),...)
\method{resids}{array}(data, model, type = c("deviance", "Pearson", "FT"),...,
                       factorsAsStrings)
\method{resids}{CPF}(data, model, type = c("deviance", "Pearson", "FT"),...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A \code{\link{CPF}}, \code{\link{CPA}}, or matrix giving 
    (expected) cell counts.}
  \item{model}{A \code{\link{CPF}}, \code{\link{CPA}}, or matrix giving 
    the (unnormalized) conditional probability distribution.  The \code{model} 
    and \code{data} objects should have the same dimensions.}
  \item{type}{A character value indicating which type of residuals should be
    calculated.  Default is \dQuote{deviance}.}
  \item{...}{To allow methods to have other arguments.}
  \item{factorsAsStrings}{A logical value, if true and model must be coerced 
    to a matrix, row names will be based on state labels.}
}
\details{

  Call the elements of the \code{data} matrix, $x_i$.
  The \code{model} is coverted to an expected matrix using 
  \code{\link{expected}()}, call its elements $m_i$.  The residual is based 
  on the contribution to the chi-squared measure of agreement.  The
  three residual types are:
  
  \emph{Deviance.} \deqn{\mtext{sign}(x_i-m_i)\sqrt{2\log(x_i/m_i) -2(x_i-m_i)}}
  
  \emph{Pearson.} \deqn{\frac{x_i-m_i}{\sqrt{m_i}}}
  
  \emph{Freedman--Tukey.} \deqn{2(\sqrt{x_i}-\sqrt{m_i})}
  
  

}
\value{
 
  The residuals are returned as a matrix.  If the argument is a 
  \code{CPF} or \code{CPA}, then the row names are based on the state names for
  the conditioning variables (see \code{\link{as.matrix.CPF}}).
}
\references{

  Bishop, Y. M., Fienberg, S. E., & Holland, P. W. (1975). 
  \emph{Discrete Multivariate Analysis.} MIT Press. p 513.
  
  Friendly, M., & Meyer, D. (2016). 
  \emph{Discrete Data Analysis with R.} CRC Press. 
  \url{https://datavis.ca/DDAR/} p 356.

}
\author{Russell Almond}
\note{

  Friendly & Meyer (2016) give the formula for the deviance residual as
  \deqn{\mtext{sign}(x_i-m_i)\sqrt{2\log(x_i/m_i) - 2(x_i-m_i)}\ .}  I'm not
  sure where the \eqn{2(x_i-m_i)} comes from as it drops out of the expression 
  for $G^2$ in Bishop, Fienberg and Holland (1975).
  
  
}
\seealso{
  \code{\link{CPA}}, \code{\link{CPF}}, \code{\link{normalize}}
  
  \code{\link{expected}}, \code{\link{g2test}}, \code{\link{mosaic2}}, \code{\link{assoc2}}
}
\examples{
arr <- array(1:24,c(2,3,4),
            dimnames=list(A=c("a1","a2"),B=c("b1","b2","b3"),
                          C=c("c1","c2","c3","c4")))
arf <- as.CPF(arr)

mod <- data.frame(A=factor(rep(c("a1","a2"),each=3)),
                  B=factor(rep(c("b1","b2","b3"),2)),
                  C.c1=rep(1/4,6), C.c2=rep(1/4,6), C.c3=rep(1/4,6), 
                  C.c4=rep(1/4,6))
mod <- as.CPF(mod)

resids(arf,mod)
resids(arf,mod,type="Pearson")
resids(arf,mod,type="FT")

}
\keyword{ category }
\keyword{ matrix }
\concept{ CPF }
\concept{ residuals }
