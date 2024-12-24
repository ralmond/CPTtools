\name{ewoe.CPF}
\alias{ewoe.CPF}
\title{Calculates the expected weight of evidence from a conditional probability frame.}
\description{

  The expected weight of evidence (EWOE) is a measure of how
  much information about a hypothesis can be learned from a
  potential observation.  The hypothesis corresponds to a 
  grouping of the rows of the \code{\link{CPF}} (and the 
  negation of the hypothesis to the remaining rows).

}
\usage{
ewoe.CPF(cpf, pos = 1L, neg = NULL)
}
\arguments{
  \item{cpf}{A conditional probability frame (\code{\link{CPF}}),  
  data frame, or matrix whose columns represent states of one
  variable, and rows represent configurations of the parent
  variable(s).}
  \item{pos}{An expression for selecting rows of the \code{cpf} 
    which corresponds to the hypothesis.}
  \item{neg}{An expression for selecting the rows corresponding to
    the complement of the hypothesis. (The default value is 
    \code{-pos} if \code{pos} is numeric; \code{!pos} if \code{pos}
    is logical, and \code{setdiff(rownames(cpf),pos} if \code{pos} 
    is a character vector.}
}
\details{

    Good (1985) defines the weight of evidence \eqn{E} for a hypothesis 
  \eqn{H} as 
  \deqn{W(H:E) = log \frac{P(E|H)}{P(E|\not H)} =
    log \frac{P(H|E)}{P(\not H|E)} - log \frac{P(H)}{P(\not H)}.}

  The expected weight of evidence (Good and Card, 1971) looks at
  potential future observations to find which might have the
  highest weight of evidence.  The expected weight of evidence is
  \deqn{EWOE(H:E) = \sum_{e in E} W(H:e) P(e|H) .}

  In this calculation, the potential evidence corresponds to the 
  columns of the (\code{\link{numericPart}}) of \code{cpf}.  The 
  hypothesis is found by splitting the rows.  The \code{pos} and 
  \code{neg} arguments can be any way of specifying a set of rows.
  
  This is similar to the \code{\link{mutualInformation}}, only EWOE
  works for a binary hypothesis, while 
  \code{\link{mutualInformation}} works for any number of states.

}
\value{
  A numeric value giving the weight of evidence in _centibans_ 
  (where the logs are taken base 10 and the result is multiplied by
  100).
}
\references{

 Good, I.J. (1985).  Weight of Evidence:  A brief survey.
 In Bernardo, J., DeGroot, M., Lindley,
 D. and Smith, A. (eds).  \emph{Bayesian Statistics 2}. North Holland.
 249--269. 
 
 Good, I. J. and Card, W. (1971). The Diagnostic Process with
 Special Reference to Errors.  \emph{Methods of Information in
 Medicine}, 10, 176--188.
}
\author{Russell Almond}
\seealso{
  \code{\link{CPF}}, \code{\link{mutualInformation}},
  \code{\link{expTable}}
}
\examples{
ACED <- dplyr::inner_join(ACED.scores,ACED.items,by="SubjID")
expcr <- expTable(ACED,"cr","tCommonRatio1a",
                  pvecregex="P\\\\.<var>\\\\.\\\\.<state>")
ewoe.CPF(expcr,"H")
ewoe.CPF(expcr,c("H","M"))


}
\keyword{multivariate}
\concept{measures of agreement}
