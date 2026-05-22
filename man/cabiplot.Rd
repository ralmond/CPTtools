\name{cabiplot}
\alias{cabiplot}
\title{Create a correspondence analysis biplot of a CPF}
\description{
  
  This performs a correspondence analysis (\code{\link[ca]{ca}}) on a 
  conditional probability table and then plots a biplot.  Note that there
  must be at least 3 states of the child variable for this to work.
}
\usage{
cabiplot(data, ..., arrows = c(FALSE, TRUE))
}
\arguments{
  \item{data}{A \code{\link{CPF}}, \code{\link{CPA}} or matrix giving a 
    conditional probability table.  Note that the child variable must have
    at least 3 states.}
  \item{\dots}{Additional arguments which are passed to 
    \code{\link[ca]{plot.ca}}.}
  \item{arrows}{A logical vector of length two indicating whether the rows 
    and columns should be plotted as arrows.  Overrides default in 
    \code{plot.ca}.}
}
\details{

  Correspondence analysis is similar to principle components analysis in
  that it establishes a number of latent dimensions (one fewer than the number
  of states of the child variable) and projects both the rows and columns
  onto those dimensions.  It then plots the rows (by default as points) 
  and the columns (by default as arrows).

}
\value{
  
  Invisibly returns the plotted points, see \code{\link[ca]{plot.ca}}.

}
\references{

  Friendly, M., & Meyer, D. (2016). 
  \emph{Discrete Data Analysis with R.} CRC Press. Chapter 6.
  \url{https://datavis.ca/DDAR/}


}
\author{Russell Almond}
\note{

  Plotting function assumes at least two latent dimensions, so does not work
  with binary child variables where there is only one latent dimension.
}
\seealso{
  \code{\link{CPF}}, \code{\link[ca]{ca}}, \code{\link[ca]{plot.ca}}
}
\examples{

## Set up variables
skill3 <- c("High","Medium","Low") 
creditL <- c("Full","Partial","None")


cptPCcomp <- calcDPCFrame(list(S1=skill3,S2=skill3),creditL,
                          log(c(S1=1,S2=.75)),list(1.0,0.0),
                          rule="Compensatory",
                          link="partialCredit")
cptPCconj <- calcDPCFrame(list(S1=skill3,S2=skill3),creditL,
                          log(1),list(c(S1=.5,S2=-.5),c(S1=-.5,S2=.5)),
                          rule="OffsetConjunctive",
                          link="partialCredit")
cptPCdisj <- calcDPCFrame(list(S1=skill3,S2=skill3),creditL,
                          log(1),list(c(S1=.5,S2=-.5),c(S1=-.5,S2=.5)),
                          rule="OffsetDisjunctive",
                          link="partialCredit")
cabiplot(cptPCcomp, main="Partial Credit, Compensatory")
cabiplot(cptPCconj, main="Partial Credit, Conjunctive")
cabiplot(cptPCdisj, main="Partial Credit, Disjunctive")


cptGRcomp <- calcDPCFrame(list(S1=skill3,S2=skill3),creditL,
                          log(c(S1=1,S2=.75)),list(1.0,0.0),
                          rule="Compensatory",
                          link="gradedResponse")
cptGRconj <- calcDPCFrame(list(S1=skill3,S2=skill3),creditL,
                          log(1),list(c(S1=.5,S2=0),c(S1=-.5,S2=-.5)),
                          rule="OffsetConjunctive",
                          link="gradedResponse")
cptGRdisj <- calcDPCFrame(list(S1=skill3,S2=skill3),creditL,
                          log(1),list(c(S1=.5,S2=0),c(S1=-.5,S2=-.5)),
                          rule="OffsetDisjunctive",
                          link="gradedResponse")

cabiplot(cptGRcomp, main="Graded Response, Compensatory")
cabiplot(cptGRconj, main="Graded Response, Conjunctive")
cabiplot(cptGRdisj, main="Graded Response, Disjunctive")

}
\keyword{ aplot }
\keyword{ array }
\concept{ CPF }
\concept{ correspondence analysis }
