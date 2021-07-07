\name{OCP.CPF}
\alias{OCP.CPF}
\title{Plots a conditional probability distribuiton with data overlayed}
\description{


%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
OCP.CPF(obs, exp, data = NULL, ...,
        baseCol = "chocolate",
        limits = c(lower = 0.025, upper = 0.975),
        auto.key = TRUE, par.settings = list())
}
\arguments{
  \item{obs}{A \code{\link{CPF}} which contains the observed data.}
  \item{exp}{A \code{\link{CPF}} which contains the candidate distribution.}
  \item{\dots}{Other arguments passed to \code{\link[lattice]{barchart}.}
  \item{baseCol}{The base color to use for the color gradients in the bars.}
  \item{limits}{A vector of two probabilities representing the quantiles
    of the beta distribution to be used in error bars.}
  \item{auto.key}{Should a key be automaticall generated.}
  \item{par.settings}{Setting passed to \code{\link[lattice]{barchart}.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

  Returns the chi-square value with the degrees of freedom as an attribute.

}
\references{
%% ~put references to the literature/web site here ~
}
\author{Russell Almond}
\note{
%%  ~~further notes~~
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{


  skill1l <- c("High","Medium","Low") 
  skill2l <- c("High","Medium","Low","LowerYet") 
  correctL <- c("Correct","Incorrect") 
  pcreditL <- c("Full","Partial","None")
  gradeL <- c("A","B","C","D","E") 

  cpfTheta <- calcDPCFrame(list(),skill1l,numeric(),0,rule="Compensatory",
                           link="normalLink",linkScale=.5)
  
  datTheta <- cpfTheta ## Copy to get the shape
  datTheta[1,] <- rmultinom(1,25,cpfTheta[1,])


  cptComp <- calcDPCFrame(list(S2=skill2l,S1=skill1l),correctL,
                          lnAlphas=log(c(1.2,.8)), betas=0,
                          rule="Compensatory")

  datComp <- cptComp
  for (i in 1:nrow(datComp))
      ## Each row has a random sample size.
      datComp[i,3:4] <- rmultinom(1,rnbinom(1,mu=15,size=1),cptComp[i,3:4])
  datComp


  cptPC1 <- calcDPCFrame(list(S1=skill1l,S2=skill2l),pcreditL,
                         lnAlphas=log(1),
                         betas=list(full=c(S1=0,S2=999),partial=c(S2=999,S2=0)),
                         rule="OffsetDisjunctive")


  datPC1 <- cptPC1
  for (i in 1:nrow(datPC1))
      ## Each row has a random sample size.
      datPC1[i,3:5] <- rmultinom(1,rnbinom(1,mu=25,size=1),cptPC1[i,3:5])
  datPC1

  OCP.CPF(datTheta,cpfTheta)

  OCP.CPF(datComp,cptComp)



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
