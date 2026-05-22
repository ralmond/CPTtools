\name{mosaic2}
\alias{mosaic2}
\alias{assoc2}
\title{Mosaic/association plot comparing data to CPF model}
\description{

  Produces a \code{\link[vcd]{mosaic}} or \code{\link[vcd]{assoc}} plot,
  only the residuals compare the \code{data} to \code{model} instead of to
  independence.

}
\usage{
mosaic2(data, model, npar, ..., residuals_type = c("deviance", "Pearson", "FT"), 
        split_vertical=c(rep(F,length(dim(data))-1),T),
        spacing=vcd::spacing_increase,
        spacing_args=list(start=grid::unit(0.05,"lines"),rate=4),
        labeling_args=list(rot_labels=c(0,0,0,0)))
assoc2(data, model, npar, ..., residuals_type = c("deviance", "Pearson", "FT"), 
        split_vertical=c(rep(F,length(dim(data))-1),T),
        spacing=vcd::spacing_increase,
        spacing_args=list(start=grid::unit(0.05,"lines"),rate=4),
        labeling_args=list(rot_labels=c(0,0,0,0)))
}

\arguments{
  \item{data}{A \code{\link{CPF}}, \code{\link{CPA}} or matrix with the counts
    (or expected counts) corresponding to each combination of parents and child.}
  \item{model}{A \code{\link{CPF}}, \code{\link{CPA}} or matrix of the same
    size as \code{data} giving the conditional probability table model.}
  \item{npar}{An integer giving the number of free parameters in \code{model}.
    Used to calculate degrees of freedom.}
  \item{\dots}{Other arguments which are passed to \code{\link[vcd]{strucplot}}.
    Note that this includes a \code{core_args} field which is passed to
    \code{\link[vcd]{struc_mosaic}} or \code{\link[vcd]{struc_assoc}}.}
  \item{residuals_type}{A character value giving the method used to calculate
    residuals (see \code{\link{resids}}).}
  \item{split_vertical}{Controls axis used for splits. Default puts parent
    variables on side and child variable on top. See \code{\link[vcd]{strucplot}}.}
  \item{spacing}{Controls spacing between rows and columns.  Default adds
    extra space when second parent variable changes. See \code{\link[vcd]{strucplot}}.} 
  \item{spacing_args}{Arguments which control spacing. See \code{\link[vcd]{spacings}}.}
  \item{labeling_args}{Arguments which control labeling. See \code{\link[vcd]{labelings}}.}
}
\details{

  Expected values are calculated using, \code{\link{expected}}, residuals with
  \code{\link{resids}}, then the data, expected values and residuals are passed
  to \code{\link[vcd]{strucplot}} which does the actual plotting.

}
\value{
  
  Invisibly, an object of class \code{\link[vcd]{structable}}, 
  see \code{\link[vcd]{strucplot}}.

}
\references{

  Friendly, M., & Meyer, D. (2016). 
  \emph{Discrete Data Analysis with R.} CRC Press. 
  \url{https://datavis.ca/DDAR/}

}
\author{Russell Almond}
\seealso{

  \code{\link{CPF}}, \code{\link{expected}}, \code{\link{resids}}
  
  \code{\link[vcd]{strucplot}}, \code{\link[vcd]{struc_mosaic}}, 
  \code{\link[vcd]{struc_assoc}}
}
\examples{

## Set up variables
skill3 <- c("High","Medium","Low") 
correctL <- c("Correct","Incorrect") 

## Simple binary model
cptComp <- calcDPCFrame(list(S1=skill3,S2=skill3),correctL,
                          log(c(S1=1,S2=.75)),1.0,rule="Compensatory",
                          link="partialCredit")

dataComp <- rpCPF(25,cptComp)

cptConj <- calcDPCFrame(list(S1=skill3,S2=skill3),correctL,
                          log(c(S1=1,S2=.75)),1.0,rule="Conjunctive",
                          link="partialCredit")

dataConj <- rpCPF(25,cptConj)

mosaic2(dataComp,cptComp, main="Compensatory model & data")
assoc2(dataComp,cptComp, main="Compensatory model & data")

mosaic2(dataComp,cptConj, main="Conjunctive model & Compensatory data")
assoc2(dataComp,cptConj, main="Conjunctive model & Compensatory data")

mosaic2(dataConj,cptConj, main="Conjunctive model & data")
assoc2(dataConj,cptConj, main="Conjunctive model & data")

mosaic2(dataConj,cptComp, main="Compensatory model & Conjunctive data")
assoc2(dataConj,cptComp, main="Compensatory model & Conjunctive data")

}
\keyword{ aplot }
\keyword{ category }
\concept{ CPF }
\concept{ residuals }

