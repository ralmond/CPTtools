\name{CPTtools-package}
\alias{CPTtools-package}
\alias{CPTtools}
\docType{package}
\title{
\packageTitle{CPTtools}
}
\description{
\packageDescription{CPTtools}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{CPTtools}

CPTtools is a collection of various bits of R code useful for processing
Bayes net output.  Some were designed to work with ETS's proprietary
StatShop code, and some with RNetica.  The code collected in this
package is all free from explicit dependencies on the specific Bayes net
package and will hopefully be useful with other systems as well.

The majority of the code are related to building conditional probability
tables (CPTs) for Bayesian networks.  The package has two output
representations for a CPT.  The first is a \code{data.frame} object
where the first several columns are factor variables corresponding the
the parent variables, and the remaining columns are numeric variables
corresponding to the state of the child variables.  The rows represent
possible configurations of the parent variables.  An example is shown below.

\preformatted{
       S1       S2       Full    Partial       None
1    High     High 0.81940043 0.15821522 0.02238436
2  Medium     High 0.46696668 0.46696668 0.06606664
3     Low     High 0.14468106 0.74930671 0.10601223
4    High   Medium 0.76603829 0.14791170 0.08605000
5  Medium   Medium 0.38733177 0.38733177 0.22533647
6     Low   Medium 0.10879020 0.56342707 0.32778273
7    High      Low 0.65574465 0.12661548 0.21763987
8  Medium      Low 0.26889642 0.26889642 0.46220715
9     Low      Low 0.06630741 0.34340770 0.59028489
10   High LowerYet 0.39095414 0.07548799 0.53355787
11 Medium LowerYet 0.11027649 0.11027649 0.77944702
12    Low LowerYet 0.02337270 0.12104775 0.85557955
}

The second representation is a table (\code{matrix}) with just the
numeric part.  Two approaches to building these tables from parameters
are described below.  The more flexible discrete partial credit model is
used for the basis of the parameterized networks in the
\code{\link[Peanut:Peanut-package]{Peanut}} package.

In addition to the code for building partial credit networks, this
package contains some code for building Bayesian network structures from
(inverse) correlation matrixes, and graphical displays for Bayes net
output.  The latter includes some diagnostic plots and additional
diagnostic tests.

}
\section{Discrete Partial Credit Framework}{

The original parameterization for creating conditional probability
tables based on Almond et al (2001) proved to be insufficiently
flexible.  Almond (2015) describes a newer parameterization based on
three steps:
\enumerate{
  \item{Translate the parent variables onto a numeric effective theta
    scale (\code{\link{effectiveThetas}}).}
  \item{Combine the parent effective thetas into a single effective
    theta using a combination rule (\code{\link{Compensatory}},
    \code{\link{OffsetConjunctive}}).}
  \item{Convert the effective theta for each row of the table into
    conditional probabilities using a link function
    (\code{\link{gradedResponse}}, \code{\link{partialCredit}}).}
}

The \code{\link{partialCredit}} link function is particularly flexible
as it allows different parameterizations and different combination rules
for each state of the child variable.  This functionality is best
captured by the two high level functions:
\describe{
  \item{\code{\link{calcDPCTable}}}{Creates the probability table for
    the discrete partial credit model given the parameters.}
  \item{\code{\link{mapDPC}}}{Finds an MAP estimate for the parameters
    given an observed table of counts.}
}

This parameterization serves as basis for the model used in the
\code{\link[Peanut:Peanut-package]{Peanut}} package.
  
}
\section{Other parametric CPT models}{

  The first two steps of the discrete partial credit framework outlined
  above are due to a suggestion by Lou DiBello (Almond et al, 2001).
  This lead to an older framework, in which the link function was hard
  coded into the conditional probability table formation.  The models
  were called DiBello-\emph{XX}, where \emph{XX} is the name of the link
  function.  Almond et al. (2015) describes several additional
  examples. 

  \describe{
    \item{\code{\link{calcDDTable}}}{Calculates DiBello-Dirichlet model
      probability and parameter tables.}
    \item{\code{\link{calcDNTable}}}{Creates the probability table for
      DiBello-Normal distribution.} 
    \item{\code{\link{calcDSTable}}}{Creates the probability table for
      DiBello-Samejima distribution.} 
    \item{\code{\link{calcDSllike}}}{Calculates the log-likelihood for
      data from a DiBello-Samejima (Normal) distribution.}
  }

  Diez (1993) and Srinivas (1993) describe an older parametric framework
  for Bayes nets based on the noisy-or or noisy-max function.  These are
  also available.
  \describe{
    \item{\code{\link{calcNoisyAndTable}}}{Calculate the conditional
      probability table for a Noisy-And or Noisy-Min distribution.}
    \item{\code{\link{calcNoisyOrTable}}}{Calculate the conditional
      probability table for a Noisy-Or distribution.}
  }
  
}
\section{Building Bayes nets from (inverse) correlation matrixes}{

  Almond (2010) noted that in many cases the best information about the
  relationship among variables came from a procedure that produces a
  correlation matrix (e.g., a factor analysis).  Applying a trick from
  Whittaker (1990), connecting pairs of nodes corresponding to nonzero
  entries in an inverse correlation matrix produces an undirected
  graphical model.  Ordering in the nodes in a perfect ordering allows
  the undirected model to be converted into a directed model (Bayesian
  network).  The conditional probability tables can then be created
  through a series of regressions.

  The following functions implement this protocol:
  \describe{
    \item{\code{\link{structMatrix}}}{Finds graphical structure from a
      covariance matrix.}
    \item{\code{\link{mcSearch}}}{Orders variables using Maximum
      Cardinality search.}
    \item{\code{\link{buildParentList}}}{Builds a list of parents of
      nodes in a graph.}
    \item{\code{\link{buildRegressions}}}{Creates a series of regressions
      from a covariance matrix.}
    \item{\code{\link{buildRegressionTables}}}{Builds conditional
      probability tables from regressions.}
  }
}
\section{Other model construction tools}{

  These functions are a grab bag of lower level utilities useful for
  building CPTs:
  \describe{
    \item{\code{\link{areaProbs}}}{Translates between normal and
      categorical probabilities.}
    \item{\code{\link{numericPart}}}{Splits a mixed data frame into a
      numeric matrix and a factor part..}
    \item{\code{\link{dataTable}}}{Constructs a table of counts from a
      setof discrete observations..}
    \item{\code{\link{eThetaFrame}}}{Constructs a data frame showing the
      effective thetas for each parent combination..}
    \item{\code{\link{effectiveThetas}}}{Assigns effective theta levels
      for categorical variable.}
    \item{\code{\link{getTableStates}}}{Gets meta data about a
      conditional probability table..}
    \item{\code{\link{rescaleTable}}}{Rescales the numeric part of the
      table.}
    \item{\code{\link{scaleMatrix}}}{Scales a matrix to have a unit
      diagonal.}
    \item{\code{\link{scaleTable}}}{Scales a table according to the Sum
      and Scale column.}
  }
}
\section{Bayes net output displays and tests}{

  Almond et al. (2009) suggested using hanging barplots for displaying
  Bayes net output and gives several examples.  The function
  \code{\link{stackedBars}} produces the simple version of this plot and
  the function \code{\link{compareBars}} compares two distributions
  (e.g., prior and posterior).  The function
  \code{\link{buildFactorTab}} is useful for building the data and the
  function \code{\link{colorspread}} is useful for building color
  gradients.
  
  Madigan, Mosurski and Almond (1997) describe a graphical weight of
  evidence balance sheet (see also Almond et al, 2015, Chapter 7; Almond
  et al, 2013).  The function \code{\link{woeHist}} calculates the weights of
  evidence for a series of observations and the function
  \code{\link{woeBal}} produces a graphical display.  

  Sinharay and Almond (2006) propose a graphical fit test for
  conditional probability tables (see also, Almond et al, 2015, Chapter
  10).  The function \code{\link{OCP}} implements this test, and the
  function \code{\link{betaci}} creates the beta credibility intervals
  around which the function is built.

  The key to Bayesian network models are the assumptions of conditional
  independence which underlie the model.  The function
  \code{\link{localDepTest}} tests these assumptions based on observed
  (or imputed) data tables.

  The function \code{\link{mutualInformation}} calculates the mutual
  information of a two-way table, a measure of the strength of
  association.  This is similar to the measure used in many Bayes net
  packages (e.g., \code{\link[RNetica]{MutualInfo}}).
                        
}
\section{Data sets}{

  Two data sets are provided with this package:
  \describe{
    \item{\code{\link{ACED}}}{Data from ACED field trial (Shute, Hansen,
      and Almond, 2008).  This example is based on a field trial of a
      Bayesian network based Assessment for Learning system, and contains
      both item-level response and high-level network summaries.  A
      complete description of the Bayes net can be found at
      \url{http://ecd.ralmond.net/ecdwiki/ACED/ACED}.}
    \item{\code{\link{MathGrades}}}{Grades on 5 mathematics tests from
      Mardia, Kent and Bibby (from Whittaker, 1990).}
    }
}
\section{Index}{
Complete index of all functions.
  
\packageIndices{CPTtools}

}
\author{
\packageAuthor{CPTtools}

Maintainer: \packageMaintainer{CPTtools}
}
\references{

    Almond, R.G. (2015).  An IRT-based Parameterization for Conditional
  Probability Tables. Paper submitted to the 2015 Bayesian Application
  Workshop at the Uncertainty in Artificial Intelligence conference.

  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and
  Yan, D. (2015) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  

  Almond, R. G. (2010). \sQuote{I can name that Bayesian network in two
  matrixes.} \emph{International Journal of Approximate Reasoning.}
  \bold{51}, 167-178.

  Almond, R. G.,  Shute, V. J., Underwood, J. S., and Zapata-Rivera,
  J.-D (2009). Bayesian Networks: A Teacher's View. \emph{International
  Journal of Approximate Reasoning.} \bold{50}, 450-460.

  Almond, R.G., DiBello, L., Jenkins, F., Mislevy, R.J.,  
  Senturk, D., Steinberg, L.S. and Yan, D. (2001) Models for Conditional
  Probability Tables in Educational Assessment.  \emph{Artificial
    Intelligence and Statistics 2001}  Jaakkola and Richardson (eds).,
  Morgan Kaufmann, 137--143.  

  Diez, F. J. (1993) Parameter adjustment in Bayes networks.  The
  generalized noisy OR-gate.  In Heckerman and Mamdani (eds)
  \emph{Uncertainty in Artificial Intelligence 93.}  Morgan Kaufmann.
  99--105. 

  Muraki, E. (1992).  A Generalized Partial Credit Model:  Application
  of an EM Algorithm.  \emph{Applied Psychological Measurement}, \bold{16},
  159-176.  DOI: 10.1177/014662169201600206

  Samejima, F. (1969) Estimation of latent ability using a
  response pattern of graded scores.  \emph{Psychometrika Monograph No.
  17}, \bold{34}, (No. 4, Part 2).

  Shute, V. J., Hansen, E. G., & Almond, R. G. (2008). You can't fatten
  a hog by weighing it---Or can you? Evaluating an assessment for learning
  system called ACED. \emph{International Journal of Artificial
  Intelligence and Education}, \bold{18}(4), 289-316. 

  Sinharay, S. and Almond, R.G. (2006).  Assessing Fit of Cognitively
  Diagnostic Models:  A case study.  \emph{Educational and Psychological
    Measurement}.  \bold{67}(2), 239--257.

  Srinivas, S. (1993) A generalization of the Noisy-Or model, the
  generalized noisy OR-gate.  In Heckerman and Mamdani (eds)
  \emph{Uncertainty in Artificial Intelligence 93.}  Morgan Kaufmann.
  208--215. 

  Whittaker, J. (1990).  \emph{Graphical Models in Applied Multivariate
    Statistics}.  Wiley.

  Madigan, D.,  Mosurski, K. and Almond, R. (1997) Graphical explanation
  in belief networks.  \emph{Journal of Computational Graphics and
    Statistics}, \bold{6}, 160-181.

  Almond, R. G., Kim, Y. J., Shute, V. J. and  Ventura, M. (2013).
  Debugging the Evidence Chain.  In Almond, R. G. and  Mengshoel,
  O. (Eds.)  \emph{Proceedings of the 2013 UAI Application Workshops:
  Big Data meet Complex Models and Models for Spatial, Temporal and
  Network Data (UAI2013AW)}, 1-10.
  \url{http://ceur-ws.org/Vol-1024/paper-01.pdf} 

  
}
\keyword{ package }
\seealso{
\code{\link[RNetica]{RNetica}} ~~
\code{\link[Peanut:Peanut-package]{Peanut}} ~~
}
\examples{
## Set up variables
skill1l <- c("High","Medium","Low") 
skill2l <- c("High","Medium","Low","LowerYet") 
correctL <- c("Correct","Incorrect") 
pcreditL <- c("Full","Partial","None")
gradeL <- c("A","B","C","D","E") 

## New Discrete Partial Credit framework:

## Complex model, different rules for different levels
cptPC2 <- calcDPCFrame(list(S1=skill1l,S2=skill2l),pcreditL,
                          list(full=log(1),partial=log(c(S1=1,S2=.75))),
                          betas=list(full=c(0,999),partial=1.0),
                          rule=list("OffsetDisjunctive","Compensatory"))

## Graded Response using the older DiBello-Samejima framework.
cptGraded <- calcDSTable(list(S1=skill1l),gradeL, 0.0, 0.0, dinc=c(.3,.4,.3))

## Building a Bayes net from a correlation matrix.
data(MathGrades)
pl <- buildParentList(structMatrix(MathGrades$var),"Algebra")
rt <- buildRegressions(MathGrades$var,MathGrades$means,pl)
tabs <- buildRegressionTables(rt, MathGrades$pvecs, MathGrades$means,
                              sqrt(diag(MathGrades$var)))

## Stacked Barplots:
margins.prior <- data.frame (
 Trouble=c(Novice=.19,Semester1=.24,Semester2=.28,Semseter3=.20,Semester4=.09),
 NDK=c(Novice=.01,Semester1=.09,Semester2=.35,Semseter3=.41,Semester4=.14),
 Model=c(Novice=.19,Semester1=.28,Semester2=.31,Semseter3=.18,Semester4=.04)
)

margins.post <- data.frame(
 Trouble=c(Novice=.03,Semester1=.15,Semester2=.39,Semseter3=.32,Semester4=.11),
 NDK=c(Novice=.00,Semester1=.03,Semester2=.28,Semseter3=.52,Semester4=.17),
 Model=c(Novice=.10,Semester1=.25,Semester2=.37,Semseter3=.23,Semester4=.05))

stackedBars(margins.post,3,
            main="Marginal Distributions for NetPASS skills",
            sub="Baseline at 3rd Semester level.",
            cex.names=.75, col=hsv(223/360,.2,0.10*(5:1)+.5))

compareBars(margins.prior,margins.post,3,c("Prior","Post"),
            main="Margins before/after Medium Trouble Shooting Task",
            sub="Observables:  cfgCor=Medium, logCor=High, logEff=Medium",
            legend.loc = "topright",
            cex.names=.75, col1=hsv(h=.1,s=.2*1:5-.1,alpha=1),
            col2=hsv(h=.6,s=.2*1:5-.1,alpha=1))

## Weight of evidence balance sheets
sampleSequence <- read.csv(paste(library(help="CPTtools")$path,
                                "testFiles","SampleStudent.csv",
                                sep=.Platform$file.sep),
                           header=TRUE,row.names=1)

woeBal(sampleSequence[,c("H","M","L")],c("H"),c("M","L"),lcex=1.25)


### Observable Characteristic Plot
pi <- c("+"=.15,"-"=.85)
nnn <- c("(0,0,0)"=20,"(0,0,1)"=10,
         "(0,1,0)"=10,"(0,1,0)"=5,
         "(1,0,0)"=10,"(1,0,1)"=10,
         "(1,1,1)"=10,"(1,1,1)"=25)
xx1 <- c("(0,0,0)"=2,"(0,0,1)"=5,
       "(0,1,0)"=1,"(0,1,1)"=3,
       "(1,0,0)"=0,"(1,0,1)"=2,
       "(1,1,0)"=5,"(1,1,1)"=24)
grouplabs <- c(rep("-",3),"+")
grouplabs1 <- rep(grouplabs,each=2)
OCP2 (xx1,nnn,grouplabs1,pi,c("-","+"),ylim=c(0,1), reflty=c(2,4),
      setlabs=c("Low Skill3","High Skill3"),setat=-.8,
      main="Data for which Skill 3 is relevant")


}
