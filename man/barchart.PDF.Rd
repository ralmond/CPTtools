\name{barchart.CPF}
\alias{barchart.CPF}
\title{This function produces a plot of a conditional probability frame.}
\description{

  This is an extension of \code{\link[lattice]{barchart.array}} for
  displaying conditional probability tables.  In particular, it will
  display the output of \code{\link{calcDPCFrame}}.

}
\usage{
barchart.CPF(x, data = NULL, ..., baseCol = "firebrick",
             auto.key=TRUE, par.settings)
}
\arguments{
  \item{x}{A conditional probaiblity frame (see \code{\link{as.CPF}}.}
  \item{data}{Ignore this value, used for compatability with
    \code{\link[lattice]{barchart}}.}
  \item{\dots}{Other arguments passed on to
    \code{\link[lattice]{barchart}}, in particular, other lattice
    graphics arguments.}
  \item{baseCol}{This should be a specification of a color.  The color
    is designed as a gradient starting at the base color and getting
    progressively lighter.  If its value is \code{NULL}, the colors are
    left at the default (pastel palette) and the value of par.settings
    is passed through unchanged.
  }
  \item{auto.key}{This is the \code{auto.key} parameter from
    \code{\link[lattice]{barchart}}, it is overridden to have a default of true.
  }
  \item{par.settings}{This is the \code{par.settings} parameter from
    \code{\link[lattice]{barchart}}.  If \code{baseCol} is not null,
    then a value for \code{superpose.polygon} is added to set the bar colors.}
}
\details{

  The function \code{\link[lattice]{barchart.array}} and the function
  \code{as.CPA} to convert the conditional probability frame to an array
  do 90 percent of the work.

  A few minor touches:
  \itemize{
    \item{The function takes special care of one row [un]conditional
      probability frames.}
    \item{The function overrides the default colors using
      \code{\link{colorspread}} to produce varying intensities of the
      same color.}
    \item{The function adds the dimension names, so that the labels
      indicate which variables they belong to.}
    \item{The function sets the default value of auto.key to \code{TRUE}
      so that a legend for the colors is produced.}
  }

  Note that the color change is brought about internally by modifying
  \code{par.settings}.  To suppress this behavior, set \code{baseCol} to
  null, and the user value for \code{par.settings} will be passed
  through unchanged.

}
\value{
  An object of class trellis that when printed will produce the graph.
}
\author{Russell Almond}
\seealso{
  \code{\link{as.CPA}}, \code{\link{colorspread}},
  \code{\link[trellis]{barchart.array}}, \code{\link{calcDPCFrame}}
}
\examples{

     ## Set up variables
     skill1l <- c("High","Medium","Low") 
     skill2l <- c("High","Medium","Low","LowerYet") 
     correctL <- c("Correct","Incorrect") 
     pcreditL <- c("Full","Partial","None")
     gradeL <- c("A","B","C","D","E") 

     cpfTheta <- calcDPCFrame(list(),skill1l,numeric(),0,rule="Compensatory",
                              link="normalLink",linkScale=.5)

     barchart.CPF(cpfTheta)

     cptPC1 <- calcDPCFrame(list(S1=skill1l,S2=skill2l),pcreditL,
                             lnAlphas=log(1),
                             betas=list(full=c(S1=0,S2=999),partial=c(S2=999,S2=0)),
                             rule="OffsetDisjunctive")
    barchart.CPF(cptPC1,baseCol="slateblue")

}
\keyword{ hplot }

