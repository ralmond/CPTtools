\name{dataTable}
\alias{dataTable}
\title{Constructs a table of counts from a set of discrete observations.}
\description{
  This constructs a table of counts in a special format useful for
  conditional probability tables.  The rows correspond to configurations
  of the parent variables and the columns correspond to possible states
  of the child variables.
}
\usage{
dataTable(data, parents, child, childStates)
}
\arguments{
  \item{data}{A data frame whose columns contain variables corresponding
    to \code{parent} and \code{child}.}
  \item{parents}{A vector of names for the columns in \code{data}
    corresponding to the parent variables.}
  \item{child}{The name of the child variable, should refer to a column
    in \code{data}.}
  \item{childStates}{A character vector giving names of levels for the
    output variables from highest to lowest.}
}
\details{
  Apply the function \code{\link[base]{table}} to generate a table of
  counts for the indicated variables (subsetting the table if
  necessary).  Then reformats this into a matrix whose columns
  correspond to the child variable.
}
\value{
  A matrix whose columns correspond to \code{childStates} and whose rows
  correspond to the possible combinations of \code{parents}.
}
\author{Russell Almond}
\seealso{\code{\link[base]{table}}, \code{\link{calcDSllike}}}
\examples{

  skill1l <- c("High","Medium","Low") 
  skill3l <- c("High","Better","Medium","Worse","Low") 
  correctL <- c("Correct","Incorrect") 

  x <- read.csv(system.file("testFiles", "randomPinned100.csv",
                            package="CPTtools"),
              header=TRUE, as.is=TRUE)
  x[,"Skill1"] <- ordered(x[,"Skill1"],skill1l)
  x[,"Skill3"] <- ordered(x[,"Skill3"],skill3l)
  x[,"Comp.Correct"] <- ordered(x[,"Comp.Correct"],correctL)


  tab <- dataTable(x, c("Skill1","Skill3"),"Comp.Correct",correctL)
  data.frame(expand.grid(list(Skill1=skill1l,Skill3=skill3l)),tab)

}
\keyword{manip}

