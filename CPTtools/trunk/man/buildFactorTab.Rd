\name{buildFactorTab}
\alias{buildFactorTab}
\alias{build2FactorTab}
\alias{buildMarginTab}
\alias{marginTab}
\title{Builds probability tables from Scored Bayes net output.}
\description{
  Looks for margin statistics in scored Bayes net output, and puts them
  into tables with rows representing variables and columns
  representing variable states.

  The \code{marginTab} function does this for a single individual.  The
  \code{buildMarginTab} uses the grand mean across all individuals and
  the \code{buildFactorTab} breaks down groups according to a factor
  variable.  The function \code{build2FactorTab} builds a three-way table.
}
\usage{
buildFactorTab(data, fact, stateNames, skillNames, reverse = TRUE,
               stem="margin", sep=".")
build2FactorTab(data, fact1, fact2, stateNames, skillNames,
                reverse = TRUE, stem="margin",sep=".")
buildMarginTab(data, stateNames, skillNames, reverse = TRUE,
               stem="margin",sep=".")
marginTab(datarow, stateNames, skillNames, reverse = TRUE,
          stem="margin",sep=".")
}
\arguments{
  \item{data}{A data sets of StatShop statistics for many individuals.}
  \item{datarow}{One row of such a data set.}
  \item{fact}{A factor variable according to which to split the
    data. Length should be the same as the length of \code{data}.}
  \item{fact1}{A factor variable according to which to split the data.}
  \item{fact2}{A factor variable according to which to split the data.}
  \item{stateNames}{Names of the variable states.}
  \item{skillNames}{Names of the proficiency variable(s) to be used.}
  \item{reverse}{Reverse the order of the states for display (i.e.,
    convert from StatShop order of highest first to more natural order
    of lowest first.}
  \item{stem}{A character string giving a prefix used to indicate
    variable names.}
  \item{sep}{A character string giving a separator used to separate
    prefix from variable names.}
}
\details{
  This looks for columns marked \dQuote{<stem><sep><skillName>} in the
  data frame, and builds them into a matrix.  It is assumed that all
  variables have the same number of states and that they are in the same
  order, and the order is the same as given in \code{stateNames}.

  The functions \code{buildFactorTab} and \code{build2FactorTab} really
  expect their \code{skillNames} argument to be a single variable name.
  However, they should work with multiple variables if suitable values
  are chosen for the state names.
  
}
\value{
  For \code{marginTab} a matrix with columns corresponding to
  \code{skillNames} and rows corresponding to \code{stateNames} giving
  the probabilities for a single individual.

  For \code{buildMarginTab} a matrix with columns corresponding to
  \code{skillNames} and rows corresponding to \code{stateNames} giving
  the average probabilities for the entire data set.

  For \code{buildFactorTab} a matrix with columns corresponding to
  the unique values of \code{fact} and rows corresponding to
  \code{stateNames} entries give the average probabilities across the
  groups.

  For \code{build2FactorTab} a 3 dimensional array with the first
  dimension corresponding to the unique values of \code{fact1}, the
  second dimension corresponding to the unique values of \code{fact2}
  and the last dimension corresponding to \code{stateNames} entries give
  the average probabilities across the groups.

}
\author{Russell Almond}
\seealso{\code{\link{stackedBars}},\code{\link{compareBars}}}
\examples{

data(ACED)

marginTab(ACED.scores[1,], c("H","M","L"), ACED.skillNames, reverse = TRUE,
          stem="p",sep="")

buildMarginTab(ACED.scores, c("H","M","L"), ACED.skillNames[1:4],
               reverse = TRUE,
               stem="p",sep="")

buildFactorTab(ACED.scores, ACED.scores$Cond, c("H","M","L"), "sgp",
               reverse = TRUE,
               stem="p", sep="")

build2FactorTab(ACED.scores, ACED.scores$Seq, ACED.scores$FB,
                c("H","M","L"), "sgp",
                reverse = TRUE, stem="p",sep="")


}
\keyword{manip}

