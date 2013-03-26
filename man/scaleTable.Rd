\name{scaleTable}
\alias{scaleTable}
\title{Scales a table according to the Sum and Scale column.}
\description{
  Takes a matrix or vector with a \code{Sum} and \code{Scale} column and
  rescales it by multiplying each remaining element by the value of
  \eqn{Scale/Sum} for that row.

  If the last two rows are not named \code{Sum} and \code{Scale} then it
  simply returns its argument.
}
\usage{
scaleTable(table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{A matrix or vector in which the last two columns are
    named "Scale" and "Sum".}
}
\details{
  The parameters of a Dirichlet distribution can be stored in two ways,
  one is to have each cell in the table represent a pseudo count.  The
  other was is to have each row represent a probability vector and use
  an additional pseudo sample size (the \code{Scale} column).  If the
  probability vector is reported in a some other metric (say as a
  percentage or as a fraction of some smaller sample) the the \code{Sum}
  column is used to store the row sum.
}
\value{
  Rescaled table with \code{Sum} and \code{Scale} columns removed.  This
  makes some attempt to preserve the type of the \code{table} argument
  as a matrix, row vector or numeric object.
}
\references{
  \url{http://research.ets.org/~ralmond/StatShop/dataFormats.html}
}
\note{
  Used by the function \code{compareDS} to compare tables which may be
  in different formats.
}
\author{Russell Almond}
\examples{
  c1 <- matrix(c(70,20,10,10,20,70),nrow=2,byrow=TRUE,
               dimnames=list(NULL,c("H","M","L")))
  s1 <- matrix(c(7,2,1,10,100,1,2,7,10,100),nrow=2,byrow=TRUE,
               dimnames=list(NULL,c("H","M","L","Sum","Scale")))

  ## 1 row matrixes need special handling (c1[1,] is a vector not a matrix)
  c1r1 <- matrix(c1[1,],nrow=1,dimnames=list(NULL,c("H","M","L")))
  s1r1 <- matrix(s1[1,],nrow=1,dimnames=list(NULL,c("H","M","L","Sum","Scale")))

  stopifnot(
    identical(c1,scaleTable(s1)),
    identical(c1[1,],scaleTable(s1[1,])),
    identical(c1r1,scaleTable(s1r1))
  )

  # This should have no effect when run on matrixes without the Sum and
  # Scale column.
  stopifnot(
    identical(c1,scaleTable(c1)),
    identical(c1[1,],scaleTable(c1[1,])),
    identical(c1r1,scaleTable(c1r1))
  )

}
\keyword{category}% at least one, from doc/KEYWORDS

