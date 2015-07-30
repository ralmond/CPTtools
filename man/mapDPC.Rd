\name{mapDPC}
\alias{mapDPC}
\title{Finds an MAP estimate for a discrete partial credit CPT
}
\description{
  This finds a set of parameters for a given discrete partial credit
  model which produces the best fit to the first argument.  It is
  assumed that the first argument is a table produced by adding observed
  counts to a prior conditional probability table.  Thus the result is
  a maximum a posterior (MAP) estimate of the CPT.  The parametric
  structure of the table is given by the \code{rules} and \code{link}
  parameters. 
}
\usage{
mapDPC(postTable, skillLevels, obsLevels, lnAlphas, betas,
       rules = "Compensatory", link = "partialCredit",
       linkScale=NULL, Q=TRUE, tvals=lapply(skillLevels,
               function (sl) effectiveThetas(length(sl))),
       ...) 
}
\arguments{
  \item{postTable}{
    A table of cell counts which should have the same structure as the
    output of \code{\link{calcDPCTable}(skillLevels, obsLevels,
      lnAlphas, betas, rules, link, linkScale)}.  As zero counts would
    cause problems, the prior conditional probability table is
    normally added to the counts to make the argument a posterior
    counts. 
  }

  \item{skillLevels}{A list of character vectors giving names of levels
    for each of the condition variables.}

  \item{obsLevels}{A character vector giving names of levels for the
    output variables from highest to lowest.  As a special case, can
    also be a vector of integers.}

  \item{lnAlphas}{A list of vectors of initial values for the log slope
    parameters.  Its length should be 1 or \code{length(obsLevels)-1}.
    The required length of the individual component vectors depends on
    the choice of \code{rule} (and is usually either 1 or the length of
    \code{skillLevels}).}

  \item{betas}{A list of vectors of initial values for the difficulty
    (-intercept) parameters.  Its length should be 1 or
    \code{length(obsLevels)-1}.  The required length of the individual
    component vectors depends on the choice of \code{rule} (and is
    usually either 1 or the length of \code{skillLevels}).}

  \item{rules}{A list of functions for computing effective theta (see
    Details). Its length should be \code{length(obsLevels)-1} or 1
    (implying that the same rule is applied for every gap.)}

  \item{link}{The function that converts a table of effective thetas to
    probabilities}

  \item{linkScale}{Initial values for the optional scale parameter for
    the \code{link} function.  This is only used with certain choices of
    \code{link} function.}  

  \item{Q}{This should be a Q matrix indicating which parent variables
    are relevant for which state transitions.  It should be a number of
    states minus one by number of parents logical matrix.  As a special
    case, if all variable are used for all levels, then it can be a
    scalar value.}

  \item{tvals}{A list of the same length as \code{skillLevels}. Each
    element should be a numeric vector values on the theta (logistic)
    scale corresponding to the levels for that parent variable.  The
    default spaces them equally according to the normal distribution
    (see \code{\link{effectiveThetas}}).}

  \item{\dots}{
    Additional arguments passed to the \link[stats]{optim} function.
}
}
\details{

  The purpose of this function is to try to estimate the values of a
  discrete partial credit model.  The structure of the model is given by
  the \code{rules} and \code{link} arguments:  the form of the table
  produces is like the output of \code{\link{calcDPCTable}(skillLevels,
  obsLevels, lnAlphas, betas, rules, link, linkScale)}.  It tries to
  find the values of \code{lnAlphas} and \code{betas} (and if used
  \code{linkScale}) parameters which are most likely to have generated
  the data in the \code{postTable} argument.  The \code{lnAlphas},
  \code{betas} and \code{linkScale} arguments provide the initial values
  for those parameters.

  Let \eqn{p_{i,j}} be the value in the \eqn{i}th row and the \eqn{j}th
  column of the conditional probability table output from
  \code{\link{calcDPCTable}(skillLevels, obsLevels, lnAlphas, betas,
  rules, link, linkScale)}, and let \eqn{x_{i,j}} be the corresponding
  elements of \code{postTable}.  The \code{mapDPC} function uses
  \code{\link[stats]{optim}} to find the value of the parameters that
  minimizes the deviance, \deqn{-2*\sum_i \sum_j x_{i,j} \log(p_{i,j}).}  


}
\value{
  A list with components:
  \describe{
    \item{lnAlphas}{A vector of the same structure as \code{lnAlphas}
      containing the estimated values.}
    \item{betas}{A veto of the same structure as \code{betas}
      containing the estimated values.}
    \item{linkScale}{If the \code{linkScale} was supplied, the estimated
      value.}
    \item{convergence}{An integer code. \code{0} indicates successful
      completion, positive values are various error codes (see
      \code{\link[stats]{optim}}).}
    \item{value}{The deviance of the fit DPC model.}
  }
  The list is the output of the \code{\link[stats]{optim}}) function,
  which has other components in the output.  See the documentation of
  that function for details.
}
\references{
  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and
  Yan, D. (Draft) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 8.

  Muraki, E. (1992).  A Generalized Partial Credit Model:  Application
  of an EM Algorithm.  \emph{Applied Psychological Measurement}, \bold{16},
  159-176.  DOI: 10.1177/014662169201600206

Samejima, F. (1969) Estimation of latent ability using a
response pattern of graded scores.  \emph{Psychometrika Monograph No.
17}, \bold{34}, (No. 4, Part 2).

  I also have planned a manuscript that describes these functions in
  more detail.
}
\author{Russell Almond}
\seealso{
  \code{\link[stats]{optim}}), \code{\link{calcDPCTable}},\code{\link{Compensatory}},
  \code{\link{OffsetConjunctive}},\code{\link{gradedResponse}},
  \code{\link{partialCredit}} 
}
\examples{

  pLevels <- list(Skill1=c("High","Med","Low"))
  obsLevels <- c("Full","Partial","None")

  trueLnAlphas <- list(log(1),log(.25))
  trueBetas <- list(2, -.5)

  priorLnAlphas <- list(log(.5),log(.5))
  priorBetas <- list(1, -1)

  truedist <- calcDPCTable(pLevels,obsLevels,trueLnAlphas,trueBetas,
                           rules="Compensatory",link="partialCredit")
  prior <- calcDPCTable(pLevels,obsLevels,priorLnAlphas,priorBetas,
                           rules="Compensatory",link="partialCredit")

  post1 <- prior + round(truedist*1000)

  map1 <- mapDPC(post1,pLevels,obsLevels,priorLnAlphas,priorBetas,
                           rules="Compensatory",link="partialCredit")

  if (map1$convergence != 0) {
    warning("Optimization did not converge:", map1$message)
  }

  postLnAlphas <- map1$lnAlphas
  postBetas <- map1$betas
  fitdist <- calcDPCTable(pLevels,obsLevels,map1$lnAlphas,map1$betas,
                           rules="Compensatory",link="partialCredit")
  ## Tolerance for recovery test.
  tol <- .01
  maxdistdif <- max(abs(fitdist-truedist))
  if (maxdistdif > tol) {
    stop("Posterior and True CPT differ, maximum difference ",maxdistdif)
  }
  if (any(abs(unlist(postLnAlphas)-unlist(trueLnAlphas))>tol)) {
    stop("Log(alphas) differ by more than tolerance")
  }
  if (any(abs(unlist(postBetas)-unlist(trueBetas))>tol)) {
   stop("Betas differ by more than tolerance")
  }

}
\keyword{ distribution }

