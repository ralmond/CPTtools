\name{fcKappa}
\alias{fcKappa}
\alias{gkLambda}
\title{Functions for measuring rater agreement.}
\description{

  The functions take a \dQuote{confusion matrix}, a square matrix were
  the rows and columns represent classifications by two different
  raters, and compute measures of rater agreement.  Cohen's kappa
  (\code{fcKappa}) is corrected for random labeling of ratings.  Goodman
  and Kruskall's lambda (\code{gkLambda}) is corrected for labeling
  every subject at the modal category.
  
}
\usage{
fcKappa(tab, weights = c("None", "Linear", "Quadratic"), W=diag(nrow(tab)))
gkLambda(tab, weights = c("None", "Linear", "Quadratic"), W=diag(nrow(tab)))
}
\arguments{
  \item{tab}{A square matrix whose rows and columns represent rating
    categories from two raters or classifiers and whose cells represent
    observed (or expected) counts.  If one classifier is regarded as
    \dQuote{truth} it should be represented by columns.}
  \item{weights}{A character scalar which should be one of
    \dQuote{None}, \dQuote{Linear}, or \code{Quadratic} which gives the
    weighting to be used if \code{W} is not supplied directly (see
    details).} 
  \item{W}{A square matrix of the same size as tab giving the weights
    (see details).  If missing and \code{weights} are supplied one of
    the standard weights are used.}
}
\details{

  Lets say the goal is to classify a number of subjects into \eqn{K}
  categories, and that two raters:  Rater 1, and Rater 2 do the
  classification.  (These could be human raters or machine
  classification algorithms.  In particular a Bayes net modal
  predication is a classier.)  Let \eqn{p_{ij}} be the probability that
  Rater 1 places a subject into Category \eqn{i} and Rater 2 places the
  same subject into Cateogry \eqn{j}.  The \eqn{K\times K} matrix,
  \code{tab}, is the \emph{confusion matrix.}

  Note that \code{tab} could be a matrix of probabilities or a matrix of
  counts, which can be easily turned into a matrix of probabilities by
  dividing by the total.  In the case of a Bayes net, expected counts
  could be used instead.  For example, if Rater 1 was a Bayes net and
  the predicted probabilities for the three categories was
  \eqn{(.5,.3,.2)}, and and Rater 2 was the true category which for this
  subject was 1, then that subject would contribute .5 to \eqn{p_{1,1}},
  .3 to \eqn{p_{2,1}}, and .2 to \eqn{p_{3,1}}.

  In both cases, \eqn{\sum{p_{kk}}}, is a measure of agreement between
  the two raters.  If scaled as probabilities, the highest possible
  agreement is \eqn{+1} and the lowest \eqn{0}.

  However, raw agreement has a problem as a measure of the quality of a
  rating, it depends on the distributions of the categories in the
  population of interest.  In particular, if a majority of the subject
  are of one category, then it is very easy to match just by labeling
  everything as the most frequent category.

  The most well-known correct is the Fliess-Cohen kappa
  (\code{fcKappa}).  This adjusts the agreement rate for the probability
  that the raters will agree by chance.  Let \eqn{p_{i+}} be the row
  sums, and Let \eqn{p_{+j}} be the column sums.  The probability of a
  chance agreement, is then \eqn{\sum p_{k+}p_{+k}}.  So the adjusted
  agreement is:

  \deqn{\kappa = \frac{\sum p_{kk} - \sum p_{k+}p_{+k}}{1 - \sum
      p_{k+}p_{+k}} .}

  So kappa answers the question how much better do the raters do than
  chance agreement.

  Goodman and Kruskal (1952) offered another way of normalizing.  In
  this case, let Rater 1 be the classificaiton method and Rater 2 be the
  truth.  Now look at a classifier which always classifies somebody in
  Category \eqn{k}; that classifier will be right with probability
  \eqn{p_{+k}}.  The best such classifer will be \eqn{\max p_{+k}}.  So
  the adjusted agreement becomes:

  \deqn{\lambda = \frac{\sum p_{kk} - \max p_{+k}}{1 - \max
      p_{+k}} .}

  Goodman and Kruskal's lambda (\code{gkLambda}) is appropriate when
  there is a different treatment associated with each category.  In this
  case, lambda describes how much better one could do than treating
  every subject as if they were in the modal category.

  Weights are used if the misclassification costs are not equal in all
  cases.  If the misclassification cost is \eqn{c_{ij}}, then the weight
  is defined as \eqn{w_{ij} = 1 - c_{ij}/\max c_{ij}}.  Weighted
  agreeement is defined as \eqn{\sum\sum w_{ij}p_{ij}}.  

  If the categories are ordered, there are three fairly standard
  weighting schemes (especially for kappa).
  
  \describe{
    \item{None}{\eqn{w_{ij} = 1} if \eqn{i=j}, 0 otherwise. (Diagonal
      matrix.)}
    \item{Linear}{\eqn{w_{ij} = 1 - |i-j|/(K-1)}.  Penalty increases
      with number of categories of difference.}
    \item{Quadratic}{\eqn{w_{ij} = 1 - (i-j)^2/(K-1)^2}.  Penalty increases
      with square of number of categories of difference.}
  }
  Indeed, quadratic weighted kappa is something of a standard in
  comparing two human raters or a single machine classification
  algorithm to a human rater.

  The argument \code{weights} can be used to select one of these three
  weighting schemes.  Alternatively, the weight matrix \code{W} can be
  specified directly.
  
}
\value{

  A real number between -1 and 1; with higher numbers indicating more
  agreement. 
}
\references{

  Almond, R.G., Mislevy, R.J. Steinberg, L.S., Yan, D. and Willamson, D.
  M. (2015). \emph{{Bayesian} Networks in Educational Assessment}.
  Springer.  Chapter 7.

  Fleiss, J. L., Levin, B. and Paik, M. C. (2003). 
  \emph{Statistical Methods for Rates and Proportions}.
  Wiley.  Chapter 18.

  {Goodman, Leo A., Kruskal, William H.} (1954).
  {Measures of Association for Cross Classifications}.
  \emph{Journal of the American Statistical Association}.
  \bold{49} (268), 732--764.
}
\author{Russell Almond}
\seealso{
  \code{\link[base]{table}}
}
\examples{

## Example from Almond et al. (2015).
read <- matrix(c(0.207,0.029,0,0.04,0.445,0.025,0,0.025,0.229),3,3,
        dimnames=list(estimated=c("Advanced","Intermediate","Novice"),
                      actual=c("Advanced","Intermediate","Novice")))

stopifnot (abs(fcKappa(read)-.8088) <.001)
stopifnot (abs(gkLambda(read)-.762475) <.001)

fcKappa(read,"Linear")
fcKappa(read,"Quadratic")
gkLambda(read,"Linear")
gkLambda(read,"Quadratic")


}
\keyword{ classif}

