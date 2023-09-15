\name{woeHist}
\alias{woeHist}
\title{Creates weights of evidence from a history matrix.}
\description{
   Takes a matrix providing the probability distribution for the target
   variable at several time points and returns a weight of evidence for
   all time points except the first.
   
}
\usage{
woeHist(hist, pos=1L, neg=NULL)
}
\arguments{
  \item{hist}{A matrix whose rows represent time points (after tests)
    and columns represent probabilities.}
  \item{pos}{An expression for selecting rows of the \code{cpf} 
    which corresponds to the hypothesis.}
  \item{neg}{An expression for selecting the rows corresponding to
    the complement of the hypothesis. (The default value is 
    \code{-pos} if \code{pos} is numeric; \code{!pos} if \code{pos}
    is logical, and \code{setdiff(rownames(cpf),pos} if \code{pos} 
    is a character vector.}
}
\details{
  Good (1971) defines the \dfn{Weight Of Evidence (WOE)} as:
  \deqn{ 100 \log_{10} \frac{\Pr(E|H)}{\Pr(E|\overline H)} =
    100 \left [\log_{10} \frac{\Pr(H|E)}{\Pr(\overline H|E)}
    - \log_{10} \frac{\Pr(H)}{\Pr(\overline H)} \right ]}{%
    100 log10 Pr(E|H)/Pr(E| not H) =
    100 [ log10 Pr(H|E)/Pr(not H|E) - log10 Pr(H)/Pr(not H)}
  Where \eqn{\overline H}{not H} is used to indicate the negation of the
  hypothesis.  Good recommends taking the log base 10 and multiplying by
  100, and calls the resulting units \dfn{centibans}.   The second
  definition of weight of evidence as a difference in log odd leads
  naturally to the idea of an incremental weight of evidence for each
  new observation. 

  Following Madigan, Mosurski and Almond (1997), all that is needed to
  calculate the WOE is the marginal distribution for the hypothesis
  variable at each time point.  They also note that the definition is
  somewhat problematic if the hypothesis variable is not binary.  In
  that case, they recommend partitioning the states into a
  \emph{positive} and \emph{negative} set.  The \code{pos} and \code{neg}
  are meant to describe that partition.  They can be any expression
  suitable for selecting columns from the \code{hist} matrix.
}
\value{
  A vector of weights of evidence of length one less than the number of
  rows of \code{hist} (i.e., the result of applying \code{diff()} to the
  vector of log odds.)
}
\references{
  Good, I. (1971) The probabilistic explication of information,
  evidence, surprise, causality, explanation and utility.
  In \emph{Proceedings of a Symposium on the Foundations of Statistical
    Inference}.  Holt, Rinehart and Winston, 108-141.

  Madigan, D.,  Mosurski, K. and Almond, R. (1997) Graphical explanation
  in belief networks.  \emph{Journal of Computational Graphics and
    Statistics}, \bold{6}, 160-181.
  }
\author{Russell Almond}
\seealso{\code{\link{readHistory}}, \code{\link{woeBal}},
  \code{\link[base]{diff}} }
\examples{
  testFiles <- system.file("testFiles",package="CPTtools")
  allcorrect <- readHistory(read.csv(file.path(testFiles,
     "CorrectSequence.csv"),as.is=TRUE),
     probcol="Margin.sequences.")
  woeHist(allcorrect,"High",c("Medium","Low"))
  woeHist(allcorrect,1:2,3)
}
\keyword{math}% at least one, from doc/KEYWORDS

