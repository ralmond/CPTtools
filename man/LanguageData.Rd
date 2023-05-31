\name{LanguageData}
\alias{Language_modal}
\alias{Language_exp}
\docType{data}
\title{Accuracy and Expected Accuracy Matrixes for Langauge Test.}
\description{

  This is an example used in Chapter 7 of Almond, et. al. (2015), based on a conceptual 
  assessment described in Mislevy (1995).  The assessment has four reporting variables: 
  _Reading_, _Writing_, _Speaking_, and _Listening_, each of which can take on states 
  `Novice`, `Intermediate`, or `Advanced`.
  To estimate the reliability 1000 students were simulated from
  the network (see \code{\link[Peanut]{NetworkTester}} and accuracy, using Maximum A Prior 
  estimates, (`Langauge_modal`) and expected accuracy (`Language_exp`) estimates were calculated. 

}
\details{
  The sample language assessment contains:
  \describe{
    \item{A}{5 \emph{Reading} tasks which only tap the Reading attribute.}
    \item{B}{3 \emph{Reading/Writing} integrated tasks which require a written response to a textual prompt.}
    \item{C}{3 \emph{Reading/Speaking/Listening} Integrated tasks which require a spoken response to an aural stimulus, with textual instructions.}
    \item{D}{5 \emph{Listening} tasks which have both aural stimulus and instructions.}
  }

  Because different numbers of tasks (which have different strengths of evidence) are available, 
  different amounts of information are available about each of the four target variables.
  A number of different measures of reliability can be calculated from the (expected) accuracy matrixes. 

} 
\usage{
data("Language_modal")
data("Language_exp")
}
\format{
  The format for both `Language_modal` and `Language_exp` is:
List of 4
 $ Reading  A 3x3 matrix giving the accuracy of the Reading measure.
 $ Writing  A 3x3 matrix giving the accuracy of the Writing  measure.
 $ Speaking A 3x3 matrix giving the accuracy of the Speaking measure.
 $ Listening  A 3x3 matrix giving the accuracy of the Listening measure.

All cases, the table has been normalized so that all entries sum to 1.
}
\source{

  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and
  Yan, D. (2015) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 7.

}
\references{
  Mislevy, R. J. (1995) Test theory and language learning in assessment. 
  \emph{Language Testing}, \emph{12}, 341--369.
}
\examples{
data(Language_modal)
fcKappa(Language_modal$Reading)
gkLambda(Language_modal$Reading)
}
\keyword{datasets}
