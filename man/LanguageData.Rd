\name{LanguageData}
\alias{Language_modal}
\alias{Language_exp}
\alias{language16}
\alias{language24}
\docType{data}
\title{Accuracy and Expected Accuracy Matrixes for Langauge Test.}
\description{

  This is an example used in Chapter 7 of Almond, et. al. (2015), based on a conceptual 
  assessment described in Mislevy (1995).  The assessment has four reporting variables: 
  _Reading_, _Writing_, _Speaking_, and _Listening_, each of which can take on states 
  `Novice`, `Intermediate`, or `Advanced`.
  To estimate the reliability 1000 students were simulated from
  the network (see \code{\link[RNetica=NetworkTester-class]{NetworkTester}} and accuracy, using Maximum A Prior 
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
data("language16")
data("language24")
}
\format{
  The format for both `Language_modal` and `Language_exp` is:
List of 4
 $ Reading  A 3x3 matrix giving the accuracy of the Reading measure.
 $ Writing  A 3x3 matrix giving the accuracy of the Writing  measure.
 $ Speaking A 3x3 matrix giving the accuracy of the Speaking measure.
 $ Listening  A 3x3 matrix giving the accuracy of the Listening measure.

All cases, the table has been normalized so that all entries sum to 1.

The data sets `language16` and `language24` are simulated data (1000 cases
each) from both the 16 task test described above, and a 24 task variant which 
has 6 copies of each task.  Both data frames have the following columns 
(not necessarily in this order).

\describe{
  \item{IDnum}{A case number.}
  \item{Listening,Speaking,Writing,Reading}{The "true" (simulated) ability.}
  \item{TaskD,TaskD1,TaskD2,TaskD3,TaskD4,TaskD5}{The simulated responses for the 
    type D (Listening) tasks.  `TaskD5` is only in `language24`.}
  \item{TaskC,TaskC1,TaskC2,TaskC3,TaskC4,TaskC5}{The simulated reponses for the 
    type C (Speaking) tasks.  `TaskC3`, `TaskC4`, and `TaskC5` are only
    in the `langauge24` data set.}
  \item{TaskB,TaskB1,TaskB2,TaskB3,TaskB4,TaskB5}{The simulated responses for
    the type B (Writing) tasks.  `TaskB3`, `TaskB4`, and `TaskB5` are only
    in the `language24` data set.}
  \item{TaskA,TaskA1,TaskA2,TaskA3,TaskA4,TaskA5}{The simulated responses for
    the type A (Reading) tasks.  `TaskA5` is only available in the 
    `langauge24` data set.}
  \item{Listening.Novice,Listening.Intermediate,Listening.Advanced}{The 
    estimated probability that this subject is in the `Novice`, 
    `Intermediate` and `Advanced` categories respectively of the Listening
    skill.}
  \item{Speaking.Novice,Speaking.Intermediate,Speaking.Advanced}{The 
    estimated probability that this subject is in the `Novice`, 
    `Intermediate` and `Advanced` categories respectively of the Speaking
    skill.}
  \item{Writing.Novice,Writing.Intermediate,Writing.Advanced}{The 
    estimated probability that this subject is in the `Novice`, 
    `Intermediate` and `Advanced` categories respectively of the Writing
    skill.}
  \item{Reading.Novice,Reading.Intermediate,Reading.Advanced}{The 
    estimated probability that this subject is in the `Novice`, 
    `Intermediate` and `Advanced` categories respectively of the Reading
    skill.}
  \item{mode.Listening,mode.Speaking,mode.Writing,mode.Reading}{The
    most likely state of the skill variables given the relevant data.}
}

The simulation code used to build `language16` and `language24` is in 
`vignette("SimulationStudies",package="RNetica")`.

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
