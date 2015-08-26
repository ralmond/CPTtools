\name{ACED.scores}
\alias{ACED}
\alias{ACED.scores}
\alias{ACED.items}
\alias{ACED.prePost}
\alias{ACED.skillNames}
\docType{data}
\title{Data from ACED field trial}
\description{
  ACED (Adaptive Content with Evidence-Based Diagnosis; Shute, Hansen
  and Almond, 2008) is a Bayes net based assessment system which
  featured:  (a) adaptive item selection and (b) extended feedback for
  incorrect items.  This data contains both item level and
  pretest/posttest data from a field trial of the ACED system.
}
\usage{data("ACED")}
\format{
  ACED contains 3 \code{data.frame} objects and one explanatory variable.

  \code{ACED.scores} is data frame with 230 observations on 74
  variables.  These are mostly high-level scores from the Bayesian
  network.  
  \describe{
    \item{\code{Cond_code}}{a numeric vector giving the experimental
      condition for this student, see also \code{Cond}}
    \item{\code{Seq}}{a factor describing whether the sequence of items
      was  \code{Linear} or \code{Adaptive}}
    \item{\code{FB}}{a factor describing whether the feedback for
      incorrect items was \code{Extended} or \code{AccuracyOnly}}
    \item{\code{All_Items}}{a numeric vector giving the number of items
      in ACED}
    \item{\code{Correct}}{a numeric vector giving the number of items
      the student got correct}
    \item{\code{Incorr}}{a numeric vector giving the number of items the
      student got incorrect}
    \item{\code{Remain}}{a numeric vector giving the number of items not
      reached or skipped}
    \item{\code{ElapTime}}{a numeric vector giving the total time spent
      on ACED}
    The next group of columns give \dQuote{scores} for each of the nodes in
    the Bayesian network.  Each node has four scores, and the columns are
    names \code{p}\emph{node}\emph{ScoreType} where \emph{node} is
    replaced by one of the codes in \code{ACED.allSkills}.
    \item{\code{p}\emph{node}\code{H}}{a numeric vector giving the probability
      \emph{node} is in the high state}
    \item{\code{p}\emph{node}\code{M}}{a numeric vector giving the probability
      \emph{node} is in the medium state}
    \item{\code{p}\emph{node}\code{L}}{a numeric vector giving the probability
      \emph{node} is in the low state}
    \item{\code{EAP}\emph{node}}{the expected a posteriori value of \emph{node}
      assuming an equal interval scale, where \code{L=1}, \code{M=2} and
      \code{H=3}} 
    \item{\code{MAP}\emph{node}}{a factor vector giving maximum a
      posteriori value of \emph{node}, i.e.,
      \code{which.max(p}\emph{node}\code{H, p}\emph{node}\code{M, p}\emph{node}\code{L)}.}
After a number of columns with this pattern, the last column is:
    \item{\code{Cond}}{a factor describing the experimental condition
      with levels \code{Adaptive/Accuracy}, \code{Adaptive/Extended} and
      \code{Linear/Extended}} 
  }

  \code{ACED.skillNames} is a character vector giving the abbreviations
  used for the node names.  Here are the interpretations:
\describe{
  \item{\code{ sgp }}{Solve Geometric Problems.  This is the highest
  level variable for the field trial data.  }
  \item{\code{ arg }}{Algebraic Rule Geometric  }
  \item{\code{ cr }}{Find Common Ratio  }
  \item{\code{ dt }}{Distinguish Types of series  }
  \item{\code{ exa }}{Examples (Geometric)  }
  \item{\code{ exp }}{Explicit Rule (Geometric)  }
  \item{\code{ ext }}{Extend Series (Geometric)  }
  \item{\code{ ind }}{Induce Rules (Geometric)  }
  \item{\code{ mod }}{Model (Geometric) }
  \item{\code{ rec }}{Recursive Rules (Geometric)  }
  \item{\code{ tab }}{Tabular Representations (Geometric)  }
  \item{\code{ ver }}{Verbal Rules (Geometric)  }
  \item{\code{ pic }}{Pictorial Representations (Geometric)  }
  }

  \code{ACED.items} is data frame with 230 observations on 73
  variables.  These are mostly item-level scores from the field trial. 
  \describe{
    \item{\code{Cond_code}}{a numeric vector giving the experimental
      condition for this student, see also \code{Cond}}
    \item{\code{Seq}}{a factor describing whether the sequence of items
      was  \code{Linear} or \code{Adaptive}}
    \item{\code{FB}}{a factor describing whether the feedback for
      incorrect items was \code{Extended} or \code{AccuracyOnly}}
    \item{\code{All_Items}}{a numeric vector giving the number of items
      in ACED}
    \item{\code{Correct}}{a numeric vector giving the number of items
      the student got correct}
    \item{\code{Incorr}}{a numeric vector giving the number of items the
      student got incorrect}
    \item{\code{Remain}}{a numeric vector giving the number of items not
      reached or skipped}
    \item{\code{ElapTime}}{a numeric vector giving the total time spent
      on ACED}
    The next 63 columns represent the items from the ACED assessment.
    All are factor variables, with possible valued \code{Incorrect} and
    \code{Correct}. The variables are named all named \code{t} (for
    task) followed by the name of one or more variables tapped by the
    task (if there is more than one, then the first one is
    \dQuote{primary}.)  This is followed by a numeric code, 1, 2 or 3,
    giving the difficulty (easy, medium or hard) and a letter (a, b or
    c) used to indicate alternate tasks following the same task model.
    Finally, following a period, there is a version number (all of the
    tasks are version 1).

    After the variables, the last column is:
    \item{\code{Cond}}{a factor describing the experimental condition
      with levels \code{Adaptive/Accuracy}, \code{Adaptive/Extended} and
      \code{Linear/Extended}} 
  }  

  \code{ACED.prePost} is data frame with 290 observations on 32
  variables giving the results of the pretest and posttest.
  
  \describe{
    \item{\code{Cond_code}}{a numeric vector giving the experimental
      condition for this student, see also \code{Cond}}
    \item{\code{Seq}}{a factor describing whether the sequence of items
      was  \code{Linear} or \code{Adaptive}}
    \item{\code{FB}}{a factor describing whether the feedback for
      incorrect items was \code{Extended} or \code{AccuracyOnly}}
    \item{\code{All_Items}}{a numeric vector giving the number of items
      in ACED}
    \item{\code{ Form_Order }}{a factor variables describing whether
    (\code{AB}) Form A was the pretest and Form B was the posttest or
    (\code{BA}) vise versa.}
  \item{\code{ Level_Code }}{a factor variable describing the academic
    track of the student with levels \code{Honors}, \code{Academic},
    \code{Regular}, \code{Part 1}, \code{Part 2} and \code{ELL}.  The
    codes \code{Part 1} and \code{Part 2} refer to special education
    students in Part 1 (mainstream classroom) or Part 2 (sequestered).}
  \item{\code{ PreACorr }}{corrected score on Form A for students who
    took Form A as a pretest  }
  \item{\code{ PostBCorr }}{ corrected score on Form B for students who
    took Form B as a posttest }
  \item{\code{ PreBCorr }}{ corrected score on Form B for students who
    took Form B as a pretest }
  \item{\code{ PostACorr }}{ corrected score on Form A for students who
    took Form A as a posttest }
  \item{\code{ PreScore }}{ a numeric vector with either the non-missing
    value from \code{PreACorr} and \code{PreBCorr} } 
  \item{\code{ PostScore }}{a numeric vector with either the non-missing
    value from \code{PostACorr} and \code{PostBCorr}  }
  \item{\code{ Gender }}{ a factor variable giving the (self-reported)
    gender of the student (codebook is lost) }
  \item{\code{ Race }}{ a factor variable giving the (self-reported)
    race of the student (codebook is lost)  }
  \item{\code{ Gain }}{ \code{PostScore - PreScore} }
  \item{\code{ preacorr_adj }}{ \code{PreACorr} adjusted to put forms A
    and B on the same scale }
  \item{\code{ postbcorr_adj }}{ \code{PostBCorr} adjusted to put forms A
    and B on the same scale }
  \item{\code{ prebcorr_adj }}{\code{PreBCorr} adjusted to put forms A
    and B on the same scale  }
  \item{\code{ postacorr_adj }}{\code{PostACorr} adjusted to put forms A
    and B on the same scale  }
  \item{\code{ Zpreacorr_adj }}{ standardized version of \code{preacorr_adj} }
  \item{\code{ Zpostbcorr_adj }}{ standardized version of \code{postbcorr_adj}  }
  \item{\code{ Zprebcorr_adj }}{ standardized version of \code{prebcorr_adj} }
  \item{\code{ Zpostacorr_adj }}{ standardized version of \code{postacorr_adj} }
  \item{\code{ scale_prea }}{ score on Form A for students who
    took Form A as a pretest scaled to range 0-100 }
  \item{\code{ scale_preb }}{ score on Form B for students who
    took Form B as a pretest scaled to range 0-100 }
  \item{\code{ pre_scaled }}{ scale score on pretest (whichever form)  }
  \item{\code{ scale_posta }}{ score on Form A for students who
    took Form A as a posttest scaled to range 0-100 }
  \item{\code{ scale_postb }}{ score on Form B for students who
    took Form B as a posttest scaled to range 0-100 }
  \item{\code{ post_scaled }}{ scale score on pretest (whichever form) }
  \item{\code{ gain_scaled }}{ \code{post_scaled - pre_scaled }}
  \item{\code{ Flagged }}{ a logical variable (codebook lost) }
  \item{\code{Cond}}{a factor describing the experimental condition
      with levels \code{Adaptive/Accuracy}, \code{Adaptive/Extended},
      \code{Linear/Extended} and \code{Control}} 
  }  
  
}
\details{

  ACED is a Bayesian network based Assessment for Learning learning
  system, thus it served as both a assessment and a tutoring system.  It
  had two novel features which could be turned on and off, elaborated
  feedback (turned off, it provided accuracy only feedback) and adaptive
  sequencing of items (turned off, it scheduled items in a fixed linear
  sequence).  

  It was originally built to cover all algebraic sequences (arithmetic,
  geometric and other recursive), but only the branch of the system
  using geometric sequences was tested.  Shute, Hansen and Almond (2008)
  describe the field trial.  Students from a local middle school (who
  studied arithmetic, but not geometric sequences as part of their
  algebra curriculum) were recruited for the study.  The students were
  randomized into one of four groups:
  \describe{
    \item{\code{Adaptive/Accuracy}}{Adaptive sequencing was used, but
      students only received correct/incorrect feedback.}
    \item{\code{Adaptive/Extended}}{Adaptive sequencing was used, but
      students received extended feedback for incorrect items.}
    \item{\code{Linear/Extended}}{The fixed linear sequencing was used, but
      students received extended feedback for incorrect items.}
    \item{\code{Control}}{The students did independent study and did not
      use ACED.}
  }

  Because students in the control group were not exposed to the ACED
  task, neither the Bayes net level scores nor the item level scores are
  available for those groups, and those students are excluded from
  \code{ACED.scores} and \code{ACED.items}.  The students are in the
  same order in all of the data sets, with the 60 control students
  tacked onto the end of the \code{ACED.prePost} data set.

  All of the students (including the control students) were given a
  25-item pretest and a 25-item posttest with items similar to the ones
  used in ACED.  The design was counterbalanced, with half of the
  students receiving Form A as the pretest and Form B as the posttest
  and the other half the other way around, to allow the two forms to be
  equated using the pretest data.  The details are buried in
  \code{ACED.prePost}.

  Note that some irregularities were observed with the English Language
  Learner (\code{ACED.prePost$Level_code=="ELL"}) students.  Their
  teachers were allowed to translated words for the students, but in
  many cases actually wound up giving instruction as part of the
  translation.  
  
}
\source{

  Shute, V. J., Hansen, E. G., & Almond, R. G. (2008). You can't fatten
  a hog by weighing it---Or can you? Evaluating an assessment for learning
  system called ACED. \emph{International Journal of Artificial
  Intelligence and Education}, \bold{18}(4), 289-316. 

  Thanks to Val Shute for permission to use the data.
  
  ACED development and data collection was sponsored by National Science
  Foundation Grant No. 0313202.  
}
\references{
  A more detailed description, including a Q-matrix can be found at the
  ECD Wiki:  \url{http://ecd.ralmond.net/ecdwiki/ACED/ACED}.
}
\examples{
data(ACED)
}
\keyword{datasets}
