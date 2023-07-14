\name{ACED.scores}
\alias{ACED}
\alias{ACED.scores}
\alias{ACED.items}
\alias{ACED.prePost}
\alias{ACED.skillNames}
\alias{ACED.taskNames}
\alias{ACED.splitHalves}
\alias{ACED.pretest}
\alias{ACED.posttest}
\alias{ACED.Qmatrix}
\alias{ACED.QEM}
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
  ACED contains 3 primary \code{data.frame} objects and some supplementary data.
  
  All of the data tables have two variables which can serve as keys.  \code{SubjID} and \code{AltID}.  
  Either can be used as a primary key in joins.  Note that the first two digits of the \code{AltID} 
  gives the session (i.e., class) that the student was in.  Note also that students in 
  the control group have only pretest and posttest data; hence they do not appear in 
  \code{ACED.items}, \code{ACED.scores} or \code{ACED.splitHalves}.

  \code{ACED.scores} is data frame with 230 observations on 74
  variables.  These are mostly high-level scores from the Bayesian
  network.  
  \describe{
    \item{\code{Cond_code}}{a factor giving the experimental
      condition for this student, the levels are 
      \dQuote{adaptive_acc}, \dQuote{adaptive_full}, \dQuote{linear_full}, and
      \dQuote{control}.  Note that there are no control students in this data set.}
    \item{\code{Sequencing}}{a factor describing whether the sequence of items
      was  \code{Linear} or \code{Adaptive}}
    \item{\code{Feedback}}{a factor describing whether the feedback for
      incorrect items was \code{Extended} or \code{AccuracyOnly}}
    \item{\code{All_Items}}{a numeric vector giving the number of items
      in ACED}
    \item{\code{Correct}}{a numeric vector giving the number of items
      the student got correct}
    \item{\code{Incorr}}{a numeric vector giving the number of items the
      student got incorrect}
    \item{\code{Remain}}{a numeric vector giving the number of items not
      reached or skipped}
    \item{\code{ElapTime}}{a difftime vector giving the total time spent
      on ACED (in seconds)}
    The next group of columns give \dQuote{scores} for each of the nodes in
    the Bayesian network.  Each node has four scores, and the columns are
    names \code{p}\emph{node}\emph{ScoreType} where \emph{node} is
    replaced by one of the short codes in \code{ACED.allSkills}.
    \item{\code{p}\emph{node}\code{H}}{a numeric vector giving the probability
      \emph{node} is in the high state}
    \item{\code{p}\emph{node}\code{M}}{a numeric vector giving the probability
      \emph{node} is in the medium state}
    \item{\code{p}\emph{node}\code{L}}{a numeric vector giving the probability
      \emph{node} is in the low state}
    \item{\code{EAP}\emph{node}}{the expected a posteriori value of \emph{node}
      assuming an equal interval scale, where \code{L=0}, \code{M=1} and
      \code{H=2}} 
    \item{\code{MAP}\emph{node}}{a factor vector giving maximum a
      posteriori value of \emph{node}, i.e.,
      \code{which.max(p}\emph{node}\code{H, p}\emph{node}\code{M, p}\emph{node}\code{L)}.}
  }

  \code{ACED.skillNames} a list with two components, \code{long} and \code{short} giving
  the long (spelled out in CamelCase) and short names for the skills is a character vector giving the abbreviations
  used for the node/skill/attributes names.  


  \code{ACED.items} is data frame with 230 observations on 73
  variables.  These are mostly item-level scores from the field trial. 
  The first two columns are \code{SubjID} and \code{AltID}.  The remaining columns
  correspond to ACED internal tasks, and are coded 1 for correct, 0 for incorrect, and 
  \code{NA} for not reached.

  \code{ACED.taskNames} is essentially the row names from \code{ACED.items}.
  The naming scheme for the tasks reflect the skills measured by the task.  
  The response variable names 
  all start with \code{t} (for task) followed by the name of one or more 
  skills tapped by the task (if there is more than one, then the first one is
  \dQuote{primary}.)  This is followed by a numeric code, 1, 2 or 3,
  giving the difficulty (easy, medium or hard) and a letter (a, b or
  c) used to indicate alternate tasks following the same task model.

  \code{ACED.prePost} is data frame with 290 observations on 32
  variables giving the results of the pretest and posttest.
  
  \describe{
    \item{\code{SubjID}}{ID assigned by study team, \dQuote{S} followed by 3 digits.  Primary key.}
    \item{\code{AltID}}{ID assigned by the ACED software vendor.  Pattern is \dQuote{sXX-YY}, 
      where \emph{XX} is the session and \emph{YY} is a student with the session.}
    \item{\code{Classroom}}{A factor correpsonding to the student's class.}
    \item{\code{Gender}}{A factor giving the student's gender (I'm not sure if this is
      self-report or administrative records.)}
    \item{\code{Race}}{A factor giving the student's self-reported race.  The codebook  
      is lost.}  
   \item{\code{ Level_Code }}{a factor variable describing the academic
     track of the student with levels \code{Honors}, \code{Academic},
     \code{Regular}, \code{Part 1}, \code{Part 2} and \code{ELL}.  The
     codes \code{Part 1} and \code{Part 2} refer to special education
     students in Part 1 (mainstream classroom) or Part 2 (sequestered).}
   \item{\code{ pre_scaled }}{ scale score (after equating) on pretest }
   \item{\code{ post_scaled }}{ scale score (after equating) on posttest  }
   \item{\code{ gain_scaled }}{ \code{post_scaled - pre_scaled }}
   \item{\code{ Form_Order }}{a factor variables describing whether
     (\code{AB}) Form A was the pretest and Form B was the posttest or
     (\code{BA}) vise versa.}
  \item{\code{ PreACorr }}{number of correct items on Form A for students who
    took Form A as a pretest  }
  \item{\code{ PostBCorr }}{number of correct items on Form B for students who
    took Form B as a posttest }
  \item{\code{ PreBCorr }}{number of correct items on Form B for students who
    took Form B as a pretest }
  \item{\code{ PostACorr }}{number of correct items on Form A for students who
    took Form A as a posttest }
  \item{\code{ PreScore }}{ a numeric vector with either the non-missing
    value from \code{PreACorr} and \code{PreBCorr} } 
  \item{\code{ PostScore }}{a numeric vector with either the non-missing
    value from \code{PostACorr} and \code{PostBCorr}  }
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
  \item{\code{ Flagged }}{ a logical variable (codebook lost) }
  \item{\code{Cond}}{a factor describing the experimental condition
    with levels \code{Adaptive/Accuracy}, \code{Adaptive/Extended},
    \code{Linear/Extended} and \code{Control}.  Note that controls are included in this data set.} 
  \item{\code{Sequencing}}{a factor describing whether the sequence of items
    was  \code{Linear} or \code{Adaptive}}
  \item{\code{Feedback}}{a factor describing whether the feedback for
    incorrect items was \code{Extended} or \code{AccuracyOnly}}
  }  
  
  \code{ACED.Qmatrix} is a logical matrix whose rows correspond to skills (long names)
  and whose columns correspond to tasks which is true if the skill is required for solving 
  the task (according to the expert).
  
  \code{ACED.QEM} is a reduced Q-matrix containing the 15 \emph{evidence models} 
  (unique rows in the $Q$-matrix).  The entries are character values with "0" indicating skill not needed, "+"   
  indicating skill is needed and "++" indicating skill is primary.  The \code{Tasks} column lists
  the tasks corresponding to this evidence model (1, 2 and 3 again represent difficulty level, and the
  letters indicating variants).  The \code{Anchor} column is used to identify subscales for scale identification.
  
  \code{ACED.splithalves} is a list of two datasets labeled \dQuote{A} and \dQuote{B}.  Both 
  have the same structure as \code{ACED.scores} (less the datas giving study condition).  These were created
  by splitting the 62 items into 2 subforms with 31 items each.  For the most part, each item was paired with
  an variant which differed only by the last letter.  The scores are the results of Bayes net scoring with
  half of the items.
  
  \code{ACED.pretest} and \code{ACED.posttest} are raw data from the external pretest and posttest given 
  before and after the study.  Each is a list with four components:
  \describe{
    \item{\code{Araw}}{Unscored responses for students who took that form as pre(post)test.  The first
      row is the key.}
    \item{\code{Ascored}}{The scored responses for the \code{Araw} students; correct is 1, incorrect is 0.}
    \item{\code{Braw}}{Unscored responses for students who took that form as pre(post)test.  The first
      row is the key.}
    \item{\code{Bscored}}{The scored responses for the \code{Araw} students; correct is 1, incorrect is 0.}
  } 
  Because of the counterbalancing each student should appear in either Form A or Form B in the pretest 
  and in the other group in the posttest.  Note that the A and B forms here have no relationship with the A and B   
  forms in \code{ACED.splithalves}.
  
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
