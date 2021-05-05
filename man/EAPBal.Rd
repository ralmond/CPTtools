\name{EAPBal}
\alias{EAPBal}
\title{Produces a graphical balance sheet for EAP or other univarate statistics.}
\description{

  This provides a graph of the history of any given measure of
  performance.  The input should be a list of values \code{hist} and the
  list of the \code{contexts} or game levels associated with them.

}
\usage{
EAPBal(hist, contexts = names(hist), obs = NULL,
varname = deparse(substitute(hist)),
elim = c(-1, 1), etic = 4, title = paste("EAP Balance Sheet:", varname),
col = "slategray", posCol = "cyan", negCol = "red",
stripCol = c("white", "lightgray"), lcex = 0.65)
}
\arguments{
  \item{hist}{A vector of doubles giving the value of the statistic at
    each time point.}
  \item{contexts}{A list of names for the events associated with the
    values.  Defaults to the names of the \code{hist} argument.}
  \item{obs}{Observable values associated with each level.  If supplied
    it should be a vector as long as \code{hist}.}
  \item{varname}{The name of the variable which is being monitored.}
  \item{elim}{A vector of length two giving the minimum and maximum EAP value.}
  \item{etic}{How many tic marks to use on the EAP axis.}
  \item{title}{The main title for the overall plot.}
  \item{col}{Color for the EAP bars.}
  \item{posCol}{Color for positive movements.}
  \item{negCol}{Color for negative movements.}
  \item{stripCol}{The colors to be used for the time step labels.
    Setting this to a vector of two colors creates alternate color
    stripes.  Set this to "white" to disable that effect.}
  \item{lcex}{Character expansion size for labels.}
}
\details{

    Madigan, Mosurski and Almond (1997) described a graphical weight of
    evidence balance sheet.  The key to this display is following a
    particular node (or more properly hypothesis involving a node)
    across time as evidence is entered into the system.  There are two
    columns, one showing the probability distribution, and one showing
    the weight of evidence---the change in probabilities.

    Often, the nodes in an ordered categorical value are assigned
    numeric values and an \emph{expected a posteriori} or EAP value is
    calculated instead.  The EAP balance sheet is similar to the woe
    balance sheet, except that it now uses a single numeric value.

    The input is a sequence of EAP value, labeled by the event which
    causes the change.  The output is a graphical display in three columns.

}
\value{
  The midpoints of the bars (see \code{\link[graphics]{barplot}}) are
  returned invisibly.
}
\references{
  Madigan, D.,  Mosurski, K. and Almond, R. (1997) Graphical explanation
  in belief networks.  \emph{Journal of Computational Graphics and
    Statistics}, \bold{6}, 160-181.

}
\author{Russell Almond}
\seealso{
  \code{\link{woeBal}}, \code{\link[graphics]{barplot}},
  \code{\link[grDevices]{colors}} 
}
\examples{
sampleSequence <- read.csv(paste(library(help="CPTtools")$path,
                                "testFiles","SampleStudent.csv",
                                sep=.Platform$file.sep),
                           header=TRUE,row.names=1)
SolveGeometricProblems <- sampleSequence$H-sampleSequence$L
names(SolveGeometricProblems) <- rownames(sampleSequence)
EAPBal(SolveGeometricProblems,  lcex=1.25)
EAPBal(SolveGeometricProblems,rownames(sampleSequence),
      varname="Solve Geometric Problems",
      obs=sampleSequence[,"Acc"],lcex=1.25)

}
\keyword{hplot}
