
## Calculates the mutual information between the rows and columns of a
## two-way table
"mutualInformation" <-
function (table) {
  if (is.data.frame(table)) 
    table <- numericPart(table)
  p12 <- table/sum(table)
  p1 <- apply(p12,1,sum)
  p2 <- apply(p12,2,sum)
  sum(ifelse(table>0,p12*log(p12/(p1%o%p2)),0))
}

ewoe.CPF <- function (cpf, pos=1L, neg= NULL) {
  if (missing(neg)) {
    if (is.numeric(pos)) neg <- -pos
    else if (is.logical(pos)) neg <- !pos
    else if (is.character(pos) && !is.null(rownames(cpf))) {
      neg <- setdiff(rownames(cpf),pos)
    } else {
      stop("Hypothesis not properly specified.")
    }
  }
  dat <- cpf
  if (is.data.frame(cpf)) dat <- numericPart(cpf)
  pYH <- colSums(dat[pos,,drop=FALSE])
  pYH <- pYH/sum(pYH)
  pYnH <- colSums(dat[neg,,drop=FALSE])
  pYnH <- pYnH/sum(pYnH)
  sum(100*log(pYH/pYnH,10)*pYH)
}   


"ciTest" <-
function (tab) 
{
    N <- sum(tab)
    ta13 <- apply(tab, c(1, 3), sum)/N
    ta23 <- apply(tab, c(2, 3), sum)/N
    ta3 <- apply(tab, 3, sum)/N
    exp <- array(N, dim(tab))
    exp <- sweep(sweep(sweep(array(N, dim(tab)), c(1, 3), ta13, 
        "*"), c(2, 3), ta23, "*"), 3, ta3, "/")
    df <- prod(dim(tab)[1:2] - 1)
    G2 <- 2 * sum(tab * log(tab/exp))
    p <- pchisq(G2, df)
    list(G2 = G2, df = df, p = p)
}

"localDepTest" <-
function (obs1, obs2, prof) 
{
    tab <- table(obs1, obs2, prof)
    ciTest(tab)
}

expected <- function(data,model,...) {
  UseMethod("expected")
}

expected.CPA <- function (data,model,...) {
  model <- as.CPA(model)
  ndim <- length(dim(data))
  if (ndim <= 1L) {
    return(model/sum(model)*sum(data))
  }
  ndim <- ndim-1L
  sweep(model,1L:ndim,apply(data,1L:ndim,sum)/apply(model,1:ndim,sum),"*")
}

expected.CPF <- function (data,model,...) {
  model <- as.CPF(model)
  probs <- sapply(data,is.numeric)
  model[probs] <- sweep(model[probs],1L,
                        apply(data[probs],1L,sum)/apply(model[probs],1L,sum),
                        "*")
  model
}


expected.matrix <- function (data,model,...,factorsAsStrings=TRUE) {
  if (is.CPA(data)) return (expected.CPA(data,model,...))
  data <- data
  model <- as.matrix(model,factorsAsStrings=factorsAsStrings)
  sweep(model,1,rowSums(data)/rowSums(model),"*")
}


resids <- function(data,model,type=c("deviance","Pearson","FT"),...) {
  UseMethod("resids")
}



resids.array <- function (data,model,type=c("deviance","Pearson","FT"),
                           ...,factorsAsStrings=TRUE) {
  data <- data
  expt <- expected(data,model,factorsAsStrings=factorsAsStrings)
  if (missing(type)) type <- type[1]
  switch(type,
         Pearson=(data-expt)/sqrt(expt),
         deviance=sign(data-expt)*sqrt(2*data*log(data/expt)-
                                         2*(data-expt)),
         FT=2*(sqrt(data)-sqrt(expt)),
         stop("Unrecognized type ",type))
}

resids.CPF <- function (data,model,type=c("deviance","Pearson","FT"),
                          ...) {
  data <- data
  expt <- expected(as.matrix(data),model)
  if (missing(type)) type <- type[1]
  probs <- sapply(data,is.numeric)
  data[,probs] <- resids(as.matrix(data),expt,type)
  data
}

g2test <- function (data,model,type=c("deviance","Pearson","FT"),
                    cc=TRUE) {
  model <- normalize(as.matrix(model))
  data <- as.matrix(data)
  if (cc) data <- data + model
  if (missing(type)) type <- type[1]
  res <- resids(data,model,type)
  G2 <- sum(res*res)
  df <- dim(res)[1]*(dim(res)[2]-1)
  attr(G2,"d.f.") <- df
  G2
}


cptChi2 <- function(obs, exp) {
  lifecycle::deprecate_warn("0.9-1","cptChi2()",'g2test(type="Pearson")')
  
  if (!all.equal(getTableStates(obs),getTableStates(exp))) {
    stop("Child variable states don't match.")
  }
  if (!all.equal(getTableParents(obs),getTableParents(exp))) {
    stop("Parent variables don't match.")
  }
  if (nrow(obs)!=nrow(exp)) {
    stop("Observed and expected table sizes don't match.")
  }
  
  ## Build CI's for cumulative probabilities.
  cobs <- numericPart(exp)+numericPart(obs)
  cobs.tot <-apply(cobs,1,sum)
  nexp <- sweep(numericPart(exp),1,cobs.tot,"*")
  
  df = nrow(cobs)*(ncol(cobs)-1)
  chi2 <- sum((cobs-nexp)^2/nexp)
  attr(chi2,"d.f.") <- df
  chi2
}

cptKL <- function(est, exp) {
  
  if (nrow(est)!=nrow(exp)) {
    stop("Estimated and expected table sizes don't match.")
  }
  
  if (is.CPF(est)) {
    est <- numericPart(est)
  }
  if (is.CPF(exp)) {
    exp <- numericPart(exp)
  }
  
  df = nrow(est)*(ncol(est)-1)
  dkl <- sum(est*log(est/exp))
  attr(dkl,"d.f.") <- df
  dkl
}

cptG2 <- function(obs, exp) {
  lifecycle::deprecate_warn("0.9-1","cptChi2()",'g2test(type="deviance")')
  
  if (!all.equal(getTableStates(obs),getTableStates(exp))) {
    stop("Child variable states don't match.")
  }
  if (!all.equal(getTableParents(obs),getTableParents(exp))) {
    stop("Parent variables don't match.")
  }
  if (nrow(obs)!=nrow(exp)) {
    stop("Observed and expected table sizes don't match.")
  }
  
  ## Build CI's for cumulative probabilities.
  cobs <- numericPart(exp)+numericPart(obs)
  cobs.tot <-apply(cobs,1,sum)
  nexp <- sweep(numericPart(exp),1,cobs.tot,"*")
  
  df = nrow(cobs)*(ncol(cobs)-1)
  g2 <- sum(cobs*log(cobs/nexp))
  attr(g2,"d.f.") <- df
  g2
}
