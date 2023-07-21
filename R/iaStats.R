
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



