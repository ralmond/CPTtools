
## Calculates the mutual information between the rows and columns of a
## two-way table
"mutualInformation" <-
function (table) {
  p12 <- table/sum(table)
  p1 <- apply(p12,1,sum)
  p2 <- apply(p12,2,sum)
  sum(ifelse(table>0,p12*log(p12/(p1%o%p2)),0))
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



