
## calcLearnForget
##
## Calculates a CPT for a growth model using a single learning and
## forgetting parameter.
## @param states -- A character vector of states for the variable from
##   high to low.  
## @param lambda -- A numeric vector giving the transition
##   probabilites for transitions up to the next level.  Length should
##   be one less than the number of states.
## @param mu -- A numeric vector giving the transition
##   probabilites for transitions down to the previou level.  Length should
##   be one less than the number of states.
## @param time -- A numeric scalar (default 1) giving the time
##   conversion from rate to probability.
calcLearnForget <- function (states,lambda,mu,time=1) {
  K <- length(states)
  rates <- matrix(NA,K,K,dimnames=list(states,states))
  if (length(lambda)==1L) lambda <- rep(lambda,K-1)
  if (length(mu) == 1L) mu <- rep(mu,K-1)
  for (i in 1L:K) {
    if (i < K) { ## Room to decline
      ## Off diagonal
      j <- i +1L
      rates[i,j] <- mu[j-1]
      while (j < K) { ## Rest of row
        j <- j+1L
        rates[i,j] <- mu[j-1]*rates[i,j-1]
      }
    } 
    if (i > 1L) { ## Room to grow
      ## Off diagonal
      j <- i - 1L
      rates[i,j] <- lambda[j]
      while (j > 1L) { ## Rest of row
        j <- j-1L
        rates[i,j] <- lambda[j]*rates[i,j+1]
      }
    }
  }
  ## Scale by time constant
  probs <- time*rates*exp(-time*rates)
  ## Normalize the diagonal
  diag(probs) <- 1-rowSums(probs,na.rm=TRUE)
  probs
}
    
    
    
