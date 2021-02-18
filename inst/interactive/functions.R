dropCor <- function(X, coln = c(1,2)){

  Xn <- X[,coln] #2 column data frame
  n <- dim(X)[1] #number of observations
  cr <- 1:n # initial vector to hold correlations

  for(i in 1:n){
    cr[i] <- cor(Xn[-i,])[1,2] # correlation proper
  }

  cr
}
