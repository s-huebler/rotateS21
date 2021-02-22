dropCor <- function(X, coln = c(1,2)){

  Xn <- X[,coln] #2 column data frame
  n <- dim(X)[1] #number of observations
  cr <- 1:n # initial vector to hold correlations

  for(i in 1:n){
    cr[i] <- cor(Xn[-i,])[1,2] # correlation proper
  }

  cr
}


myTilde<-function(X, theta){
  theta<-theta*pi/180

  x1t= cos(theta)*X[,1] + sin(theta)*X[,2]
  x2t= -sin(theta)*X[,1] + cos(theta)*X[,2]
  list(x1t=x1t,x2t=x2t)
}
