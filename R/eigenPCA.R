#' Perform an Eigen Analysis to Construct Principal Components
#'
#' This function will allow you to construct a set of principal components using
#' the eigen analysis of the covariance matrix built from the input data frame. The
#' function will return, as a list, the covariance matrix, the linear combinations
#' of the principal components, the total sample variance of each component, and a
#' matrix containing the correlation coefficients between each linear combination and
#' each variable.
#'
#' @param df , df is a data frame of quantatative variables
#'
#' @return
#' @export
#'
#' @examples
#' data <- iris[,1:3]
#' eigenPCA(data)
eigenPCA <- function(df){
  vars <- names(df)

  p <- ncol(df)

  S <- cov(df)
  eig <- eigen(S)
  lambdas <- eig$values
  pc <- as.data.frame(eig$vectors, row.names=vars)

  prinNames <- c()
  eigNames <- c()
  for(i in 1:p){
    prinNames[i] <- paste("Prin.Comp", i)
    eigNames[i] <- paste("X", i)
  }

  names(pc) <- prinNames

  variance <- lambdas/sum(lambdas)
  names(variance) <- prinNames
  cumvar <- cumsum(lambdas)/sum(lambdas)
  names(cumvar) <- prinNames

  corrs <- data.frame(NA, nrow=p, ncol=p)
  for(j in 1:p){
    for(k in 1:p){
      corrs[j,k] <- pc[j,k]*sqrt(lambdas[j])/S[j,j]
    }
  }
  names(corrs) <- eigNames
  row.names(corrs) <- prinNames


  ret <- list(
    "Covariance_Matrix"= S,
    "Principal_Component_Matrix" = pc,
    "Sample_Variance" = variance,
    "Cummulative_Variance" = cumvar,
    "Correlations_Component" = corrs

  )

  ret
}
