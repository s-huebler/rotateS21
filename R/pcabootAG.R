#' Bootstrap Analysis of PCA
#'
#' This function allows the user to make estimates for the eigenvalues and eigenvectors
#' of a covariance or correlation matrix. The function provides bootstrap and
#' Anderson-Girshick estimates for the lambdas (eigenvalues). The function provides
#' bootstrap estimates for the eigenvectors. The iter argument controls the number of
#' times the bootstrap is run, and the alpha determines the confidence level printed
#' to the console. Histograms for the lambdas and violin plots for the eigenvectors are
#' printed as well. The three types of confidence intervals are stored as lists to be
#' called if the results of the function are stored as an object.
#'
#'
#' @param df , df is a data frame of quantitative variables
#' @param alpha, alpha is a confidence level
#' @param iter, iter is the number of desired bootstrap iterations
#' @param covar, covar is a logical arg where TRUE uses covariance and FALSE uses correlation
#'
#' @return A list a confidence intervals
#' @export
#'
#' @examples
#' df <- iris[c(2,3)]
#' pcabootAG(df, 0.01, iter=1000, covar=TRUE )
#'
pcabootAG <- function(df, alpha=0.05, iter, covar=TRUE){
  # Setup and checks

      # Set up correlation vs covariance
  if(covar==FALSE){
    mat <- stats::cor(df)
  }else{
    mat <- stats::cov(df)
  }

      # Size
  n <- nrow(df)
  p <- ncol(df)

  eig <- eigen(mat)
  lambdas <- eig$values
  eHats <- eig$vectors

  # Bootstrap Analysis

      # Lambdas
  bootsLam <- boot::boot(df,
                       function(data, indices){
                        dataRed <- data[indices,]
                        matRed <- stats::cov(dataRed)

                        if(covar==FALSE){
                          matRed <- cov2cor(matRed)
                        }

                        eigRed <- eigen(matRed)
                        lambdasRed <- eigRed$values

                        return(lambdasRed)},
                      iter)

      # Eigenvectors

bootsVectors <- boot::boot(df,
                       function(data, indices){
                         dataRed <- data[indices,]
                         matRed <- stats::cov(dataRed)

                         if(covar==FALSE){
                           matRed <- cov2cor(matRed)
                         }

                         eigRed <- eigen(matRed)
                         vecsRed <- eigRed$vectors


                         return(as.vector(vecsRed))},
                       iter)




  # Lambda

  bootLambdas <- as.data.frame(bootsLam$t)
  bootVecsAll <- as.data.frame(bootsVectors$t)

ciBootLams <- list()
ciAgLams <- list()
ciBootVecs <- list()
for(i in 1:p){
  x <- bootLambdas[,i]
   h <- hist(x, plot=FALSE)
   d <- h$density
   den <- d/max(d)

   # Bootstrap CI
   ci <- stats::quantile(bootLambdas[,i], c(alpha/2, 1-alpha/2))
   ciBootLams <- list(ciBootLams,list(ci[1], ci[2]))
   print(paste("(1-", alpha, ")%", "Bootstrap CI", "lambda", i,":", ci[1],ci[2]))

   # AG CI
   z <- stats::qnorm(0.05/2, lower.tail = FALSE)
   agL <- eig$values[i]/(1+z*sqrt(2/n))
   agU <- eig$values[i]/(1-z*sqrt(2/n))
   print(paste("(1-", alpha, ")%", "AG CI", "lambda", i,":", agL, agU))
   ciAgLams <- list(ciAgLams, list(agL, agU))

   # Histogram
    p1 <- ggplot2::ggplot(bootLambdas, ggplot2::aes(x=x))+
      ggplot2::geom_histogram(ggplot2::aes(y=..density.., fill=..density..),
                              show.legend = FALSE,
                              bins=25)+
      ggplot2::scale_fill_gradient(low="green", high="blue")+

      ggplot2::theme_classic()+
      ggplot2::xlab(bquote(widehat(lambda)[.(i)]))+
      ggplot2::ggtitle(
        bquote("Distribution of": widehat(lambda)[.(i)]))+
      ggplot2::ylab("Density")+
      ggplot2::labs(
        caption=paste("(1-", alpha, ")%", "Confidence Interval:",
        "(", round(ci[1], 4), ",", round(ci[2],4), ")")
      )
    print(p1)


    # Violin Plots
    bootVec1 <- bootVecsAll[,((p*(i-1)+1): (p*i))]

    labels <- c()
      for(j in 1:p){
        labels[j] <- paste("e", j)
      }
    names(bootVec1) <- labels

    bootVec <- tidyr::pivot_longer(bootVec1, everything())

    p2 <- ggplot2::ggplot(data=bootVec, ggplot2::aes(x=name, y=value))+
      ggplot2::geom_violin(trim=TRUE)+
      ggplot2::theme_classic()+
      ggplot2::ggtitle(
        bquote("Eigen Vectors Associated with": widehat(lambda)[.(i)]))+
      ggplot2::ylim(min(bootVec$value)-stats::sd(bootVec$value),
                    max(bootVec$value)+stats::sd(bootVec$value))+
      ggplot2::ylab("")+
      ggplot2::xlab(" ")

    print(p2)

    for(k in 1:p){
    ci2 <- stats::quantile(bootVec1[,k], c(alpha/2, 1-alpha/2))
    ciBootVecs <- list(ciBootVecs,list(ci2[1], ci2[2]))
    print(paste("(1-", alpha, ")%", "Bootstrap CI", "e", i, k, ":", ci2[1],ci2[2]))
    }

}

invisible(
list(
  "Bootstrap_Lambda_CI" =  unlist(ciBootLams),
  "Bootstrap_Eigen_Vector_CI" = unlist(ciBootVecs),
  "AG_Lambda_CI" = unlist(ciAgLams)
)
)
}
