#' Simultaneous Bonferroni Confidence Intervals for Lambdas
#'
#' This function will perform simulataneous confidence interval estimates. The
#' estimates are corrected using the bonferroni method of correction.
#'
#' @param df , df a data frame
#' @param m , m the number of simultaneous intervals
#'
#' @return
#' @export
#'
#' @examples
#' data <- iris[,1:3]
#' bonCI(data, 2)
bonCI <- function(df, m, alpha=0.05){

  n <- nrow(df)
  S <- cov(df)
  eig <- eigen(S)

  ciAgLams <- c()
  for(i in 1:m){
    z <- stats::qnorm(alpha/(2*m), lower.tail = FALSE)
    agL <- eig$values[i]/(1+z*sqrt(2/n))
    agU <- eig$values[i]/(1-z*sqrt(2/n))
    print(paste("(1-", alpha, ")%", "AG CI", "lambda", i,":", "(" ,
                round(agL, 6), round(agU, 6), ")"))
    ciAgLams <- c(ciAgLams, agL, agU)
    #append(ciAgLams, list(agL, agU))
  }

  invisible(ciAgLams)
}
