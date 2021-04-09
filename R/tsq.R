#' Tsq Chart
#'
#' This chart will give an idea of which observations are out of control. The chi square
#' distance of each observation is plotted on the y axis, and the ordering of the x axis
#' comes from time series ordering. The upper control limit (percent defined by the chi
#' argument) is also charted. Observations above this line are considered out of control
#' and should be examined further.
#'
#' @param df , a data frame
#' @param chi , the desired alpha level
#'
#' @return , a tsq chart
#' @export
#'
#' @examples
#'
#'
tsq <- function(df, chi=0.05){
  n <- nrow(df)
  a <- ncol(df)

  s <- stats::cov(df)
  sInv <- solve(s)

  ucl <- stats::qchisq(chi, a, lower.tail = FALSE)

  means <- sapply(df, FUN=function(x){scale(x, center=TRUE, scale=FALSE)})
  means <- as.data.frame(means)

  multi<-c()
  for(i in 1:n){
    multi[i]=as.matrix(means[i,]) %*% sInv %*% t(as.matrix(means[i,]))
  }


  temp1 <- seq(1:n)
  tsq <- data.frame("j" <- temp1, "d2" <- multi)

p <- ggplot2::ggplot(data=tsq, ggplot2::aes(x=j, y=d2))+
  ggplot2::geom_point()+
  ggplot2::geom_hline(yintercept = ucl, lty=2, color="red")+
  ggplot2::geom_path()+
  ggplot2::xlab("Observation Number")+
  ggplot2::ylab("Tsq")+
  ggplot2::theme_classic()

print(p)

}
