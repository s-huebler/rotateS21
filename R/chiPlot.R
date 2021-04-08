#' Chi QQ Plot
#'
#' This plot will show the ordered squared distances for each of the observations on
#' the y axis. On the x axis we have the chi square quantiles of (j-0.5)/n for each j.
#' The quantiles are calculated with the chi square distribution on p degrees of freedom
#' where p is the number of columns in the data frame.
#'
#' @param df , a data frame with continuous numeric columns
#'
#' @return , a qq chi plot
#' @export
#'
#' @examples
#' df <- iris
#' chiPlot(iris[,c(1,2,3)])
#'
chiPlot <- function(df) {

  n <- nrow(df)
  a <- ncol(df)

  s <- cov(df)
  sInv <- solve(s)

  means <- sapply(df, FUN=function(x){scale(x, center=TRUE, scale=FALSE)})
  means <- as.data.frame(means)

  multi<-c()
  for(i in 1:n){
    multi[i]=as.matrix(means[i,]) %*% sInv %*% t(as.matrix(means[i,]))
  }

  multi<-sort(multi)

  chi_plot<-data.frame("j"=seq(1:n), "d2"=multi) %>%
    mutate("q"=qchisq((j-0.5)/n, a))

  p <- ggplot(chi_plot, aes(x=q, y=d2))+
    geom_point()+
    theme_classic()+
    xlab("Chi")+
    ylab("Dist")+
    ggtitle("Chi-Square Plot for Multivariate Data")

  suppressWarnings(p)
}
