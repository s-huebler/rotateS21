#' EllipseScatter
#'
#'This function will produce a scatterplot of the 2 variables. An ellipse determined by
#'the covariance matrix is overlaid on top of the scatter plot. To form the ellipse
#'we turn to the covariance matrix. The length of the major axis is the square root
#'of the first eigenvalue of the covariance matrix and the direction is determined by
#'the associated eigenvector. Similarly, the length of the mainor axis is the square
#'root of the second eigenvalue of the covariance matrix and the direction is
#'determined by the associated eigenvector.
#'
#' @param df , a data frame of 2 continuous variables
#' @param chi , the desired chi square quantile
#'
#' @return a scatter plot with an overlaid ellipse
#' @export
#'
#' @examples
#' df <- iris
#' ellipseScatter(iris[,c(1,2)])
#'
ellipseScatter <- function(df, chi=.05){
  #df <- df[,c(1,2)]

  xbar <- colMeans(df)
  S <- stats::cov(df)
  eig <- eigen(S)
  chi <- stats::qchisq(chi, 2, lower.tail=FALSE)

  rmaj <- sqrt(abs(eig$values[1])*chi)
  rmin <- sqrt(abs(eig$values[2])*chi)

  ang <- 90-atan(eig$vectors[1,1]/eig$vectors[2,1])*180/pi

  ell <- PlaneGeometry::Ellipse$new(center = xbar,
                          rmajor = rmaj,
                          rminor = rmin,
                          alpha= ang)
  ellpath <- ell$path()
  ellpath <- rbind(ellpath, ellpath[1,])

  p <- ggplot2::ggplot(as.data.frame(ellpath), ggplot2::aes(x=x, y=y))+
    ggplot2::geom_path(color="red")+
    ggplot2::geom_point(data=df, ggplot2::aes(x=df[,1], y=df[,2]))+
    ggplot2::theme_classic()

  p

}
