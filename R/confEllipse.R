#' Bivariate Confidence Ellipse for Multivariate Normal Data
#'
#' @param data , a data frame
#' @param vec , a test point
#' @param alpha , a confidence level
#' @param formatted , a logical argument to determine whether the data is formatted correctly
#'
#' @return
#' @export
#'
#' @examples
#'
#' data(iris)
#' iris2<-iris[,c(1,2)]
#' confEllipse(iris2, c(6, 2.9))
#'
confEllipse<-function(data, vec, alpha=0.05, formatted=TRUE){

  # Check to make sure that data is pre-formatted
  if (formatted == FALSE){
    try(if(identical(unname(sapply(data, FUN=is.numeric)), rep(TRUE, ncol(data))))
      stop("Data frame must be numeric"))

    try(if(length(vec) != 2)
      stop("Improper hypothesis vector dimensions"))

    print("Warning: Pre-format data")
  }

  # If data frame has more than 2 columns
  if ( dim(data)[2] != 2){
    data <- data[,c(1,2)]
    print ("Warning: Only first 2 columns used")
  }

  # Setting up variables
  n <- dim(data)[1]
  xbar <- colMeans(data)
  S <- stats::cov(data)
  invS <- solve(S)


  f <- stats::qf(alpha, 2, (n-2), lower.tail = FALSE)
  fScale <- 2 * (n-1) / (n-2) * f

  testStat <- n *  ( t(xbar - vec) %*% invS %*% (xbar - vec) )

  # Performing test
  if ( testStat <= fScale){
    result = "Do not reject null: point within confidence region"
  }else{
    result = "Reject null: point not within confidence region"
  }
  print(testStat)

  # Eigen analysis
  e <- eigen(S)

  eVal <-e$values
  eVec <- e$vectors


  # Half widths
  ax1 <- sqrt(eVal[1]) * sqrt(fScale / n)
  ax2 <- sqrt(eVal[2]) * sqrt(fScale / n)

  axMajor=max(ax1, ax2)
  axMinor=min(ax1, ax2)

  # Ratio
  ratio <- sqrt(eVal[1])/sqrt(eVal[2])

  # Plot
  names <- names(data)

  xbar <- as.matrix(xbar, nrow=2)


  inside<-c()
  for(i in 1:n){
    point <- t(data[i,])
    m <- n *  ( t(xbar - point) %*% invS %*% (xbar - point) )
    if (m <= fScale){
      inside[i] = "No"
    }else{
      inside[i] = "Yes"
    }
  }


  data$inside <- inside

  centroid <- as.data.frame(t(xbar))

  vector <- as.data.frame(t(vec))

  ellipseDf <- as.data.frame(
    car::dataEllipse(data[,1], data[,2], levels=(1-alpha), draw=FALSE))

  ellipseDf2 <- as.data.frame(
    car::dataEllipse(data[,1], data[,2], levels=(alpha), draw=FALSE))


  plot<-ggplot2::ggplot(data, ggplot2::aes(x=data[,1], y=data[,2]))+
    #ggplot2::geom_point(ggplot2::aes(color=factor(inside)))+
    ggplot2::geom_point(size=0.5)+
    ggplot2::xlim(min(ellipseDf[,1]),
                  max(ellipseDf[,1]))+
    ggplot2::ylim(min(ellipseDf[,2]),
                  max(ellipseDf[,2]))+
    # ggplot2::geom_polygon(data=ellipseDf,
    #                       inherit.aes = FALSE,
    #                       ggplot2::aes(x=ellipseDf[,1], y=ellipseDf[,2]),
    #                       fill=NA,
    #                       color="black")+
    ggplot2::geom_polygon(data=ellipseDf2,
                          inherit.aes = FALSE,
                          ggplot2::aes(x=ellipseDf2[,1], y=ellipseDf2[,2]),
                          fill=NA,
                          color="blue")+
    ggplot2::geom_point(data=centroid, ggplot2::aes(x=centroid[,1], y=centroid[,2]),
                        color="blue",
                        size=1)+
    ggplot2::geom_point(data=vector, ggplot2::aes(x=vector[,1], y=vector[,2]),
                        color="red",
                        size=3)+
    ggplot2::geom_segment(x= centroid[,1], xend=centroid[,1],
                          y=0, yend= centroid[,2],
                          lty=2,
                          color="blue",
                          size=0.25)+
    ggplot2::geom_segment(x= 0, xend=centroid[,1],
                          y=centroid[,2], yend= centroid[,2],
                          lty=2,
                          color="blue",
                          size=0.25)+
    ggplot2::xlab(names[1])+
    ggplot2::ylab(names[2])+
    ggplot2::ggtitle("Confidence Ellipse")+
    ggplot2::theme_classic()



  suppressWarnings(print(plot))

# Return
  ret = list("Test_Result" = result,
       "Size_of_Quadratic_Form" = testStat,
       "Scaled_Quantile" = fScale,
       "First_Eigen_Value" = eVal[1],
       "First_Eigen_Vector" = eVec[,1],
       "Second_Eigen_Value" = eVal[2],
       "Second_Eigen_Vector" = eVec[,2],
       "Major_Half_Width" =  axMajor,
       "Minor_Half_Width" = axMinor,
       "Ratio" = ratio
       )
}
