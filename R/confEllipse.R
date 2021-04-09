#' Bivariate Quality Control Ellipse
#'
#' This function has 2 main features. The first feature is the quality control ellipse
#' chart itself. The chart can be used to visually asses bivariate normality. The
#' shape and direction of the ellipse gives information about the covariance between
#' the 2 data columns. Further information about the ellipse can be found by printing
#' the function. Printing the function as an object will return as a list the eigen
#' vectors and values of the covariance matrix, the half width of the major and minor
#' axes, and the ratio of major to minor axis. The second feature is test points. A
#' vector argument can be supplied to be plotted on the control chart. The point is
#' plotted in red for easy viewing. The user can easily determine whether the point
#' being tested falls inside the quality control ellipse.
#'
#'
#' @param data , a 2 column data frame
#' @param vec , a test point in the form of a 2x1 bector
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
confEllipse<-function(data, vec=NULL, alpha=0.05, formatted=TRUE){

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

  if(!is.null(vec)){
  testStat <- n *  ( t(xbar - vec) %*% invS %*% (xbar - vec) )

  # Performing test
  if ( testStat <= fScale){
    result = "Do not reject null: point within confidence region"
  }else{
    result = "Reject null: point not within confidence region"
  }

  vector <- as.data.frame(t(vec))
  print(testStat)
  }
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


  # inside<-c()
  # for(i in 1:n){
  #   point <- t(data[i,])
  #   m <- n *  ( t(xbar - point) %*% invS %*% (xbar - point) )
  #   if (m <= fScale){
  #     inside[i] = "No"
  #   }else{
  #     inside[i] = "Yes"
  #   }
  # }


  #data$inside <- inside

  centroid <- as.data.frame(t(xbar))



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

  if(!is.null(vec)){
   plot <- plot +
     ggplot2::geom_point(data=vector, ggplot2::aes(x=vector[,1], y=vector[,2]),
                         color="red",
                         size=1)
  }



  suppressWarnings(print(plot))

# Return
  ret = list(
        #"Test_Result" = result,
       #"Size_of_Quadratic_Form" = testStat,
       #"Scaled_Quantile" = fScale,
       "First_Eigen_Value" = eVal[1],
       "First_Eigen_Vector" = eVec[,1],
       "Second_Eigen_Value" = eVal[2],
       "Second_Eigen_Vector" = eVec[,2],
       "Major_Half_Width" =  axMajor,
       "Minor_Half_Width" = axMinor,
       "Ratio" = ratio
       )
}
