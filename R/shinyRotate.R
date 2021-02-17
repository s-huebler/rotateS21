#' Interactive Data Cloud Rotation
#'
#' This function launches a shiny app to interact with a data cloud. Explore the effects
#' of different rotations of the axes.You will be able to use the built in sample data
#' set or one of your own. Choose which variables to plot and see how an axis rotation
#' effects the covariance of the variables. Widgets are provided to allow you to change
#' the visual asthetics of the data presentation.
#'
#' @return
#'
#' @export
#'
#' @examples
#' \dontrun{ shinyRotate()}
shinyRotate<-function(){
  shiny::runApp(system.file("Interactive", package="rotateS21"),launch.browser = TRUE)
}
