#' Interactive Multivariate Analysis
#'
#' This function will launch the shiny app that allows you to interact with the multivariate analysis data I've chosen for project 3.
#' The app goes over trends in covid severity as it relates to demographic information about each patient.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{ shinyProject3()}
shinyProject3<-function(){
  shiny::runApp(system.file("proj3", package="rotateS21"),launch.browser = TRUE, quiet = TRUE)
}
