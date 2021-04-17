#' Interactive Multivariate Normality Check
#'
#'This function launches an app that allows the user to interact with the data directly
#'while performing normality checks.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{MVnorm()}
#'
MVnorm<-function(){
  shiny::runApp(system.file("norm", package="MVnormS21"), launch.browser = TRUE)
}
