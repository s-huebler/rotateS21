#' Canonical Correlation Analysis
#'
#' This function will perform a canonical correlation analysis between the 2 data sets entered as
#' arguments and plot one or all of the observations and the variables in the component space
#' for any of the different pairs of linear combinations.
#'
#' @param df1 , df1 is a data frame with numerical columns
#' @param df2 , df2 is a data frame with numerical columns
#' @param plotType, either "obs" for observation plot or "bi" for plots of the differnt variables from both data sets or "tri" for all scores
#' @param pair, pair tells which pair of linear combinations will make up the axes
#'
#' @return
#' @export
#'
#' @examples
canCorrPlot <- function(df1, df2, plotType="obs", pair=1){
  ccOut <- cc(df1, df2)
  demLC <- ccOut[["xcoef"]]
  severLC <- ccOut[["ycoef"]]
}
