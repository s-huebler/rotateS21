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
#' @param labels, label all names of variables in df1 and df2 in order
#'
#' @return
#' @export
#'
#' @examples
#' data <- rotateS21::covidWrangled
#' demographics <- data[,9:15]
#' severity <- data[,16:19]
#' lab <- c("Male", "Asian", "Black", "MultiRace", "NativePI", "White", "Ethnicity", "Symptomatic", "Hospital", "ICU", "Death")
#' canCorrPlot(demographics,severity, labels=lab)
canCorrPlot <- function(df1, df2, plotType="obs", pair=1, labels){

  ccaList <- CCA::cc(df1,df2)
  obsScores <- data.frame("x1"=ccaList[["scores"]][["xscores"]][,pair],
                          "y1"=ccaList[["scores"]][["yscores"]][,pair])
  demVarScores <- data.frame("Demographics"=ccaList[["scores"]][["corr.X.xscores"]][,pair],
                             "Severity"=ccaList[["scores"]][["corr.X.yscores"]][,pair],
                             "Type"="Dem")
  sevVarScores <- data.frame("Demographics"=ccaList[["scores"]][["corr.Y.xscores"]][,pair],
                             "Severity"=ccaList[["scores"]][["corr.Y.yscores"]][,pair],
                             "Type"="Sev")
  varScores <- dplyr::full_join(demVarScores,sevVarScores)


  if(plotType=="obs"){
    p <- ggplot2::ggplot(obsScores, ggplot2::aes(x=x1, y=y1))+
      ggplot2::geom_point()+
      ggplot2::theme_classic()

    }

  if(plotType=="bi"){

    p <- ggplot2::ggplot()+
      ggplot2::geom_segment(data=varScores, ggplot2::aes(x=0, xend=Demographics, y=0, yend=Severity, color=Type),
                                arrow = ggplot2::arrow(length = unit(0.5, "cm")))+
      ggrepel::geom_text_repel(data=varScores, ggplot2::aes(x=Demographics, y=Severity, label=labels), size=5)+
      ggplot2::theme_classic()

  }

  if(plotType=="tri"){
    p <- ggplot2::ggplot(obsScores, ggplot2::aes(x=x1, y=y1), alpha=0.5)+
      ggplot2::geom_point()+
      ggplot2::geom_segment(data=varScores, ggplot2::aes(x=0, xend=Demographics, y=0, yend=Severity, color=Type),
                            arrow = ggplot2::arrow(length = unit(0.5, "cm")))+
      ggrepel::geom_text_repel(data=varScores, ggplot2::aes(x=Demographics, y=Severity, label=labels), size=5)+
      ggplot2::theme_classic()
  }

  p
}
