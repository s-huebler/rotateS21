#' Visual Profile Analysis
#'
#' This function will create a graph that compares the levels of a single categorical predictor variable against 3 different
#' categorical response variables. The x axis will be the categorical variable. The y axis will the proportion of each level of the
#' predictor variable that displays the response variable.
#'
#' @param df , df, a data frame with at least 4 categorical columns
#' @param x1 , x, index of a binary (0,1) column in the data frame to use as the predictor variable
#' @param y1 , y1, index of a binary (0,1) column in the data frame to use as the first response variable
#' @param y2 , y2, index of a binary (0,1) column in the data frame to use as the second response variable
#' @param y3 , y3, index of a binary (0,1) column in the data frame to use as the third response variable
#'
#' @return
#' @export
#'
#' @examples
#' dataframe <- rotateS21::covidWrangled
#' profiles(dataframe, 3, 17, 18, 19)
profiles <- function(df, x1, y1, y2, y3){


  data <- data.frame("x1"=df[,x1], "y1"=df[,y1], "y2"=df[,y2], "y3"=df[,y3])
  names(data)<-c("x1", "y1", "y2", "y3")
  proportionDF <- dplyr::summarise(
    dplyr::group_by(data, x1),
    tally1 = sum(y1 ==1),
    tally2 = sum(y2 ==1),
    tally3 = sum(y3 ==1),
    n = dplyr::n(),
    prop1 = tally1/n,
    prop2 = tally2/n,
    prop3 = tally3/n
  )


  p <- ggplot2::ggplot(proportionDF)+
    ggplot2::geom_point(ggplot2::aes(x=x1, y=prop1, color="1"))+
    ggplot2::geom_path(ggplot2::aes(x=x1, y=prop1, group="all", color="1"))+
    ggplot2::geom_point(ggplot2::aes(x=x1, y=prop2, color="2"))+
    ggplot2::geom_path(ggplot2::aes(x=x1, y=prop2, group="all", color="2"))+
    ggplot2::geom_point(ggplot2::aes(x=x1, y=prop3, color="3"))+
    ggplot2::geom_path(ggplot2::aes(x=x1, y=prop3, group="all", color="3"))+
    ggplot2::theme_classic()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(vjust=0))+
    ggplot2::ylab("Observed Proportion")
  return(p)

}
