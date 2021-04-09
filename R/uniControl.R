#' Univariate Control Charts
#'
#' This function will produce univariate control charts to visually assess which
#' observations are out of control with regards to a single variable. The variable
#' being assessed for control is plotted on the y-axis for each observation. The ordering
#' of the x axis comes from time series ordering. The upper and lower control limits
#' (percent defined by the alpha argument) is also charted. Points that fall above or
#' below the control limit lines are considered out of control
#'
#' @param df , a data frame with continuous numerical columns
#'
#' @return
#' @export
#'
#' @examples
#' df <- iris
#' uniControl(df[,c(2,3)])
#'
uniControl <- function(df){

  n <- nrow(df)
  p <- ncol(df)

  df <- cbind("obs"=1:n, df)
  varNames <- names(df)[-1]
  #print(varNames)


  gridExtra::grid.arrange(
  gridExtra::arrangeGrob( grobs=
  purrr::map(names(df)[-1],
      function(i){
        #print(i)
        var <- df[,i]

        std <- stats::sd(var)
        ave <- mean(var)

        ucl <- ave + 3*std
        lcl <- ave - 3*std

        p <- ggplot2::ggplot(data=df, ggplot2::aes(x=obs, y=var))+
          ggplot2::geom_point()+
          ggplot2::geom_hline(yintercept = ucl, lty=2, color="red")+
          ggplot2::geom_hline(yintercept = lcl, lty=2, color="red")+
          ggplot2::geom_path()+
          ggplot2::xlab("Observation Number")+
          ggplot2::ylab(i)+
          ggplot2::theme_classic()
        return(p)
      })
  ))

}
