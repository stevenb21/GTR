#' Return a univariate plot of each feature in a dataframe.
#'
#' @param df tabular dataframe
#' @param catvec boolean vector of length ncol(df). 1s for categorical features.
#'
#' @return A ggplot of every feature in the dataframe.
#' @export
#'
#' @import dplyr ggplot2 patchwork
#'
#' @examples
#' \dontrun{oneplot(iris,c(0,0,0,0,1))}
#'
oneplot <- function(df, catvec){
  conts <- contsplit(df,catvec)
  cats <- catsplit(df,catvec)
  p <- contplot(conts) / catplot(cats)
  return(p)
}
