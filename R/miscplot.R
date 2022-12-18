#' Returns plots for feature selection.
#'
#' @param df tabular dataframe
#' @param depvar column name in dataframe of interest
#' @param catvec boolean vector of length ncol(df). 1s for categorical features.
#'
#'
#' @return A pca plot, randomforest importance plot, and an entropy plot.
#' @export
#'
#'
#' @import dplyr ggplot2 entropy randomForest
#' @examples
#' \dontrun{miscplots(iris,"Species",c(0,0,0,0,1))}
#'
miscplots <- function(df,depvar,catvec){
  df <- df %>% drop_na
  return( pca_plot(df) + (rfplot(df,depvar) / entropyplot(df,catvec)))
}
