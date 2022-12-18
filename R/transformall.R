#' Plot transformation of all the features in a dataframe.
#'
#' @param df tabular dataframe
#' @param catvec boolean vector of length ncol(df). 1s for categorical features.
#'
#' @return 6 Plots for each feature: original,qqplot,log,sqrt,impute, and standardized.
#' @export
#'
#' @import dplyr ggplot2 patchwork
#'
#' @examples
#' \dontrun{transformall(iris,c(0,0,0,0,1))}
#'
transformall <- function(df,catvec){
  df <- contsplit(df,catvec)
  m <- ncol(df)
  skews <- order(abs(skewness(df %>% drop_na))) #reorder by skewness magnitude
  df <- df[,skews]
  colname <- colnames(df)[1]
  f <- enquo(colname)
  p <- gghist(df,!! f) +  ggqq(df,!! f) +  ggloghist(df,!! f) + ggsqrthist(df,!! f)  + ggimputehist(df,!! f) + gg01(df,!! f)

  if (m==1){return(p)}
  else{
    for(i in 2:m){
      colname <- colnames(df)[i]
      f <- enquo(colname)
      p <- p + gghist(df,!! f) +  ggqq(df,!! f) +  ggloghist(df,!! f) + ggsqrthist(df,!! f)  + ggimputehist(df,!! f) + gg01(df,!! f)
    }
    return(p)# + plot_layout(ncol=2,widths=20,heights=20))
  }
}
