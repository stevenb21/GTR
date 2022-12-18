#' Computes statistical tests of each feature in the dataframe against the dependent variable.
#'
#' @param df tabular dataframe
#' @param depvar column name in dataframe of interest
#' @param catvec boolean vector of length ncol(df). 1s for categorical features.
#'
#'
#' @return A flextable of statistical tests
#' @export
#'
#'
#' @import dplyr nnet flextable
#' @examples
#' \dontrun{compute(iris,"Species",c(0,0,0,0,1))}
#'
compute <- function(df,dv,catvec){
  dv_idx <- which(colnames(df) == dv)
  is_dv_cat <-catvec[dv_idx]
  is_dv_bin <- (length(unique(df[,dv_idx])) == 2)
  if (is_dv_cat){
    cats <- cathelp(df,which(catvec==1))
    conts <- cathelp(df,append(which(catvec==0),dv_idx))
  }else{
    cats <- cathelp(df,append(which(catvec==1),dv_idx))
    conts <- cathelp(df,which(catvec==0))
  }
  catfeats_p_test <- flowchart(cats,dv,1,is_dv_cat,is_dv_bin)
  contfeats_p_test <- flowchart(conts,dv,0,is_dv_cat,is_dv_bin)
  finalout <- as.data.frame(rbind(contfeats_p_test,catfeats_p_test))
  colnames(finalout) <- c("Feature","p.value","Test")
  suppressWarnings(finalout <- finalout %>% mutate(p.value = ifelse(p.value == -1, "<.0001",p.value)))
  suppressWarnings(finalout <- finalout %>% mutate(Feature = ifelse(p.value == "" | p.value == "p.value", as.character(Feature),
                                                                    ifelse(p.value == "<.0001", paste(Feature, "***", sep = ""),
                                                                           ifelse(as.numeric(as.character(p.value)) < 0.001, paste(Feature, "***", sep = ""),
                                                                                  ifelse(as.numeric(as.character(p.value)) < 0.01, paste(Feature, "**", sep = ""),
                                                                                         ifelse(as.numeric(as.character(p.value)) < 0.05, paste(Feature, "*", sep = ""), as.character(Feature))))))))
  return(flextable(finalout))
}
