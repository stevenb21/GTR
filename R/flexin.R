#' Return a summary table of a dataframe
#'
#' @param df tabular dataframe
#' @param catvec boolean vector of length ncol(df). 1s for categorical features.
#'
#' @return flextable
#' @export
#'
#' @import dplyr flextable moments
#'
#' @examples
#' \dontrun{flexin(iris,c(0,0,0,0,1))}
#'
flexin <- function(df,catvec){
  m <- ncol(df) + 1
  summary <- mysummarydf(df,catvec)
  flex <- flextable(summary)
  na_count <- sum(as.numeric(summary['NA..6',2:m]))
  flex <- add_header_row(flex,
                         values=c(paste0("Row count: ",nrow(df)),
                                  paste0("Col count: ",ncol(df))),
                         colwidths=c(as.integer(m/2), m-as.integer(m/2)))
  flex <- add_footer_row(flex,
                         values=c(paste0("Total NAs: ",na_count)),
                         colwidths=m)
  return(flex)
}
