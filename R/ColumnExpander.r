#' @title Column Expander
#' @description Expands Values to Columns
#' @details Gives Data Set with levels / values as column variable with vales
#' 0 and 1. 1's are the value where that value occurs in the variable rest all
#' are zero
#' @author Jitin Kapila
#' @aliases column_expander
#' @export column_expander
#' @param data Any Data Set
#' @param name Name of the Column Variable whose value needs to be expanded
#' @return Data set with additional columns with vules 0 and 1
#' @examples
#' data(iris)
#' column_expander(iris,"Species")
#'


column_expander <- function(data,name){
  data_ <- data.frame(data[,name])
  colnames(data_) <- name
  data[,name] <- NULL
  for(t in unique(data_[,name])) {
    new_col <- paste(name,t,sep="_")
    data_[,new_col] <- ifelse(data_[,name]==t,1,0)
  }
  # print(data_[,new_col])
  data <- cbind(data,data_)
  return(data)
}
