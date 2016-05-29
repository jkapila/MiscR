#' @title Remove Outlier
#' @description Remove Outlier Rows
#' @details Gives Data Set with reduced no of rows based on simple criteria that
#' outliers will have either highest or lowest percentile. It first calculates
#' all the percentile for each variable and then based on threshold, remove the
#' outliers.
#' @author Jitin Kapila
#' @aliases remove_outlier
#' @export remove_outlier
#' @param data Any Data Set
#' @param max_perc Maximum percentile limit for considering as outlier, default is 0.99
#' @param min_perc Minimum percentile limit for considering as outlier, default is 0.91
#' @param threshold No of Variable that should occur simultaneoulsy as outliers, default is 5
#' @param col_name Vector with column names to be considered for outlier, default is "" and considers all variable for outlier
#' @param na.rm logical, Remove NA values while calculating percentile, default is TRUE
#' @param ... Other Parameters for quantile function
#' @return Data set with no set of outliers based on percentile
#' @examples
#' data(mtcars)
#' mtcars_1 <- remove_outlier(iris,threshold = 1)
#' mtcars_2 <- remove_outlier(iris,threshold = 2)
#' mtcars_3 <- remove_outlier(iris,threshold = 3)
#' # check the no of rows removed and at various thresholds
#' dim(mtcars)
#' dim(mtcars_1)
#' dim(mtcars_2)
#' dim(mtcars_3)


remove_outlier <- function(data,max_perc=0.99,min_perc=0.91,threshold = 5,
                           col_name="",na.rm=TRUE,...) {
  max_prob <- max_perc
  min_prob <- min_perc
  data_ <- data.frame(row.names = 1:nrow(data))
  if (is.vector(col_name) & nchar(col_name) != 0){
    for (col_ in col_name){
      if(is.numeric(data[,col_])){
        vec_ <- ifelse((data[,col_] > quantile(data[,col_],
                                               probs=c(.01, max_prob),
                                               na.rm = na.rm,...)[2] |
                          data[,col_] < quantile(data[,col_],
                                                 probs=c(.01, min_prob),
                                                 na.rm = na.rm,...)[1]),1,0)

        data_[,col_] <- vec_
      }

    }
  }else{
    for (col_ in colnames(data)){
      if(is.numeric(data[,col_])){
        vec_ <- ifelse((data[,col_] > quantile(data[,col_],
                                               probs=c(.01, max_prob),
                                               na.rm = na.rm,...)[2] |
                          data[,col_] < quantile(data[,col_],
                                                 probs=c(.01, min_prob),
                                                 na.rm = na.rm,...)[1]),1,0)

        data_[,col_] <- vec_
      }
    }
  }
  data_$row_sums <- rowSums(data_)

  return(data[which(data_$row_sums < threshold),])
}
