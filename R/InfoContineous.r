#' @title Continuous Information
#' @description Gives all information about continuous variable in data.
#' @details Gives Max., Min., Mean. Median, Std. Div. Counts, Unique Counts,
#' NA's, And Quantile
#' @author Jitin Kapila
#' @import stats
#' @aliases infogen_continuous
#' @export infogen_continuous
#' @param data Any Data Set
#' @return Information about continuous Variables
#' @examples
#' data(iris)
#' infogen_continuous(iris)
#'
#' data(mtcars)
#' info <- infogen_continuous(mtcars)
#' print(info)

infogen_continuous <- function(data){

  infoCont <- data.frame(matrix(vector(), 0, 15,
                                dimnames=list(c(),
                                              c("Variable","Counts","Uniques",
                                                "NAs","Min","Max","Mean","Median",
                                                "0%","25%","50%","75%","100%",
                                                "SD","Var"))),
                         stringsAsFactors=F)

  for (name in colnames(data)){

    var_ <- data[,name]

    if (is.numeric(var_)){
      sum_na <- sum(is.na(var_))
      count_tot <- (NROW(var_)-sum_na)
      info <- c(Variable=var_,
                count_tot,
                length(unique(var_)),
                sum_na,
                min(var_,na.rm = TRUE),
                max(var_,na.rm = TRUE),
                mean(var_,na.rm = TRUE),
                median(var_,na.rm = TRUE),
                quantile(var_,na.rm = TRUE),
                sd(var_,na.rm = TRUE),
                var(var_,na.rm = TRUE))
      info <- t(data.frame(info))
      rownames(info) <- name
      infoCont <- rbind(infoCont,info)

    }
  }
  colnames(infoCont) <-  c("Variable","Counts","Uniques","NAs",
                           "Min","Max","Mean","Median",
                           "0%","25%","50%","75%","100%",
                           "SD","Var")
  # rownames(infoCont) <- nrow(infoCont)
  return(infoCont)
}