#' @title Category / Descrete Information
#' @description Gives all information about categorical / descrete variable in data.
#' @details Gives Variable, Level, Frequency, Percentage
#' @author Jitin Kapila
#' @aliases infogen_category
#' @export infogen_category
#' @param data Any Data Set
#' @return Information about Categorical / Descrete Variables
#' @examples
#' data(iris)
#' infogen_category(iris)
#'
#' data(mtcars)
#' info <- infogen_category(mtcars)
#' print(info)

infogen_category <- function(data){

  infoCat <- data.frame(matrix(vector(), 0, 4,
                               dimnames=list(c(),c("Variable","Level","Freq","Prec"))),
                        stringsAsFactors=F)

  for (name in colnames(data)){

    var <- data[,name]

    if (is.character(var)){
      var <- as.factor(var)
    }

    if( is.factor(var)){

      dfc <- data.frame(table(var))
      dfc <- dfc[order(dfc$Freq,decreasing = TRUE),]
      dfc$Prec <- round( (dfc$Freq / sum(dfc$Freq))*100 ,2)
      info <- data.frame(Variable=name,
                         Level=dfc[,"var"],
                         Freq=dfc[,"Freq"],
                         Prec=dfc[,"Prec"])
      infoCat <- rbind(infoCat,info)
    }

  }
  colnames(infoCat) <- c("Variable","Level","Freq","Prec")
  return(infoCat)
}
