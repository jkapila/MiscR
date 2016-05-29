#' @title Selective Factoring
#' @description Does selective factoring on Data
#' @details Gives uniquely identified variable as factors
#' @author Jitin Kapila
#' @aliases selective_factoring
#' @export selective_factoring
#' @param data Any Data Set
#' @param col_name Vector with column names to be considered for outlier,
#' default is "" and considers all variable for Selective Factoring
#' @param numeric_factors No of Unique Numeric values to be considered as
#' factors, default is 2
#' @param text_factors_char Length of String to be considered as factors,
#' default is 64
#' @return Data set with Columns factored based on criteria defined
#' @examples
#' data(mtcars)
#' factored_iris <- selective_factoring(iris)
#' str(iris)
#' str(factored_iris)
#'


is_elegible_for_factor <- function(data,numeric_factors = 5,
                                   text_factors_char = 128){
  if(is.numeric(data)){
    if (length(unique(data)) < numeric_factors){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }else if(is.logical(data)){
    return(FALSE)
  }else if(is.integer(data)){
    return(FALSE)
  }else if(is.factor(data)){
    return(TRUE)
  }else if(is.character(data)){
    if (any(nchar(data) < text_factors_char)){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }else{
    return(FALSE)
  }

}

selective_factoring <- function(data,col_name="",numeric_factors = 2,
                                text_factors_char = 64){
  if (dim(data)[2]==1 & nchar(col_name) == 0){
    if (is_elegible_for_factor(data = data,
                               numeric_factors = numeric_factors,
                               text_factors_char = text_factors_char)){
      data <- as.factor(data)
    }else{
      print(paste(col_,"is Text Column"))
    }
  }else if (nchar(col_name)>0 & !is.vector(col_name)){
    # data_ <- data.frame(data[,col_name])
    # colnames(data_) <- col_name
    # data[,col_name] <- NULL
    # apply(data_,2,function(x) nchar(x) > 128)
    if (is_elegible_for_factor(data[,col_])){
      data[,col_] <- as.factor(data[,col_])
    }else{
      print(paste(col_,"is Text Column"))
    }
  }else if (is.vector(col_name)){
    for( col_ in col_name){
      if (is_elegible_for_factor(data = data[,col_],
                                 numeric_factors = numeric_factors,
                                 text_factors_char = text_factors_char)){

        data[,col_] <- as.factor(data[,col_])

      }else{
        print(paste(col_,"is Text Column"))
      }
    }
  }else{
    for ( col_ in colnames(data)){
      if (is_elegible_for_factor(data = data[,col_],
                                 numeric_factors = numeric_factors,
                                 text_factors_char = text_factors_char)){
        if (any(nchar(data[,col_]) < 128)){
          data[,col_] <- as.factor(data[,col_])
        }else{
          print(paste(col_,"is Text Column"))
        }
      }
    }
  }
  return(data)
}

