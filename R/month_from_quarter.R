#' Takes a numeric calendar quarter value to produce the last month in that quarter
#' @name month_from_quarter
#' @importFrom magrittr %>%
#' @param quarter numeric between 1-4 indicating the quarter of a calendar year
#' @title Get the last month in a numeric calendar quarter
#'
month_from_quarter <- function(quarter){
  if(quarter == 1){"mar"
    }else if(quarter == 2){"jun"
    }else if(quarter == 3){"sep"
    }else if(quarter == 4){"dec"
    }else {stop("Quarter is not a numeric value between 1 and 4")
      }

}
