#' @title clean_columns
#' @description
#' Removes prefixes from column names
#' @param df Uncleaned dataframe as input
#' @return Cleaned Dataframe
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
clean_columns <- function(df) {
  pattern <- "\\v_" # column names would start with v_
  names(df) <- sub(pattern, "", names(df))
  return(df)
}
