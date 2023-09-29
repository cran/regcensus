#' @title get_series
#' @description
#' Get series metadata for all or one specific jurisdiction
#' @param verbose prints out the url of the API call, default value of 0
#' @return Returns pandas dataframe with the metadata
#' @examples
#' \dontrun{get_series()}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_series <- function(verbose = 0) {
  url_call <- series_url()
  if (verbose) {
    message(paste0("API call: ", url_call))
  }
  output <- clean_columns(fromJSON(content(GET(url_call), as = "parsed")))
  return(output)
}
