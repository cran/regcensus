#' @title get_agencies
#' @description
#' Get metadata for all agencies of a specific jurisdiction
#' @param jurisdiction_id ID for the jurisdiction
#' @param keyword search for keyword in agency name
#' @param verbose prints out the url of the API call, default value of 0
#' @return Returns pandas dataframe with the metadata
#' @examples
#' \dontrun{get_agencies(jurisdiction_id = 38, keyword = "test")}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_agencies <- function(jurisdiction_id = NULL, keyword = NULL, verbose = 0) {
  url_call <- agency_url(jurisdiction_id, keyword)
  if (verbose) {
    message(paste0("API call: ", url_call))
  }
  output <- clean_columns(fromJSON(content(GET(url_call), as = "parsed")))
  return(output)
}
