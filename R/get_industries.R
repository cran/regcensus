#' @title get_industries
#' @description
#' Get metadata for all industries available in a specific jurisdiction
#' @param keyword search for keyword in industry name
#' @param label_level NAICS level (2 to 6-digit), default value of 3
#' @param label_source classification standard (NAICS, BEA, SOC)
#' @param verbose prints out the url of the API call, default value of 0
#' @return Returns pandas dataframe with the metadata
#' @examples
#' \dontrun{get_industries()}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_industries <- function(keyword = NULL, label_level = 3,
                           label_source = NULL, verbose = 0) {
  url_call <- industries_url(keyword, label_level, label_source)
  if (verbose) {
    message(paste0("API call: ", url_call))
  }
  output <- clean_columns(fromJSON(content(GET(url_call), as = "parsed")))
  return(output)
}
