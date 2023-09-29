#' @title agency_url
#' @description
#' Gets url call for agencies endpoint.
#' @param jurisdiction_id ID for the jurisdiction
#' @param keyword search for keyword in agency name
#' @return url as a string
#' @examples
#' \dontrun{agency_url(jurisdiction_id = 38, keyword = "test_word")}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
agency_url <- function(jurisdiction_id, keyword) {
  url <- .url
  if (!is.null(keyword)) {
    url_call <- paste0(url, "/agencies-keyword?", "keyword=", keyword)
  } else if (!is.null(jurisdiction_id)) {
    url_call <- paste0(url, "/agencies?", "jurisdiction=", jurisdiction_id)
  } else {
    message("Must include either 'jurisdiction_id' or 'keyword.'")
    return()
  }
  return(url_call)
}
