#' @title list_dates
#' @description
#' Fetches the dates available for the jurisdiction
#' @param jurisdiction_id ID for the jurisdiction
#' @param document_type ID for type of document
#' @return Returns list of dates available for the jurisdiction
#' @examples
#' \dontrun{list_dates(jurisdiction_id = 38)}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
list_dates <- function(jurisdiction_id, document_type = NULL) {
  return(as.vector(sort(unique(get_datafinder(jurisdiction_id,
                                              document_type)$year))))
}
