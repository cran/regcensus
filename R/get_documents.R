#' @title get_documents
#' @description
#' Get metadata for documents available in a specific jurisdiction or for a
#' specific document ID
#' @param document_id ID of the specific document
#' @param jurisdiction_id ID for the jurisdiction
#' @param date Year(s) of the documents
#' @param document_type ID for type of document, default value of 1
#' @param verbose prints out the url of the API call, default value of 0
#' @return Returns pandas dataframe with the metadata
#' @examples
#' \dontrun{get_documents(jurisdiction_id = 38, date = 2022)}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_documents <- function(document_id = NULL, jurisdiction_id = NULL,
                          date = NULL, document_type = 1, verbose = 0) {
  if (!is.null(document_id)) {
    text <- paste0("document_id is no longer accessible as of version 1.0.",
                      " Use previous version of API or use jurisdiction_id",
                      " and date combination")
    message(text)
    return()
  } else if (!is.null(jurisdiction_id) && !is.null(date)) {
    return(get_values(series = 1, jurisdiction = jurisdiction_id,
                      year = date,
                      document_type = document_type,
                      summary = FALSE))
  } else {
    message("Must include jurisdiction_id and date")
    return()
  }
}
