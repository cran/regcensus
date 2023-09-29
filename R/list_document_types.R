#' @title list_document_types
#' @description
#' Fetches the names of documenttypes and associated IDs
#' @param jurisdiction_id ID for the jurisdiction
#' @param reverse reverses the key-value mapping
#' @param verbose prints out the url of the API call
#' @return Returns dictionary containing names of documenttypes & associated IDs
#' @examples
#' \dontrun{list_document_types(jurisdiction_id = 65,
#'          reverse = FALSE, verbose = 0)}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
list_document_types <- function(jurisdiction_id = NULL,
                                reverse = FALSE, verbose = 0) {
  url <- .url
  if (!is.null(jurisdiction_id)) {
    url_call <- paste0(url, "/documenttypes", "?jurisdiction=", jurisdiction_id)
  } else {
    url_call <- paste0(url, "/documenttypes")
  }
  if (verbose) {
    message(paste0("API call: ", url_call))
  }
  response <- fromJSON(content(GET(url_call), as = "parsed"))
  document_types <- response[!is.na(response$document_type), ]
  if (reverse) {
    document_types <- document_types[order(document_types$document_type_id), ]
    document_types <- document_types[
                      !duplicated(document_types$document_type_id), ]
    return(as.list(setNames(document_types$document_type,
                            document_types$document_type_id)))
  } else {
    document_types <- document_types[order(document_types$document_type), ]
    document_types <- document_types[!duplicated(document_types$document_type,
                                       document_types$document_type_id), ]
    return(as.list(setNames(document_types$document_type_id,
                            document_types$document_type)))
  }
}
