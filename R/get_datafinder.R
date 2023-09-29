#' @title get_datafinder
#' @description
#' Get API info for a specific jurisdiction and documentType
#' @param jurisdiction ID for the jurisdiction
#' @param document_type ID for type of document, default value is NULL
#' @return Returns dataframe with the series and years available,
#'          along with the endpoints to access the data
#' @examples
#' \dontrun{get_datafinder(jurisdiction = 1)}
#' @import httr
#' @import jsonlite
#' @import stats
#' @import stringr
#' @import tidyverse
#' @import utils
#' @export
get_datafinder <- function(jurisdiction, document_type = NULL) {
  url <- .url
  url <- ifelse(is.null(document_type),
    paste0(url, "/datafinder?jurisdiction=", jurisdiction),
    paste0(
      url, "/datafinder?jurisdiction=", jurisdiction,
      "&documenttype=", document_type
    )
  )
  response <- GET(url)
  content <- fromJSON(content(response, as = "parsed")) # will be a dataframe
  output <- clean_columns(content)

  if (length(output)) {
    old_cols <- c("jurisdiction_id", "document_type_id", "series_id")
    new_cols <- c("jurisdiction", "documentType", "series")
    new_output <- output %>%
      dplyr::rename(!!!setNames(old_cols, new_cols))
  } else {
    new_output <- data.frame()
  }
  return(new_output)
}
