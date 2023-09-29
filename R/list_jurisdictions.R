#' @title list_jurisdictions
#' @description
#' Fetches names of jurisdictions and associated IDs
#' @param reverse reverses the key-value mapping, default value of FALSE
#' @return Returns dictionary containing names of jurisdictions & associated IDs
#' @examples
#' \dontrun{list_jurisdictions(reverse = TRUE)}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
list_jurisdictions <- function(reverse = FALSE) {
  url_call <- jurisdictions_url()
  response <- fromJSON(content(GET(url_call), as = "parsed"))
  if (reverse) {
    response <- response[order(response$jurisdiction_id), ]
    response <- response[!duplicated(response$jurisdiction_id), ]
    output <- as.list(setNames(response$jurisdiction_name,
                               response$jurisdiction_id))
    return(output)
  } else {
    response <- response[order(response$jurisdiction_name,
                               response$jurisdiction_id), ]
    response <- response[!duplicated(response$jurisdiction_name), ]
    output <- as.list(setNames(response$jurisdiction_id,
                               response$jurisdiction_name))
    return(output)
  }
}
