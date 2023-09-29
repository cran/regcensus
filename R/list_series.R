#' @title list_series
#' @description
#' Fetches the names of series and associated IDs
#' @param reverse reverses the key-value mapping, default value of FALSE
#' @return Returns dictionary containing names of series and associated IDs
#' @examples
#' \dontrun{list_series(reverse = FALSE)}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
list_series <- function(reverse = FALSE) {
  url_call <- series_url()
  response <- fromJSON(content(GET(url_call), as = "parsed"))
  if (reverse) {
    response <- response[order(response$series_id), ]
    response <- response[!duplicated(response$series_id), ]
    return(as.list(setNames(response$series_name, response$series_id)))
  } else {
    response <- response[order(response$series_name, response$series_id), ]
    response <- response[!duplicated(response$series_name), ]
    return(as.list(setNames(response$series_id, response$series_name)))
  }
}
