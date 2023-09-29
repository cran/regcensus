#' @title jurisdictions_url
#' @description
#' Gets url call for jurisdictions endpoint.
#' @return url as a string
#' @examples
#' \dontrun{jurisdictions_url()}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
jurisdictions_url <- function() {
  url <- .url
  url_call <- paste0(url, "/jurisdictions/")
  return(url_call)
}
