#' @title series_url
#' @description
#' Gets url call for dataseries endpoint
#' @return url as a string
#' @examples
#' \dontrun{series_url()}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
series_url <- function() {
  url <- .url
  return(paste0(url, "/dataseries"))
}
