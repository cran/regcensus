#' @title industries_url
#' @description
#' Gets url call for label (formerly industries) endpoint.
#' @param keyword search for keyword in industry name
#' @param label_level NAICS level (2 to 6-digit)
#' @param label_source classification standard (NAICS, BEA, SOC)
#' @return url as a string
#' @examples
#' \dontrun{industries_url("test", 323, "NAICS")}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export

industries_url <- function(keyword, label_level, label_source) {
  url <- .url
  if (!is.null(keyword)) {
    url_call <- paste0(url, "/labels?", "labellevel=", label_level,
                       "&keyword=", keyword)
  } else {
    url_call <- paste0(url, "/labels?", "labellevel=", label_level)
  }
  if (!is.null(label_source)) {
    url_call <- paste0(url_call, "&labelsource=", label_source)
  }
  return(url_call)
}
