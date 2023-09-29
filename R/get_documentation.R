#' @title get_documentation
#' @description
#' Get documentation for projects, including citations.
#' @return Returns a dataframe with information on the source of the
#'          documentation, the source name, source citation,
#'          source url and documentation.
#' @examples
#' \dontrun{get_documentation()}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_documentation <- function() {
  url <- .url
  url_call <- paste0(url, "/documentation")
  output <- clean_columns(fromJSON(content(GET(url_call), as = "parsed")))
  return(output)
}
