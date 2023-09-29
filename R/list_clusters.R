#' @title list_clusters
#' @description
#' Fetches the names of the clusters and their associated IDs
#' @param reverse reverses the key-value mapping, default value of FALSE
#' @return Returns dictionary containing names of clusters & associated IDs
#' @examples
#' \dontrun{list_clusters()}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
list_clusters <- function(reverse = FALSE) {
  url <- .url
  url_call <- paste0(url, "/clusters")
  response <- fromJSON(content(GET(url_call), as = "parsed"))
  if (reverse) {
    response <- response[order(response$agency_cluster), ]
    response <- response[!duplicated(response$agency_cluster), ]
    output <- as.list(setNames(response$cluster_name, response$agency_cluster))
    return(output)
  } else {
    response <- response[order(response$cluster_name,
                               response$agency_cluster), ]
    response <- response[!duplicated(response$cluster_name), ]
    output <- as.list(setNames(response$agency_cluster, response$cluster_name))
    return(output)
  }
}
