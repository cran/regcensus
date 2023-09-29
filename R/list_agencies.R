#' @title list_agencies
#' @description
#' Fetches the names of the agencies and their associated IDs
#' @param jurisdiction_id ID for the jurisdiction
#' @param keyword search for keyword in agency name
#' @param reverse reverses the key-value mapping, default value of FALSE
#' @return Returns dictionary containing names of agencies and associated IDs
#' @examples
#' \dontrun{list_agencies(jurisdiction_id = 38)}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
list_agencies <- function(jurisdiction_id = NULL,
                          keyword = NULL, reverse = FALSE) {
  url_call <- agency_url(jurisdiction_id, keyword)
  if (is.null(url_call)) {
    return()
  }
  response <- fromJSON(content(GET(url_call), as = "parsed"))

  # calling get_jurisdiction function & creating id-name mapping
  jurisdictions_df <- get_jurisdictions()
  jurisdiction_id_name <- as.list(setNames(jurisdictions_df$jurisdiction_name,
                                           jurisdictions_df$jurisdiction_id))

  # Add jurisdiction name to key if keyword is used
  if (reverse) {
    if (!is.null(keyword)) {
      # array of jurisdiction names
      jurisdiction_names <- c()
      for (i in response$a_jurisdiction_id) {
        jurisdiction_names <- c(jurisdiction_names,
                                jurisdiction_id_name[[as.character((i))]])
      }

      # merge with the dataframe
      response <- cbind(response, jurisdiction_names)

      # now merge it with agency names
      names <- paste0(response$agency_name, " (",
                      response$jurisdiction_names, ")")
      response <- cbind(response, names)
      response <- response[order(response$agency_id), ]
      response <- response[!duplicated(response$agency_id), ]
      return(as.list(setNames(response$names, response$agency_id)))
    } else {
      response <- response[order(response$agency_id), ]
      response <- response[!duplicated(response$agency_id), ]
      return(as.list(setNames(response$agency_name, response$agency_id)))
    }
  } else {
    response <- response[order(response$agency_name, response$agency_id), ]
    response <- response[!duplicated(response$agency_name), ]
    return(as.list(setNames(response$agency_id, response$agency_name)))
  }
}
