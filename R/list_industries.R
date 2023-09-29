#' @title list_industries
#' @description
#' Fetches the names of industries and associated IDs
#' @param keyword search for keyword in agency name
#' @param label_level NAICS level (2 to 6-digit), default value of 3
#' @param label_source classification standard (NAICS, BEA, SOC),
#'                    default value of "NAICS"
#' @param only_id uses the NAICS code instead of name as key of dictionary,
#'                default value of FALSE
#' @param reverse reverses the key-value mapping, default value of FALSE
#' @return Returns dictionary containing names of industries and associated IDs
#' @examples
#' \dontrun{list_industries(only_id = TRUE, reverse = FALSE)}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
list_industries <- function(keyword = NULL, label_level = 3,
                            label_source = "NAICS",
                            only_id = FALSE, reverse = FALSE) {

  url_call <- industries_url(keyword, label_level, label_source)
  response <- fromJSON(content(GET(url_call), as = "parsed"))

  tryCatch(
    {
      if (only_id == TRUE) {
        if (reverse) {
          response <- response[order(response$label_id), ]
          response <- response[!duplicated(response$label_id), ]
          output <- as.list(setNames(response$label_code, response$label_id))
          return(output)
        } else {
          response <- response[order(response$label_code, response$label_id), ]
          response <- response[!duplicated(response$label_code), ]
          output <- as.list(setNames(response$label_id, response$label_code))
          return(output)
        }
      } else {
        if (reverse) {
          names <- paste0(response$label_name, " (", response$label_code, ")")
          response <- cbind(response, names)
          response <- response[order(response$label_id), ]
          response <- response[!duplicated(response$label_id), ]
          output <- as.list(setNames(response$names, response$label_id))
          return(output)
        } else {
          names <- paste0(response$label_name, " (", response$label_code, ")")
          response <- cbind(response, names)
          response <- response[order(response$names, response$label_id), ]
          response <- response[!duplicated(response$names), ]
          output <- as.list(setNames(response$label_id, response$names))
          return(output)
        }
      }
    },
    error = function() {
      if (reverse) {
        response <- response[order(response$label_id), ]
        response <- response[!duplicated(response$label_id), ]
        output <- as.list(setNames(response$label_name, response$label_id))
        return(output)
      } else {
        response <- response[order(response$label_name, response$label_id), ]
        response <- response[!duplicated(response$label_name), ]
        output <- as.list(setNames(response$label_id, response$label_name))
        return(output)
      }
    }
  )
}
