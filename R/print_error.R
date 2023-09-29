#' @title print_error
#' @description
#' Handle and print out error for invalid API call.
#' @param output key-value mapping of the output
#' @return None
#' @examples
#' \dontrun{print_error(list(message = "test"))}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
print_error <- function(output) {
  tryCatch(
    {
      message(paste0("ERROR: ", output$message))
    },
    error = function() {
      message(paste0("ERROR: ", output$errorMessage))
    }
  )
  return()
}
