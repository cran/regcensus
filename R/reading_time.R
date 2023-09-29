#' @title reading_time
#' @description
#' Computes the time it takes to read a document based on the words a document
#' has.
#' @param words count of words
#' @param workday default value of 8 hour workday
#' @param workweek default value of 5 day work week
#' @param workyear default value of 50 week work year
#' @return A string detailing how long it takes to read a document based on
#' how many words the document has. The function assumes an 8 hour work-day,
#' a 5 day work-week, and a 50 week work-year.
#' @examples
#' \dontrun{reading_time(1200000, workday = 8,
#'          workweek = 5, workyear = 50)}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
#' @title reading_time
#' @description
#' Computes the time it takes to read a document based on the words a
#' document has.
#' @param words count of words
#' @param workday default value of 8 hour workday
#' @param workweek default value of 5 day work week
#' @param workyear default value of 50 week work year
#' @return A string detailing how long it takes to read a document based
#' on how many words the document has. The function assumes an 8 hour work-day,
#' a 5 day work-week, and a 50 week work-year.
#' @examples
#' reading_time(1200000, workday=8, workweek=5, workyear=50)
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
#nolint start
reading_time <- function(words, workday = 8, workweek = 5, workyear = 50) {
  text <- ""
  years <- words / 36000000
  weeks <- (years - as.integer(years)) * workyear
  days <- (weeks - as.integer(weeks)) * workweek
  hours <- (days - as.integer(days)) * workday
  minutes <- (hours - as.integer(hours)) * 60

  if (as.integer(years) > 0) {
    text <- paste0(text, as.integer(years), " year, ")
    if (as.integer(years) > 1) {
      text <- gsub("year", "years", text)
    }
  }

  if (as.integer(weeks) > 0) {
    text <- paste0(text, as.integer(weeks), " week, ")
    if (as.integer(weeks) > 1) {
      text <- gsub("week", "weeks", text)
    }
  }

  if (as.integer(days) > 0) {
    text <- paste0(text, as.integer(days), " day, ")
    if (as.integer(days) > 1) {
      text <- gsub("day", "days", text)
    }
  }

  if (as.integer(hours) > 0 && as.integer(years) == 0) {
    text <- paste0(text, as.integer(hours), " hour, ")
    if (as.integer(hours) > 1) {
      text <- gsub("hour", "hours", text)
    }
  }

  if (as.integer(minutes) > 0 && as.integer(years) == 0
      && as.integer(weeks) == 0) {
    text <- paste0(text, as.integer(minutes), " minute")
    if (as.integer(minutes) > 1) {
      text <- gsub("minute", "minutes", text)
    }
  }

  if (text != "") {
    return(trimws(text, "both", ", "))
  } else {
    return("Less than a minute")
  }
}
#nolint end
