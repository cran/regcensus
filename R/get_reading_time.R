#' @title get_reading_time
#' @description
#' Convert word counts to total reading time
#' @param jurisdiction ID for the jurisdiction
#' @param series Series ID (s), default value is 2 here
#' @param year Year(s) of data
#' @param document_type ID for type of document, default value of 1
#' @param summary Return summary instead of document level data
#'                (only one year of data is allowed for document level data),
#'                default value is TRUE
#' @param date_is_range Indicating whether the time parameter is range or
#'                    should be treated as single data points,
#'                    default value is TRUE
#' @param country Get values for all subjurisdictions, default value is FALSE
#' @param agency Agency ID, default value is NULL
#' @param cluster Cluster ID
#' @param label Industry code using the jurisdiction-specific coding system
#'              (returns all 3-digit industries by default),
#'              default value is NULL
#' @param industry industry is deprecated; use label
#' @param filtered Exclude poorly-performing industry results
#'                  (use of unfiltered results is NOT recommended),
#'                  default value is TRUE
#' @param label_level Level of NAICS industries to include, default value is 3
#' @param industry_level industry_level is deprecated; use label_level
#' @param label_source classification standard (NAICS, BEA, SOC),
#'                    default value of "NAICS"
#' @param version Version ID for datasets with multiple versions
#'                (if no ID is given, returns most recent version),
#'                  default value is NULL
#' @param download If not False, a path location for a downloaded csv
#'                  of the results, default value is FALSE
#' @param page Page Number of the Response, default value is NULL
#' @param date date is deprecated, use year now
#' @param verbose Print out the url of the API call (useful for debugging),
#'                default value is 0
#' @return Returns pandas dataframe with the metadata
#' @examples
#' \dontrun{get_reading_time(
#'   jurisdiction = 45,
#'   year = array(c(2022, 2023))
#' )}
#' @import jsonlite
#' @import stringr
#' @import httr
#' @import tidyverse
#' @import stats
#' @import utils
#' @export
get_reading_time <- function(series = 2, jurisdiction, year, document_type = 1,
                             summary = TRUE, date_is_range = TRUE,
                             country = FALSE, agency = NULL, cluster = NULL,
                             label = NULL, industry = NULL, filtered = TRUE,
                             label_level = 3, industry_level = NULL,
                             label_source = "NAICS", version = NULL,
                             download = FALSE, page = NULL, date = NULL,
                             verbose = 0) {
  #for demo example
  if (jurisdiction == 0) {
    return(NULL)
  }
  results <- get_values(
    series, jurisdiction, year, document_type, summary, date_is_range,
    country, agency, cluster, label, industry, filtered, label_level,
    industry_level, label_source, version, download, page, date, verbose
  )
  if (is.null(results)) {
    return(NULL)
  }
  tryCatch(
    {
      results$series_name <- "Reading Time"
      results$series_value <- sapply(results$series_value, reading_time)
      results$footNote <- paste0("Reading time calculation assumes",
                                 " an 8 hour work-day, a 5 day work-week, ",
                                 "and a 50 week work-year.")
    },
    error = function() {
    }
  )
  return(results)
}
