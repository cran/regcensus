# reading_time function
expect_equal(
  reading_time(1200000, workday = 8, workweek = 5, workyear = 50),
  "1 week, 3 days, 2 hours"
)

# industries_url function
actual_industries_url <- paste0("https://64gzqlrrd2.execute-api.us-east-1",
                                ".amazonaws.com", "/dev/labels?",
                                "labellevel=3&keyword=test_word&",
                                "labelsource=test_source")
expect_equal(
  industries_url("test_word", 3, "test_source"), actual_industries_url)


# jurisdiction_url function
actual_jurisdiction_url <- paste0("https://64gzqlrrd2.execute-api.us-east-1",
                          ".amazonaws", ".com/", "dev/jurisdictions/")
expect_equal(
  jurisdictions_url(), actual_jurisdiction_url)


# agency_url function
actual_agency_url <- paste0("https://64gzqlrrd2.execute-api.us-east-1",
                            ".amazonaws.com/dev/",
                            "agencies-keyword?keyword=test_word")
expect_equal(
  agency_url(jurisdiction_id = 38, keyword = "test_word"),
            actual_agency_url)


# series_url function
actual_series_url <- paste0("https://64gzqlrrd2.execute-api.us-east-1",
                            ".amazonaws.com/dev/dataseries")
expect_equal(
  series_url(), actual_series_url
)


# list_industries function
expect_equal(
  class(list_industries(only_id = TRUE, reverse = TRUE)),
  "list"
)

expect_equal(
  class(list_industries(only_id = TRUE, reverse = FALSE)),
  "list"
)

expect_equal(
  class(list_industries(only_id = FALSE, reverse = FALSE)),
  "list"
)

expect_equal(
  class(list_industries(only_id = FALSE, reverse = TRUE)),
  "list"
)

expect_equal(
  list_industries(only_id = TRUE, reverse = TRUE)[["111"]],
  "928"
)

expect_equal(
  list_industries(only_id = TRUE, reverse = FALSE)[["713"]],
  101
)

expect_equal(
  list_industries(only_id = FALSE, reverse = TRUE)[["111"]],
  "National Security and International Affairs (928)"
)

expect_equal(
  list_industries(
    only_id = FALSE,
    reverse = FALSE
  )[["Water Transportation (483)"]],
  70
)


# list_jurisdiction function
expect_equal(
  class(list_jurisdictions()),
  "list"
)

expect_equal(
  class(list_jurisdictions(reverse = TRUE)),
  "list"
)

expect_equal(
  list_jurisdictions(reverse = TRUE)[["87"]],
  "Ontario"
)

expect_equal(
  list_jurisdictions()[["Vermont"]],
  97
)


# list_clusters function
expect_equal(
  class(list_clusters(reverse = TRUE)),
  "list"
)

expect_equal(
  class(list_clusters(reverse = FALSE)),
  "list"
)

expect_equal(
  list_clusters(reverse = TRUE)[["1"]],
  "Primary and Secondary Education"
)

expect_equal(
  list_clusters(reverse = FALSE)[["Social Assistance"]],
  12
)


# list_agencies function
expect_equal(
  class(list_agencies(jurisdiction_id = 15, reverse = TRUE)), "list")

expect_equal(
  list_agencies(jurisdiction_id = 15, reverse = TRUE)[["378"]],
  "the public school retirement system of missouri"
)

expect_equal(
  class(list_agencies(jurisdiction_id = 15, reverse = FALSE)),
  "list"
)

expect_equal(
  list_agencies(
    jurisdiction_id = 15,
    reverse = FALSE
  )[["well installation"]],
  326
)

expect_equal(
  class(list_agencies(
    jurisdiction_id = 15,
    reverse = TRUE, keyword = "a"
  )),
  "list"
)

expect_equal(
  list_agencies(
    jurisdiction_id = 15,
    reverse = TRUE, keyword = "a"
  )[["1465"]],
  "division of historical and cultural affairs (Delaware)"
)

expect_equal(list_agencies(), NULL)


# list_dates function
expect_equal(
  class(list_dates(jurisdiction_id = 38)),
  "integer"
)

expect_equal(
  list_dates(jurisdiction_id = 38)[5],
  1974
)


# list_series function
expect_equal(class(list_series(reverse = FALSE)), "list")

expect_equal(
  list_series(reverse = FALSE)[["2 Digit Probability Industry (NAICS 2007)"]],
  36
)

expect_equal(class(list_series(reverse = TRUE)), "list")

expect_equal(
  list_series(reverse = TRUE)[[1]],
  "Restrictions"
)


# list_document_types function
expect_equal(
  class(list_document_types(jurisdiction_id = 38, reverse = FALSE)),
  "list"
)

expect_equal(list_document_types(
  jurisdiction_id = 38,
  reverse = FALSE
)[["Executive orders"]], 15)

expect_equal(
  class(list_document_types(jurisdiction_id = 38, reverse = TRUE)),
  "list"
)

expect_equal(
  list_document_types(jurisdiction_id = 38, reverse = TRUE)[["7"]],
  "Jurisdiction text Propagation of subjurisdiction regulations"
)

expect_equal(
  class(list_document_types(reverse = TRUE)),
  "list"
)

expect_equal(
  list_document_types(reverse = TRUE)[["7"]],
  "Jurisdiction text Propagation of subjurisdiction regulations"
)

expect_equal(
  list_document_types(reverse = FALSE)[["Executive orders"]],
  15
)


# get_documentation function
expect_equal(class(get_documentation()), "data.frame")
expect_equal(colnames(get_documentation())[1], "documentation_id")


# get_documents function
expect_equal(class(get_documents(jurisdiction_id = 65, date = 2022)),
            "data.frame")


# get_industries function
expect_equal(class(get_industries()), "data.frame")
expect_equal(colnames(get_industries())[1], "label_id")


# get_jurisdictions function
expect_equal(class(get_jurisdictions()), "data.frame")
expect_equal(colnames(get_jurisdictions())[1], "jurisdiction_id")


# get_agencies function
expect_equal(class(get_agencies(jurisdiction_id = 38)), "data.frame")
expect_equal(colnames(get_agencies(jurisdiction_id = 38))[1], "agency_id")


# get_series function
expect_equal(class(get_series()), "data.frame")
expect_equal(colnames(get_series())[1], "series_id")


# get_datafinder function
expect_equal(class(get_datafinder(jurisdiction = 38)), "data.frame")
expect_equal(colnames(get_datafinder(jurisdiction = 38))[1], "jurisdiction")


# get_endpoint function
expect_equal(get_endpoint(
  series = c(28, 33, 36), jurisdiction = 38,
  year = c(1970, 2003, 2004, 2018),
  document_type = 1
), "/usregdata-summary")


# get_values function
expect_equal(
  class(get_values(
    series = array(c(1, 28, 33, 36)),
    jurisdiction = 38,
    year = array(c(1970, 2003, 2004, 2018, 2020))
  )),
  "data.frame"
)

expect_equal(colnames(get_values(
  series = array(c(1, 28, 33, 36)),
  jurisdiction = 38,
  year = array(c(1970, 2003, 2004, 2018, 2020))
))[1], "year")

expect_warning(get_values(
  series = array(c(1, 28, 33, 36)),
  jurisdiction = 38,
  year = array(c(1970, 2003, 2004, 2018, 2020)),
  country = TRUE
))

expect_equal(class(get_values(
  series = array(c(1, 28, 33, 36)),
  jurisdiction = 38,
  year = array(c(1970, 2003, 2004, 2018, 2020)),
  agency = array(c(0, 84))
)), "data.frame")

expect_equal(colnames(get_values(
  series = array(c(1, 28, 33, 36)),
  jurisdiction = 38,
  year = array(c(1970, 2003, 2004, 2018, 2020)),
  agency = array(c(0, 84))
))[1], "year")

expect_equal(class(get_values(
  series = array(c(1, 28, 33, 36)),
  jurisdiction = 38,
  year = array(c(1970, 2003, 2004, 2018, 2020)),
  label = "111"
)), "data.frame")

expect_equal(colnames(get_values(
  series = array(c(1, 28, 33, 36)),
  jurisdiction = 38,
  year = array(c(1970, 2003, 2004, 2018, 2020)),
  label = "111"
))[1], "year")

expect_warning(get_values(
  series = array(c(1, 28, 33, 36)), jurisdiction = 38,
  year = array(c(1970, 2003, 2004, 2018, 2020)),
  version = 1
))

expect_invisible(get_values(
  series = array(c(1, 28, 33, 36)), jurisdiction = 38,
  year = array(c(1970, 2003, 2004, 2018, 2020)),
  download = "regdatamultiplefromr.csv"
))


# get_document_values
expect_equal(
  class(get_document_values(
    series = 33, jurisdiction = 38,
    year = 2021, label = "111"
  )),
  "data.frame"
)


# get_reading_time function
expect_equal(
  class(get_reading_time(
    jurisdiction = 38,
    year = array(c(1970, 2003, 2004, 2018, 2020))
  )),
  "data.frame"
)
