
# SMS - Filter Dates ------------------------------------------------------
# Couldn't figure out how to do it with rlang, so I brute forced it! IDK!

fx_sms_filter <- function(data,
                          contact_filters = NULL,
                          mindate = NULL,
                          maxdate = NULL) {

  library(dplyr)
  library(lubridate)
  library(rlang)
  library(stringr)

  .data_step_1 <-
    if (contact_filters %>% is_empty()) {data} else{
      data %>% filter(Contact %in% contact_filters)
    }

  .data_step_2 <-
    if (mindate %>% is_empty()) {.data_step_1} else {
      .data_step_1 %>% filter(date(DateTime) >= mindate)
    }

  .data_step_3 <-
    if (maxdate %>% is_empty()) {.data_step_2} else {
      .data_step_2 %>% filter(date(DateTime) <= maxdate)
    }


  return(.data_step_3)

}


# Testing -----------------------------------------------------------------
# data <- data_master
# contact_filters <- NULL
# mindate <- NULL
# maxdate <- NULL
#
# data <- data_master
# contact_filters <- "Emily Kay Piellusch"
# mindate <- NULL
# maxdate <- NULL
#
# data <- data_master
# contact_filters <- "Emily Kay Piellusch"
# mindate <- "2019-07-01"
# maxdate <- NULL

# data_master %>% fx_sms_filter()
# data_master %>% fx_sms_filter(contact_filter = "Emily Kay Piellusch")
# data_master %>% fx_sms_filter(contact_filter = "Emily Kay Piellusch", mindate = "2019-07-01")
#
# data_master %>%
#   fx_sms_filter(contact_filter = "Emily Kay Piellusch",
#                 mindate = "2019-07-01",
#                 maxdate = "2019-08-01") %>%
#   fx_sms_summarise()
