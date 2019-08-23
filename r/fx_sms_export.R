
# SMS - Export Data to RDS ------------------------------------------------

fx_sms_export <- function(data, filename_label) {

  library(lubridate)
  library(readr)
  library(stringr)

  filename_date <-
    data %>%
    pull(DateTime) %>%
    max() %>%
    date()

  write_rds(data,
            path = str_glue("data/{filename_date}_{filename_label}.rds"),
            compress = "bz")


}


# Testing -----------------------------------------------------------------

# test_read_5 %>% fx_sms_export("testing")
