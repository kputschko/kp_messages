
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

  filepath <- str_glue("data/{filename_date}_{filename_label}.rds")

  str_glue("|- Writing {filepath}") %>% inform()

  write_rds(data, path = filepath, compress = "bz")


}


# Testing -----------------------------------------------------------------

# test_read_5 %>% fx_sms_export("testing")
