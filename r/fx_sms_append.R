
# SMS - Append New Messages to Master Backup ------------------------------

fx_sms_append <- function(new, master) {

  # Environment ----
  library(dplyr)
  library(readr)
  library(tibble)


  # Append Data ----
  message("Appending New to Master")
  data_update <-
    bind_rows(master, new) %>%
    arrange(desc(DateTime)) %>%
    ungroup() %>%
    distinct()


  # Quality Check ----
  # Check the old script in sms_analysis for a more fleshed out approach

  message("Quality Check")
  data_check <-
    data_update %>%
    mutate(MessageLength = str_length(Message),
           Fuzz_Date = date(DateTime),
           Fuzz_Hour = hour(DateTime),
           Fuzz_Minute = minute(DateTime),
           Fuzz_Message = str_remove_all(Message, "[^A-Za-z0-9]"),
           Fuzz_Length = str_length(Fuzz_Message)) %>%
    distinct(Contact, MessageType, Fuzz_Date, Fuzz_Hour, Fuzz_Minute, Fuzz_Message, .keep_all = TRUE) %>%
    select(Contact, DateTime, MessageType, Message, MessageLength)

  data_removed <-
    anti_join(data_update, data_check,
              by = c("Contact", "DateTime", "MessageType", "Message"))

  data_added <-
    anti_join(data_check, master,
              by = c("Contact", "DateTime", "MessageType", "Message"))

  rows_removed <- nrow(data_update) - nrow(data_check)
  rows_added   <- nrow(data_check) - nrow(master)

  str_glue("|- Duplicate Rows Found: {rows_removed}") %>% inform()
  str_glue("|- Rows Added to Backup: {rows_added}") %>% inform()


  # Output ----
  lst(new = new,
      old = master,
      master = data_check,
      dupes = data_removed,
      added = data_added,
      notes = list(removoed = str_glue("Duplicate Rows Found: {rows_removed}"),
                   added    = str_glue("Rows Added: {rows_added}"))
  )

}


# Test Function -----------------------------------------------------------

# fx_sms_append()
