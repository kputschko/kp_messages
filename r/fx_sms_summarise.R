
# Function - SMS Summarise ------------------------------------------------

fx_sms_summarise <- function(data, type = "overall") {

  library(tidyverse)
  library(scales)

  if (type == "overall") {

    data %>%
      ungroup() %>%
      summarise(Contacts = n_distinct(Contact),
                Messages = length(Contact),
                Length = sum(MessageLength),
                MinDate = min(DateTime) %>% as_date(),
                MaxDate = max(DateTime) %>% as_date()) %>%
      mutate_at(vars(Contacts, Messages, Length), comma)

  } else if (type == "stats") {

    data %>%
      summarise(contact_n = n_distinct(Contact),
                message_n = n(),
                length_sum = sum(MessageLength),
                length_avg = mean(MessageLength),
                length_std = sd(MessageLength))
  }

}

