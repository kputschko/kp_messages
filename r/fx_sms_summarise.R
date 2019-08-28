
# Function - SMS Summarise ------------------------------------------------
# Types of summaries include:
# glance, time_period, by_contact


fx_sms_summarise <- function(data, type = "glance") {

  library(dplyr)
  library(rlang)
  library(scales)

  if (type == "glance") {
    # ---- Summary at a glance, one row for entire database
    data %>%
      ungroup() %>%
      summarise(Contacts = n_distinct(Contact),
                Messages = length(Contact),
                Length = sum(MessageLength),
                MinDate = min(DateTime) %>% as_date(),
                MaxDate = max(DateTime) %>% as_date()) %>%
      mutate_at(vars(Contacts, Messages, Length), comma)


  } else if (type == "time_period") {
    # ---- Detailed summary by specified time period
    if (data %>% group_vars() %>% is_empty()) {abort("Time period summaries require a group column")}

    data %>%
      summarise(contact_n = n_distinct(Contact),
                message_n = n(),
                length_sum = sum(MessageLength),
                length_avg = mean(MessageLength),
                length_std = sd(MessageLength))


  } else if (type == "by_contact") {
    # ---- Summary by contact, with ranking of contacts
    data %>%
      group_by(Contact, add = TRUE) %>%
      summarise(message_n     = n(),
                length_sum    = sum(MessageLength),
                length_avg    = mean(MessageLength),
                contact_first = min(DateTime) %>% date(),
                contact_last  = max(DateTime) %>% date(),
                contact_days  = n_distinct(date(DateTime))) %>%
      mutate(daily_messages   = message_n / contact_days,
             daily_length     = length_sum / contact_days) %>%
      # ---- Begin Ranking
      ungroup() %>%
      mutate(.Rank_Message_Count = dense_rank(desc(message_n)),
             .Rank_Length_Sum = dense_rank(desc(length_sum)),
             .Rank_Contact_Days = dense_rank(desc(contact_days)),
             .Rank_Messages_per_Day = dense_rank(desc(daily_messages)),
             rank_score = .Rank_Message_Count + .Rank_Length_Sum + .Rank_Contact_Days + .Rank_Messages_per_Day) %>%
      select(-contains(".Rank"))


  }

}

