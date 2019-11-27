
# Function - SMS Summarise ------------------------------------------------
# Types of summaries include:
# glance, time_period, by_contact


fx_sms_summarise <- function(data, type = "glance", rank_n = NULL) {

  library(dplyr)
  library(lubridate)
  library(rlang)
  library(scales)



# glance ------------------------------------------------------------------
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



# time_periods ------------------------------------------------------------
  } else if (type == "time_periods") {
    data %>%
      mutate(hour = hour(DateTime),
             day = date(DateTime),
             weekday = wday(DateTime, label = TRUE, week_start = 1),
             week = floor_date(DateTime, unit = "week"),
             month = floor_date(DateTime, unit = "month") %>% date(),
             year = floor_date(DateTime, unit = "year") %>% year())



# group_summary -----------------------------------------------------------
  } else if (type == "group_summary") {
    # ---- Detailed summary by specified time period
    if (data %>% group_vars() %>% is_empty()) {abort("Time period summaries require a group column")}

    data %>%
      summarise(contact_n = n_distinct(Contact),
                message_n = n(),
                length_sum = sum(MessageLength),
                length_avg = mean(MessageLength),
                length_std = sd(MessageLength))



# by_contact --------------------------------------------------------------
  } else if (type == "by_contact") {
    # --- Summary by contact,
    # --- Add groups prior to running this function for day/week/message type/etc.
    data %>%
      group_by(Contact, add = TRUE) %>%
      summarise(message_n     = n(),
                length_sum    = sum(MessageLength),
                length_avg    = mean(MessageLength),
                contact_first = min(DateTime) %>% date(),
                contact_last  = max(DateTime) %>% date(),
                contact_days  = n_distinct(date(DateTime))) %>%
      mutate(daily_messages   = message_n / contact_days,
             daily_length     = length_sum / contact_days)



# rank --------------------------------------------------------------------
  } else if (type == "rank") {
    stopifnot(c("message_n", "length_sum") %in% colnames(data))

    .temp_data <-
      data %>%
      mutate(.Rank_Message_Count = dense_rank(desc(message_n)),
             .Rank_Length_Sum = dense_rank(desc(length_sum)),
             .Rank_Contact_Days = dense_rank(desc(contact_days)),
             .Rank_Messages_per_Day = dense_rank(desc(daily_messages)),
             .Rank_Length_per_Day = dense_rank(desc(daily_length)),
             rank_score = .Rank_Message_Count + .Rank_Length_Sum + .Rank_Contact_Days + .5*.Rank_Messages_per_Day) %>%
      select(-contains(".Rank"))

    # ---- Select Top N
    if (is_empty(rank_n)) {.temp_data} else {
      .temp_data %>%
        top_n(rank_n, -rank_score) %>%
        arrange(rank_score)
    }


# rank_new ----------------------------------------------------------------
  } else if (type == "rank_new") {

    data %>%
      filter(Contact != "(Unknown)") %>%
      fx_sms_summarise(type = "by_contact") %>%
      fx_sms_summarise(type = "rank") %>%
      filter(contact_first > (data_summary$MaxDate - months(6))) %>%
      top_n(10, -rank_score) %>%
      arrange(rank_score)


# send_rec ----------------------------------------------------------------
  } else if (type == "send_rec") {

    data %>%
      select(Contact, DateTime, MessageType, Message, MessageLength, day) %>%
      mutate(length_adj = if_else(MessageType == "Sent", -MessageLength, MessageLength)) %>%
      group_by(Contact, day) %>%
      summarise(length_diff = sum(length_adj)) %>%
      group_by(Contact) %>%
      summarise(quantiles = list(quantile(length_diff) %>% enframe() %>% spread(name, value)),
                `Days of Contact` = length(day)) %>%
      unnest(quantiles) %>%
      rename(Min = "0%", Max = "100%", Q1 = "25%", Median = "50%", Q3 = "75%") %>%
      arrange(Median) %>%
      mutate(Contact = as_factor(Contact),
             `Longer Messages` = if_else(Median > 0, "Mine", "Theirs")) %>%
      arrange(-`Days of Contact`)


# period_adjustment -------------------------------------------------------
  } else if (type == "period_adjustment") {

    data %>%
      fx_sms_summarise("time_periods") %>%
      mutate(period = ifelse(day >= max(day) - days(90), "current", "historical")) %>%
      group_nest(period) %>%
      deframe() %>%
      modify_at("current", ~ .x %>%
                  fx_sms_summarise("by_contact") %>%
                  fx_sms_summarise("rank", rank_n = 20) %>%
                  mutate(days_all = difftime(contact_last, contact_first, units = "days") %>% as.numeric() %>% magrittr::add(1),
                         days_proportion = contact_days / days_all)) %>%
      modify_at("historical", ~ .x %>%
                  fx_sms_summarise("by_contact") %>%
                  mutate(days_all = difftime(contact_last, contact_first, units = "days") %>% as.numeric() %>% magrittr::add(1),
                         days_proportion = contact_days / days_all)) %>%
      reduce(left_join, by = "Contact", suffix = c(".c", ".h")) %>%
      mutate(adj_length = length_avg.c / length_avg.h,
             adj_freq   = daily_messages.c / daily_messages.h) %>%
      replace_na(list(adj_length = 1, adj_freq = 1)) %>%
      arrange(adj_freq) %>%
      mutate_at("Contact", as_factor) %>%
      select(Contact, adj_freq, adj_length) %>%
      gather(label, value, adj_freq, adj_length) %>%
      mutate(flag = case_when(value == 1 ~ "new",
                              value > 1 ~ "pos",
                              value < 1 ~ "neg"))

  }

}


# Test --------------------------------------------------------------------

# data <- data_master
# data <- data_periods
