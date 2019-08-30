
# Manual Message Analysis -------------------------------------------------

# To Do:
# - Visualize w function. When NULL, create general overview of top contacts.
# - Otherwise, we create visuals for the selected name
# - Find a way to quantify effort. For example, 2019-08-09 was 428 messages to a single person!



# Environment -------------------------------------------------------------

pacman::p_load(tidyverse)

source("r/fx_sms_import.r")
source("r/fx_sms_summarise.r")
source("r/fx_sms_append.r")
source("r/fx_sms_export.r")
source("r/fx_sms_visualize.r")
source("r/fx_sms_filter.r")


# Import + Export ---------------------------------------------------------

# |- Read New File --------------------------------------------------------
data_new <- fx_sms_import()


# |- Read Master Database -------------------------------------------------
data_master <- fx_sms_import()


# |- Merge New and Old Files ----------------------------------------------
data_updated <- fx_sms_append(new = data_new, master = data_master)


# |- Verification ---------------------------------------------------------
data_updated$new %>% fx_sms_summarise("glance")
data_updated$old %>% fx_sms_summarise("glance")
data_updated$master %>% fx_sms_summarise("glance")

data_updated$dupes
data_updated$added


# |- Export New File and Updated Master -----------------------------------
data_updated$new %>% fx_sms_export(filename_label = "new_whatsapp")
data_updated$master %>% fx_sms_export(filename_label = "master")





# Exploration -------------------------------------------------------------


# Data Repository ---------------------------------------------------------
data_master <-
  "C:/Users/kputs/OneDrive/Data/kp_messages/data/2019-08-28_master.rds" %>%
  fx_sms_import()

data_dates <-
  data_master %>%
  fx_sms_filter(mindate = "2019-01-01", maxdate = "2019-06-01")

data_rank <-
  data_master %>%
  fx_sms_summarise("by_contact") %>%
  fx_sms_summarise("rank", rank_n = 20)

data_periods <-
  data_rank %>%
  select(Contact) %>%
  left_join(data_master, by = "Contact") %>%
  fx_sms_summarise("time_periods")



# Overall Scatterplot -----------------------------------------------------
data_master %>% fx_sms_visualize(plot_type = "scatter_all")


# Top N - Bar Comparison --------------------------------------------------
plot_top <-
  data_rank %>%
  fx_sms_visualize("bar_rank")

plot_top$message_n


# Top N - Send/Rec Comparison ---------------------------------------------
data_periods %>%
  fx_sms_summarise("send_rec") %>%
  fx_sms_visualize("send_rec")


# All - Habit Comparison --------------------------------------------------
# Who is new, who started texting more, who texted less?

# data_master %>%
#   mutate(Hour = hour(DateTime),
#          Day = date(DateTime),
#          Weekday = wday(DateTime, label = TRUE, week_start = 1),
#          Week = floor_date(DateTime, unit = "week"),
#          Month = floor_date(DateTime, unit = "month") %>% date(),
#          Year = floor_date(DateTime, unit = "year") %>% year()) %>%
#   group_by(Contact, Day) %>%
#   summarise(Message_Count = n(),
#             Length_Sum = sum(MessageLength),
#             Length_Avg = mean(MessageLength)) %>%
#   ungroup() %>%
#   mutate(Period = ifelse(Day >= max(Day) - days(90), "new", "historical")) %>%
#   group_by(Contact, Period) %>%
#   summarise(day_min = min(Day),
#             day_max = max(Day),
#             day_all = difftime(day_max, day_min, units = "days") %>% as.numeric() %>% ifelse(. == 0, 1, .),
#             day_contact = length(Day),
#             day_proportion = day_contact / day_all,
#             length_sum = sum(Length_Sum),
#             day_length = length_sum / day_contact) %>%
#   select(Period, Contact, day_proportion, day_length) %>%
#   gather(measure, value, day_proportion:day_length) %>%
#   unite(Label, Period, measure) %>%
#   spread(Label, value, fill = 0) %>%
#   mutate(change_length = new_day_length / historical_day_length,
#          change_frequency = new_day_proportion / historical_day_proportion) %>%
#
#   # Visuals
#   filter(Contact %in% data_top()$Contact) %>%
#   ggplot() +
#   aes(x = change_length,
#       y = change_frequency,
#       text = str_glue("Contact: {Contact}\nLength Change: {change_length %>% number(accuracy = 0.01, suffix = 'x')}\nFrequency Change: {change_frequency %>% number(accuracy = 0.01, suffix = 'x')}")) +
#   geom_jitter(width = 0.025, height = 0.025, color = "#3182bd") +
#   geom_hline(yintercept = 1) +
#   geom_vline(xintercept = 1) +
#   labs(x = "Daily Message Length",
#        y = "Daily Contact Frequency") +
#   fx_plot_theme()$light
#
#
# plot_adjustment %>% ggplotly(tooltip = "text")


# Order of Operations -----------------------------------------------------

# Side Bar  - Date Filter
# |- Apply date filter to all non-import-export operations

# Main Page - Import / Export / Summary Tables

# Main Page - Overall Scatterplot
# |- Daily summary, number of messages, total length, number of contacts

# Contacts  - Top 20: Bars / Count / Length / Days / Per Day
# Contacts  - Top 20:
# Contacts  -| Longer Messages
# Contacts  -| Changes in Message Habits last 3/6 months -
# Contacts  ----| group by contact and time period, compare requested time period with previous time periods

# Contacts  - Select Contact
# Contacts  -| Summary Table - summary by contact and message type
# Contacts  -| Contact Timeline - min, max, median, count of message length by week
# Contacts  -| Initial Messages - first message type by contact and day
# Contacts  -| Word Sentiment Frequency - unnest message by word, total words, total sentiment score, proportion of sentiment

# Sender    -
