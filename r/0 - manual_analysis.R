
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
  "C:/Users/kputs/OneDrive/Data/kp_messages/data/2019-09-26_master.rds" %>%
  fx_sms_import()

data_dates <-
  data_master %>%
  fx_sms_filter(mindate = "2019-01-01", maxdate = "2019-06-01")

data_rank <-
  data_master %>%
  fx_sms_summarise("by_contact") %>%
  fx_sms_summarise("rank", rank_n = 20)


# Is this necessary?  For Habit Comparison, I want data_master > time_periods
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

data_master %>%
  fx_sms_summarise("time_periods") %>%
  fx_sms_summarise("period_adjustment") %>%
  fx_sms_visualize("period_adjustment")


# Test - Reg Exp ----------------------------------------------------------

# install.packages("devtools")
# devtools::install_github("VerbalExpressions/RVerbalExpressions")
library(RVerbalExpressions)

test_string <-
  data_master %>%
  # filter(str_detect(Message, "https:")) %>%
  filter(str_detect(Message, "goo.gl")) %>%
  slice(1:10) %>%
  pull(Message) %>%
  print()

test_regex <-
  rx() %>%
  rx_seek_prefix("https://") %>%
  rx_anything() %>%
  rx_seek_suffix(".com")

test_string %>% str_extract(test_regex)

test_regex2 <-
  rx() %>%
  rx_find("https://") %>%
  rx_anything()

  # rx_anything() %>%
  # rx_space() %>%
  # rx_word_edge()
  # rx_one_or_more()

test_string %>% str_extract(test_regex2)
test_string %>% str_extract('(?:(?:https?|ftp):\\/\\/)?[\\w/\\-?=%.]+\\.[\\w/\\-?=%.]+')


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
