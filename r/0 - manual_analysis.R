
# Manual Message Analysis -------------------------------------------------

# To Do:
# - Visualize w function. When NULL, create general overview of top contacts.
# - Otherwise, we create visuals for the selected name
# - Find a way to quantify effort. For example, 2019-08-09 was 428 messages to a single person!


# Environment -------------------------------------------------------------

pacman::p_load(tidyverse, anytime)

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

# whatsapp new output
data_updated$new %>% fx_sms_export(filename_label = "new_whatsapp")

# normal output
data_updated$new %>% fx_sms_export(filename_label = "new")
data_updated$master %>% fx_sms_export(filename_label = "master")





# Exploration -------------------------------------------------------------


# Data Repository ---------------------------------------------------------
data_master <-
  fx_sms_import("data/2019-11-25_master.rds") %>%
  filter(Contact != "(Unknown)")

data_dates <-
  data_master %>%
  fx_sms_filter(mindate = "2019-01-01", maxdate = "2019-06-01")

data_summary <-
  data_master %>%
  fx_sms_summarise()

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


# New Contacts ------------------------------------------------------------
data_new_contacts <-
  data_master %>%
  fx_sms_summarise(type = "rank_new") %>%
  print()

data_master %>%
  semi_join(data_new_contacts, by = "Contact") %>%
  arrange(DateTime) %>%
  mutate(Contact = fct_inorder(Contact, ordered = TRUE) %>% fct_rev()) %>%
  fx_sms_visualize("rank_new")


# ! Experimental -
pacman::p_load_gh("rstudio/gt")

data_new_contacts %>%
  select(Contact, message_n, length_sum) %>%
  gt(rowname_col = "Contact") %>%
  tab_header(title = "New Contacts",
             subtitle = "In the Last 6 Months")
  cols_label(message_n = "Messages",
             length_sum = "<center>Total<br>Message Length</center>" %>% html()) %>%
  fmt_number(columns = vars(length_sum),
             scale_by = 1 / 1000,
             decimals = 1,
             pattern = "{x}k")


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

# Side Bar ---
# - Import master database

# Master Database ---
# - Scatterplot: Day by Message Length (N Msg, N Contact)
# - Summary Values: N Messages, N Contacts, Sum Length, Date Min, Date Max
# - Bar Graph: Top 20 Contacts (Count, Length, Days, Daily Avg)

# New Messages ---
# - Load: New Backups
# - Merge: New and Master
# - Export: Updated Master
# - Table: New Contacts Last 3/6 Months
# - Lollipop Chart: Habit Changes in Last 3/6 Months

# Contact Specific ---
# - Select Contact
# - Table: Summary by contact and message type
# - Timeline: Week by (Min, Max, Median, N Message, Length)
# - Initial Messages: First message type by contact and day
# - Word Sentiment Freq: Unnesting message by workd, total words, total sentiment score, prop of sentiment

# Sent Messages ---


