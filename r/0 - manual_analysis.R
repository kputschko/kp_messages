
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
data_updated$new %>% fx_sms_export(filename_label = "new")
data_updated$master %>% fx_sms_export(filename_label = "master")





# Exploration -------------------------------------------------------------
data_master <- fx_sms_import("C:/Users/kputs/OneDrive/Data/kp_messages/data/2019-08-26_master.rds")


# General Visuals ---------------------------------------------------------
data_master %>% fx_sms_visualize()
data_master %>% fx_sms_filter(mindate = "2019-01-01") %>% fx_sms_visualize()


# General Summaries -------------------------------------------------------
data_master %>% fx_sms_summarise("by_contact")
data_master %>% fx_sms_summarise("by_contact") %>% top_n(20, -rank_score) %>% arrange(rank_score) %>% view()

data_master %>% fx_sms_filter(mindate = "2019-01-01") %>% fx_sms_summarise()
data_master %>% fx_sms_filter(mindate = "2019-01-01") %>% fx_sms_summarise("by_contact") %>% view()



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
