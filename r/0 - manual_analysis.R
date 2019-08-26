
# Manual Message Analysis -------------------------------------------------

# To Do:
# - Visualize w function. When NULL, create general overview of top contacts.
# - Otherwise, we create visuals for the selected name
# - Find a way to quantify effort. For example, 2019-08-09 was 428 messages to a single person!

pacman::p_load(tidyverse)

source("r/fx_sms_import.r")
source("r/fx_sms_summarise.r")
source("r/fx_sms_append.r")
source("r/fx_sms_export.r")
source("r/fx_sms_visualize.r")


# Read New File -------------------------------------------------------
data_new <- fx_sms_import()


# Read Master Database ----------------------------------------------------
# data_master <- fx_sms_import("C:/Users/kputs/OneDrive/Data/kp_messages/data/2019-08-26_master.rds")
data_master <- fx_sms_import()


# Merge New and Old Files -------------------------------------------------
data_updated <- fx_sms_append(new = data_new, master = data_master)


# Verification ------------------------------------------------------------
data_updated$new %>% fx_sms_summarise("glance")
data_updated$old %>% fx_sms_summarise("glance")
data_updated$master %>% fx_sms_summarise("glance")

data_updated$dupes
data_updated$added


# Export New File and Updated Master --------------------------------------
# data_new %>% fx_sms_export(filename_label = "new_whatsapp")

data_updated$new %>% fx_sms_export(filename_label = "new")
data_updated$master %>% fx_sms_export(filename_label = "master")


# General Visuals ---------------------------------------------------------
data_updated$master %>% fx_sms_visualize()

