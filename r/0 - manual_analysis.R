
# Manual Message Analysis -------------------------------------------------

# To Do:
# - Visualize w function. When NULL, create general overview of top contacts.
# - Otherwise, we create visuals for the selected name

pacman::p_load(tidyverse)

source("r/fx_sms_import.r")
source("r/fx_sms_summarise.r")
source("r/fx_sms_append.r")
source("r/fx_sms_export.r")


# Read New File -------------------------------------------------------
data_new <- fx_sms_import()


# Verify New File ---------------------------------------------------------
data_new %>% fx_sms_summarise()


# Read Master Database ----------------------------------------------------
data_master <- fx_sms_import()


# Merge New and Old Files -------------------------------------------------
data_updated <- fx_sms_append(new = data_new, master = data_master)


# Verify New Master -------------------------------------------------------
data_updated$master %>% fx_sms_summarise()


# Export New File and Updated Master --------------------------------------
data_new %>% fx_sms_export("new_whatsapp")
data_updated$master %>% fx_sms_export("master")


# General Visuals ---------------------------------------------------------


