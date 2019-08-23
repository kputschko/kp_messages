
# Read WhatsApp Export ----------------------------------------------------
# Contact, DateTime, MessageType, Message, MessageLength

# pacman::p_install_gh("JBGruber/rwhatsapp")
pacman::p_load(tidyverse, rwhatsapp, lubridate)


# Import ------------------------------------------------------------------

d_raw <- rwa_read("C:/Users/kputs/Downloads/WhatsApp Chat with Emily Kay Piellusch.txt")

d_prep <-
  d_raw %>%
  filter(!is.na(author)) %>%
  mutate(Contact = author %>% unique() %>% setdiff("Kevin")) %>%
  mutate(MessageType = ifelse(author == "Kevin", "Sent", "Received")) %>%
  mutate(Message = str_replace(text, "<Media omitted>", ".")) %>%
  mutate(MessageLength = str_length(Message)) %>%
  select(Contact, DateTime = time, MessageType, Message, MessageLength, emoji_name, author) %>%
  print()


# Export ------------------------------------------------------------------

export_label <-
  d_prep %>%
  summarise(max_date = DateTime %>% max() %>% date()) %>%
  pull()

d_export <-
  d_prep %>%
  select(-emoji_name, -author)

d_export %>% write_rds(str_glue("data/new/wa_{export_label}.rds"))

# Visuals -----------------------------------------------------------------

d_prep %>%
  ggplot(aes(x = DateTime, fill = MessageType)) +
  geom_density(alpha = 0.50)

d_prep %>%
  group_by(day = date(DateTime), MessageType) %>%
  summarise(length_sum = sum(MessageLength)) %>%
  ggplot(aes(x = day, y = length_sum, fill = MessageType)) +
  geom_col(position = "dodge")

d_prep %>%
  select(author, emoji_name) %>%
  unnest(emoji_name) %>%
  count(author, emoji_name, sort = TRUE)

# Testing! ----------------------------------------------------------------


# Testing Text Import -----------------------------------------------------

# fruits <- "apples and oranges and pears and bananas\npineapples and mangos and guavas"
#
# fruits %>%
#   str_split("\n") %>%
#   map_df(enframe, name = "id", value = "message")
#
# spotify_text <- "Here’s a song for you… Rebel Rebel by Seu Jorge https://open.spotify.com/track/5mZYRyOPWVlTtPGWHJCbAL?si=vbIw1Ty1SBqrH-nu3hTmVA\nnewline"
# spotify_text <- "https://open.spotify.com/episode/6Lt33QIVpvBk9fpHKrRJ91?s\nalala"
# spotify_regex <- "https://open.spotify.*\n"
#
# str_view(spotify_text, spotify_regex)
# str_replace(spotify_text, spotify_regex, "")


# Test JBGruber's Package -------------------------------------------------

# test_rwa <- rwa_read("C:/Users/kputs/Downloads/WhatsApp Chat with Emily Kay Piellusch.txt")
# test_rwa %>% view()

# Read Text File ----------------------------------------------------------

# raw <- read_file("C:/Users/kputs/Downloads/WhatsApp Chat with Emily Kay Piellusch.txt")
#
# table <-
#   raw %>%
#   str_replace_all("https://open.spotify.com*\n", "<spotify>\n") %>%
#   str_replace_all("https://www.reddit.com.*\n", "<reddit>\n") %>%
#   str_replace_all("\n\n", "<newline>") %>%
#   str_split("\n") %>%
#   map_df(enframe, name = "id", value = "text")
#
# table %>% View()
#
# tidy <-
#   table %>%
#   # slice(1:10) %>%
#   slice(710:715) %>%
#   separate(text, c("dt", "message"), sep = " - ", extra = "merge") %>% print()
#   mutate(seconds = str_pad(id, pad = "0", width = 2, side = "left")) %>%
#   mutate(dts = str_c(dt, seconds, sep = ":")) %>%
#   mutate(DateTime = parse_datetime(dts, format = "%D, %T")) %>%
#   select(DateTime, message) %>%
#   filter(!str_detect(message, "Messages to this chat and calls")) %>%
#   separate(message, c("raw_contact", "Message"), sep = ":", extra = "merge") %>%
#   mutate(Message = str_trim(Message, side = "both")) %>%
#   mutate(Contact = raw_contact %>% unique() %>% setdiff("Kevin")) %>%
#   mutate(MessageType = ifelse(raw_contact == "Kevin", "Sent", "Received")) %>%
#   mutate(Message = str_replace(Message, "<Media omitted>", ".")) %>%
#   mutate(MessageLength = str_length(Message)) %>%
#   select(Contact, DateTime, MessageType, Message, MessageLength) %>%
#   print()
#
# view(tidy, name = "tidy")
