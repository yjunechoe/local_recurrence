# ~~~ Retrieve & process data
setwd(here::here("final"))
library(tidyverse)
library(furrr)
library(childesr)

plan(multisession, workers = 4)

d_eng_na <- get_transcripts(collection = "Eng-NA")

df <- d_eng_na %>% 
  filter(target_child_age <= 18) %>% 
  select(
    age = target_child_age,
    name = target_child_name,
    corpus = corpus_name
  ) %>% 
  group_by(name, corpus) %>% 
  summarize(
    start_age = min(age),
    end_age = max(age),
    .groups = 'drop'
  )

tokens <- df %>% 
  mutate(data = future_map2(corpus, name, ~ {
    get_tokens(
      corpus = .x, 
      target_child = .y, 
      token = "*",
      role_exclude = c("Target_Child")
    )
  }, .progress = TRUE))

speakers <- tokens$data %>%
  map("speaker_role") %>%
  flatten_chr() %>% 
  sort() %>% 
  rle()

tokens <- tokens %>% 
  mutate(data = map(data, ~ {
    .x %>% 
      select(utterance_id, token_order, gloss, stem, part_of_speech, speaker_role) %>% 
      mutate(utterance_id = as.integer(factor(utterance_id)))
  })) %>% 
  mutate(childID = 1L:n()) %>% 
  relocate(childID)

write_rds(tokens, "C:/Users/jchoe/Desktop/tokens.rds")

keys <- tokens %>% 
  select(-data)

write_csv(keys, "keys.csv")


# --- Write files

df <- read_rds("C:/Users/jchoe/Desktop/tokens.rds")
keys <- read_csv("keys.csv")

df$data %>% 
  imap(~ {
    .x %>% 
      mutate(childID = as.integer(.y)) %>% 
      select(childID, utterance_id, gloss, part_of_speech)
  }) %>% 
  iwalk(~ {arrow::write_parquet(.x, paste0("C:/Users/jchoe/Desktop/tokens_data/child", as.integer(.y), ".parquet"))})




