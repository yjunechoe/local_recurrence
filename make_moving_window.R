# ~~~ setup
setwd(here::here("final"))
library(tidyverse)
library(slider)
library(arrow)

arrowdf <- open_dataset("tokens_data")
keys <- read_csv("keys.csv")


# SUBTLEX-US
### zipped tab-delim file from: https://www.ugent.be/pp/experimentele-psychologie/en/research/documents/subtlexus
subtlex <- vroom::vroom("subtlex-us.zip")

nouns <- subtlex %>% 
  select(Word, All_PoS_SUBTLEX, Percentage_dom_PoS) %>% 
  filter(str_extract(All_PoS_SUBTLEX, "^\\w+(?=\\.)") == "Noun") # dominant nouns

# child-produced nouns
child_production_nouns <- keys %>% 
  mutate(production = future_map2(corpus, name, ~ {
    get_tokens(
      corpus = .x, 
      target_child = .y, 
      token = "*",
    ) %>% 
      filter(
        speaker_role == "Target_Child",
        part_of_speech == "n",
        gloss %in% nouns$Word
      )
  }, .progress = TRUE))


## functions

collect_child <- function(ID, include_symbol = c("xx", "yy")) {
  # Special word symbols - http://www.bu.edu/linguistics/UG/course/lx865-f02/local/childes-symbols.pdf
  arrowdf %>% 
    filter(
      childID == ID,
      ((part_of_speech == "n" & gloss %in% nouns$Word) | gloss %in% include_symbol)
    ) %>% 
    collect()
}


moving_window <- function(ID, size = 5L, child_df = NULL) {
  
  if (is.null(child_df)) {
    child_df <- collect_child(ID)
  }
  
  memory_nested <- child_df %>% 
    group_by(utterance_id) %>% 
    summarize(
      words = list(gloss),
      .groups = 'drop'
    ) %>% 
    complete(utterance_id = 1:max(child_df$utterance_id)) %>% 
    mutate(
      buffer = slide(words, ~ sort(flatten_chr(.x)), .before = size), # memory buffer
      recurring = map(buffer, ~ unique(.x[duplicated(.x)])), # within buffer window, which word(s) do you hear again?
      recurring_n = map2(buffer, recurring, ~ rle(.x[.x %in% .y])$lengths),
      recurrence = map2(recurring, recurring_n, paste)
    ) %>% 
    select(-contains("recurring"))
  
  memory_unnested <- memory_nested %>% 
    rowwise() %>% 
    filter(length(recurrence) > 0) %>% 
    ungroup() %>% 
    select(utterance_id, recurrence) %>% 
    unnest(recurrence) %>% 
    extract(recurrence, c("word", "times"), "^(.*) (\\d+)$", convert = TRUE)
  
  max_counts <- memory_unnested %>% 
    group_by(word) %>% 
    summarize(
      max_times = max(times),
      .groups = 'drop'
    ) %>% 
    arrange(-max_times)
  
  attr(max_counts, "size") <- size
  attr(max_counts, "nested") <- memory_nested
  attr(max_counts, "unnested") <- memory_unnested
  
  max_counts
  
}

moving_window(1)


# ~~~ moving window

df <- keys %>% 
  left_join(
    arrowdf %>% 
      group_by(childID) %>% 
      collect() %>% 
      summarize(
        n_words = n(),
        n_utterances = max(utterance_id),
        .groups = 'drop'
      ),
    by = "childID"
  ) %>% 
  mutate(
    utterances = map(childID, ~{
      arrowdf %>% 
        filter(childID == .x) %>% 
        group_by(utterance_id) %>% 
        collect() %>% 
        summarize(
          utterance = paste(gloss, collapse = " "),
          has_noun = any(part_of_speech == "n"),
          .groups = 'drop'
        ) %>% 
        select(utterance, has_noun)
    }),
    nouns_data = map(childID, ~ {
      collect_child(.x) %>% 
        select(utterance_id, gloss)
    })
  )

library(foreach)
library(progressr)
library(doFuture)
registerDoFuture()
plan(multisession, workers = availableCores() - 1)

with_progress({
  p <- progressor(along = 1L:nrow(df))
  df$moving_window <- foreach(i = 1L:nrow(df), .packages = c('dplyr', 'tidyr', 'slider')) %dopar% {
    p(sprintf("i=%g", i))
    moving_window(df$childID[i], size = 10L, child_df = df$nouns_data[[i]])
  }
})

df

write_rds(df, glue::glue("moving_window_{attr(df$moving_window[[1]], 'size')}.rds"))
