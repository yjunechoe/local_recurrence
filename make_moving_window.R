# ~~~ setup
library(tidyverse)
library(here)
library(slider)
library(arrow)

arrowdf <- open_dataset(here("data", "tokens_data"))
keys <- read_csv(here("data", "keys.csv"))


# SUBTLEX-US
### zipped tab-delim file from: https://www.ugent.be/pp/experimentele-psychologie/en/research/documents/subtlexus
subtlex <- vroom::vroom(here("data", "subtlex-us.zip"))

nouns <- subtlex %>% 
  select(Word, All_PoS_SUBTLEX, Percentage_dom_PoS) %>% 
  filter(str_extract(All_PoS_SUBTLEX, "^\\w+(?=\\.)") == "Noun") # dominant nouns

# # child-produced nouns
# child_production_nouns <- keys %>% 
#   mutate(production = future_map2(corpus, name, ~ {
#     get_tokens(
#       corpus = .x, 
#       target_child = .y, 
#       token = "*",
#     ) %>% 
#       filter(
#         speaker_role == "Target_Child",
#         part_of_speech == "n",
#         gloss %in% nouns$Word
#       )
#   }, .progress = TRUE))


## functions

collect_child <- function(ID, include_symbol = c("xx", "yy")) {
  # Special word symbols - http://www.bu.edu/linguistics/UG/course/lx865-f02/local/childes-symbols.pdf
  arrowdf %>% 
    filter(
      childID == ID,
      ((part_of_speech == "n" & gloss %in% nouns$Word) | gloss %in% include_symbol)
    ) %>% 
    collect() %>% 
    mutate(gloss = textstem::lemmatize_words(gloss))
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
      buffer = slide(words, ~ sort(flatten_chr(.x)), .before = size - 1), # memory buffer; -1 for inclusive lag
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
    tidyr::extract(recurrence, c("word", "times"), "^(.*) (\\d+)$", convert = TRUE) # override conflict with {magrittr}
  
  recurrence_stats <- memory_unnested %>% 
    group_by(word) %>% 
    summarize(
      n_events = sum(diff(utterance_id) > 1) + 1,     # number of recurrence events
      total_size = length(utterance_id),              # number of utterances involved in recurrence events
      max_size = max(times),                          # max number of times a word recurs in a given window
      avg_size = total_size/n_events,                 # average size (in # of utterances) of recurrence events
      .groups = 'drop'
    ) %>% 
    left_join(count(child_df, word = gloss, name = "frequency"), by = "word") %>% 
    relocate(word, frequency) %>% 
    arrange(desc(frequency, total_size))
  
  attr(recurrence_stats, "size") <- size
  attr(recurrence_stats, "nested") <- memory_nested
  attr(recurrence_stats, "unnested") <- memory_unnested
  
  recurrence_stats
  
}


# ~~~ corpus metadata

corpora <- keys %>% 
  left_join(
    arrowdf %>% 
      group_by(childID) %>% 
      collect() %>% 
      summarize(
        n_unique_words = length(unique(gloss)),
        n_words = n(),
        n_utterances = max(utterance_id),
        .groups = 'drop'
      ),
    by = "childID"
  ) %>% 
  mutate(
    TTR = n_unique_words/n_words,
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


# ~~~ create moving windows

library(foreach)
library(progressr)
library(doFuture)
registerDoFuture()
plan(multisession, workers = availableCores() - 1)

with_progress({
  p <- progressor(along = 1L:nrow(corpora))
  corpora$moving_window <- foreach(i = 1L:nrow(corpora), .packages = c('dplyr', 'tidyr', 'slider')) %dopar% {
    p(sprintf("i=%g", i))
    moving_window(corpora$childID[i], size = 5L, child_df = corpora$nouns_data[[i]])
  }
})

# normalized stats
corpora <- corpora %>% 
  mutate(moving_window = imap(moving_window, ~ mutate(
    .x,
    log_freq = log(frequency/n_words[.y]),               # log frequency of count for nouns that participate in repetition events
    normalized_size = log(total_size/n_utterances[.y])   # total size of repetition events over # of utterances in corpus
  ))) 

write_rds(corpora, here("data", "moving_windows", glue::glue("moving_window_{attr(corpora$moving_window[[1]], 'size')}.rds")))





############
## Images ##
############


# moving window schematic table

library(kableExtra)

cookie_ex <- collect_child(1) %>% 
  select(utterance_id, gloss) %>% 
  filter(gloss == "cookie" & utterance_id > 600) %>% 
  right_join(tibble(utterance_id = 632:646), by = "utterance_id") %>% 
  arrange(utterance_id)

cols_moving_window <- function(df, id_col, size, .fill = c("x", "")) {
  ID <- as.character(pull(df, {{ id_col }}) + size - 1) # account for lag
  reduce(1:(nrow(df) - (size - 1)), ~ bind_cols(.x, !!ID[.y] := ifelse(between(1:nrow(df), .y, .y + size - 1), .fill[1], .fill[2])), .init = df)
}

cookie_tbl_df <- cols_moving_window(cookie_ex, utterance_id, 5, .fill = c("__", "")) %>% 
  rename_at(1:2, ~ c("Utterance", "Noun")) %>% 
  mutate(across(matches("\\d+"), ~ {
    .x[.x == "__" & cookie_ex$gloss == "cookie"] <- "--" 
    .x %>% 
      cell_spec(
        background = case_when(.x == "--" ~ "#404040", .x == "__" ~ "#A1A1A1", TRUE ~ "white"),
        color = "transparent",
        extra_css = "padding:2px",
        background_as_tile = F
      )
  })) %>% 
  replace_na(list(Noun = "-"))

names(cookie_tbl_df) <- map_chr(names(cookie_tbl_df), ~ ifelse(sum(str_detect(cookie_tbl_df[[.x]], "#404040")) >= 2,
                                                               paste0("<strong style='border: 1px solid black; padding-right:1px; padding-left:1px;'>", .x, "</strong>"), .x))

cookie_tbl_df %>% 
  kbl(align = 'c', escape = FALSE) %>% 
  kable_classic(full_width = FALSE) %>% 
  add_header_above(c(" " = 1, " " = 1, "5-Utterance Moving Window (Lagged)" = 11)) %>% 
  column_spec(2, border_right = TRUE, width = "80px") %>% 
  column_spec(3:13, border_right = TRUE, width = "40px") %>% 
  kable_styling(
    bootstrap_options = "none", 
    font_size = 18,
    html_font = "CMU Typewriter Text"
  ) %>% 
  save_kable(file = here("img", "cookie_windows.png"), zoom = 3)



# moving window stats table - examples

corpora %>% 
  filter(childID == 1) %>% 
  rowwise() %>% 
  mutate(
    n_repeat_words = paste0(nrow(moving_window), "/", n_distinct(nouns_data$gloss)),
    n_repeat_utterances = sum(moving_window$total_size)
  ) %>% 
  ungroup() %>% 
  transmute(Child = name, Corpus = corpus, Age = round(start_age, 1), "Unique Nouns" = n_repeat_words, "Utterances" = paste0(n_repeat_utterances, "/", n_utterances)) %>% 
  kbl(align = 'c') %>% 
  kable_classic(full_width = FALSE) %>%
  add_header_above(c(" " = 1, " " = 1, " " = 1, "Proportion in Repetition Events" = 2)) %>% 
  kable_styling(
    bootstrap_options = "none", 
    font_size = 18,
    html_font = "CMU Typewriter Text"
  ) %>% 
  save_kable(file = here("img", "ex_corpus_stats.png"), zoom = 3)

corpora %>% 
  filter(childID == 1) %>% 
  pull(moving_window) %>% 
  pluck(1) %>% 
  mutate(avg_size = round(avg_size, 2)) %>% 
  select(Word = word, Frequency = occurence, Count = n_events, "Total Size" = total_size, "Max Size" = max_recurrence, "Average Size" = avg_size) %>% 
  arrange(desc(Frequency, Word)) %>% 
  kbl(align = 'lccccc') %>% 
  kable_classic(full_width = FALSE) %>%
  add_header_above(c(" " = 1, " " = 1, "Repetition Events" = 4)) %>% 
  column_spec(2, border_right = TRUE, width = "100px") %>% 
  kable_styling(
    bootstrap_options = "none", 
    font_size = 18,
    html_font = "CMU Typewriter Text"
  ) %>% 
  save_kable(file = here("img", "ex_nouns_stats.png"), zoom = 3)


# Moving window stats table - Providence

df %>% 
  filter(corpus == "Providence") %>% 
  rowwise() %>% 
  mutate(
    n_repeat_words = paste0(nrow(moving_window), "/", n_distinct(nouns_data$gloss)),
    n_repeat_utterances = sum(moving_window$total_size)
  ) %>% 
  ungroup() %>% 
  transmute(Child = name, Age = paste0(sprintf("%.1f", round(start_age, 1)), "-", round(end_age, 1)), "Unique Nouns" = n_repeat_words, "Utterances" = paste0(n_repeat_utterances, "/", n_utterances)) %>% 
  kbl(align = 'c') %>% 
  kable_classic(full_width = FALSE) %>%
  add_header_above(c(" " = 1, " " = 1, "Proportion in Repetition Events" = 2)) %>% 
  kable_styling(
    bootstrap_options = "none", 
    font_size = 18,
    html_font = "CMU Typewriter Text"
  ) %>% 
  save_kable(file = here("img", "providence_corpus_stats.png"), zoom = 3)
