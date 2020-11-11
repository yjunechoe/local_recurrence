# ~~~ setup

library(tidyverse)
library(slider)
library(arrow)

arrowdf <- open_dataset("tokens_data")
key <- read_csv("keys.csv")

collect_child <- function(ID, include_symbol = c("xx", "yy")) {
  # Special word symbols - http://www.bu.edu/linguistics/UG/course/lx865-f02/local/childes-symbols.pdf
  arrowdf %>% 
    filter(
      childID == ID,
      part_of_speech == "n" | gloss %in% include_symbol
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
    extract(recurrence, c("word", "times"), "(\\w+) (\\d+)", convert = TRUE)
  
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

moving_window(60)


# ~~~ moving window

df <- key %>% 
  mutate(
    n_utterances = arrowdf %>% 
      group_by(childID) %>% 
      collect() %>% 
      summarize(max(utterance_id), .groups = 'drop') %>% 
      pull(2),
    utterances = map(childID, ~{
      arrowdf %>% 
        filter(childID == .x) %>% 
        group_by(utterance_id) %>% 
        collect() %>% 
        summarize(
          utterance = paste(gloss, collapse = " "),
          has_noun = any(part_of_speech == "n"),
          .groups = 'drop'
        )
    }),
    nouns_data = map(childID, collect_child)
  )

library(foreach)
library(progressr)
library(doFuture)
registerDoFuture()
plan(multisession, workers = 4)

with_progress({
  p <- progressor(along = 1L:nrow(df))
  df$moving_window <- foreach(i = 1L:nrow(df), .packages = c('dplyr', 'tidyr', 'slider')) %dopar% {
    p(sprintf("i=%g", i))
    moving_window(df$childID[i], size = 10L, child_df = df$nouns_data[[i]])
  }
})

df

# write_rds(df, "moving_window_10.rds")





# ~~~ aggregate 

df$moving_window %>% 
  bind_rows() %>% 
  count(word, max_times) %>% 
  filter(max_times > 3) %>% 
  count(word, wt = n, sort = TRUE) %>% 
  View()

df$moving_window %>% 
  bind_rows() %>% 
  ggplot(aes(max_times)) +
  geom_histogram(color = "white", binwidth = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks(10))

# ~~~ correlation plot

library(GGally)

unique_nouns <- arrowdf %>%
  filter(part_of_speech == "n") %>%
  group_by(childID) %>%
  select(childID, gloss) %>%
  collect() %>%
  distinct() %>%
  count() %>% 
  pull(n)

df %>% 
  mutate(
    n_unique_nouns = unique_nouns,
    n_words_recurring = map_dbl(nouns_data, nrow)
  ) %>% 
  select(contains("n_")) %>% 
  ggpairs() +
  hrbrthemes::theme_ipsum() +
  theme(panel.grid.minor = element_blank())
  






# ~~~ ridge plot

library(ggridges)
library(grid)
library(extrafont)

ridge_plot_df <- df %>% 
  mutate(max_times_dist = map(moving_window, ~ count(.x, max_times))) %>% 
  unnest(max_times_dist) %>% 
  mutate(
    ID = glue::glue("{name} [{corpus}] (n={n_utterances})"),
    n_recurring = map_dbl(moving_window, nrow)
  )

ridge_plot <- ridge_plot_df %>% 
  mutate(ID = fct_reorder(ID, n_utterances)) %>% 
  filter(max_times < 25) %>% 
  ggplot(aes(max_times, ID)) +
  geom_density_ridges(
    aes(fill = log(n_recurring)),
    show.legend = FALSE
  ) +
  scale_fill_gradientn(colors = pals::ocean.tempo(100)) +
  geom_text(
    aes(x = 25, label = str_pad(n_recurring, 3)),
    size = 2.5,
    family = "Roboto"
  ) +
  labs(
    title = "Local recurrences of Nouns in child-directed speech",
    subtitle = "10-utterance moving window",
    x = "Maximum recurrence count of a recurring noun",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = expansion(c(.015, 0))) +
  scale_x_continuous(limits = c(0, 25.5), expand = expansion(c(.02, .03))) +
  theme_classic(base_family = "Roboto") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(
      family = "Roboto Slab",
      size = 24,
      margin = margin(b = .5, unit = "cm")
    ),
    plot.subtitle = element_text(
      family = "Montserrat Medium",
      size = 16,
      hjust = .65,
      margin = margin(b = .6, unit = "cm")
    ),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(
      size = 12,
      margin = margin(t = .1, unit = "cm")
    ),
    axis.text.y = element_text(margin = margin(r = .1, unit = "cm")),
    axis.title.x = element_text(
      size = 16,
      margin = margin(t = .3, unit = "cm")
    ),
    plot.margin = margin(1, 1, .5, 1, "cm"),
  ) +
  annotation_custom(
    textGrob(
      "Corpus (# utterances)",
      x = -.11, y = 1.015,
      gp = gpar(fontfamily = "Roboto Slab", fontsize = 11),
    )
  ) +
  annotation_custom(
    textGrob(
      "# recurring",
      x = .94, y = 1.015,
      gp = gpar(fontfamily = "Roboto Slab", fontsize = 11),
    )
  )

# TODO: ggsave crashes with 10-window plot -- try ragg?
# ggsave(ridge_plot, "ridgelines_10.pdf", width = 10, height = 16, units = "in", device = cairo_pdf)

