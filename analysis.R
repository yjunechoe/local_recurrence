library(tidyverse)
library(magrittr)
library(arrow)

setwd(here::here("final"))

arrowdf <- open_dataset("tokens_data")
keys <- read_csv("keys.csv")

df <- read_rds("moving_window_10.rds")



# --- Analysis 1 
### Are nouns that recur often across corpuses more likely to be learned?
### Is that effect larger than how frequency predicts acquisition?

df$moving_window %>% 
  bind_rows() %>% 
  ggplot(aes(max_times)) +
  geom_histogram(color = "white", binwidth = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks(10))

# filter out corpus w/ low local_recurrences
low_nouns_recurring <- rep(FALSE, nrow(df))  # map_lgl(df$moving_window, ~ nrow(.x) < 30)

aggregate_noun_recurrences <- df$moving_window[!low_nouns_recurring] %>% 
  bind_rows() %>% 
  count(word, max_times) %>% 
  count(word, wt = n, name = "local_recurrence", sort = TRUE)

aggregate_noun_occurences <- df[!low_nouns_recurring, ] %>% 
  transmute(word = map(nouns_data, ~ unique(.x$gloss))) %>% 
  unnest(word) %>% 
  count(word, name = "occurence", sort = TRUE)
  
aggregate_noun_freq <- df[!low_nouns_recurring, ] %>% 
  mutate(word = map(nouns_data, ~ count(.x, gloss))) %>% 
  transmute(word = map2(word, n_words, ~ mutate(.x, freq = log(n/.y)))) %>% 
  unnest(word) %>% 
  group_by(gloss) %>% 
  summarize(freq = mean(freq), .groups = 'drop') %>% 
  rename(word = gloss)


aggregate_normalized <- aggregate_noun_recurrences %>% 
  left_join(aggregate_noun_occurences, by = "word") %>% 
  left_join(aggregate_noun_freq, by = "word") %>% 
  mutate(normalized = local_recurrence/occurence) %>% 
  filter(occurence > 6) %>% # words that occur at least 10% of corpuses
  arrange(-normalized)

hist(aggregate_normalized$normalized)

item_trajectories <- read_csv("item_trajectory_table.csv") %>% 
  select(-c(1:3)) %>% 
  pivot_longer(-age) %>% 
  filter(age == 18) %>% 
  mutate(name = str_extract(name, "^.*(?=\\n)")) %>%
  rowwise() %>% 
  mutate(name = ifelse(length(unique(str_extract_all(name, "\\w+")[[1]])) == 1, str_extract(name, "^\\w+"), name)) %>% 
  select(word = name, trajectory = value)


growth_corr_df <- aggregate_normalized %>% 
  filter(word %in% nouns$Word) %>% 
  left_join(item_trajectories, by = "word") %>% 
  arrange(-normalized)

growth_corr_df %>% 
  filter(!is.na(trajectory))

# local_recurrence and trajectory
growth_corr_df %>% 
  na.omit() %$%
  cor(normalized, trajectory)

growth_corr_df %>% 
  na.omit() %>% 
  ggplot(aes(normalized, trajectory)) +
  geom_point() +
  geom_smooth(method = 'lm')


# frequency and trajectory
growth_corr_df %>% 
  na.omit() %$%
  cor(freq, trajectory)

growth_corr_df %>% 
  na.omit() %>% 
  ggplot(aes(freq, trajectory)) +
  geom_point() +
  geom_smooth(method = 'lm')


library(GGally)

# all three
growth_corr_df %>% 
  select(freq, normalized, trajectory) %>% 
  ggpairs() +
  hrbrthemes::theme_ipsum() +
  theme(panel.grid.minor = element_blank())


mdl <- lm(trajectory ~ normalized * freq, data = growth_corr_df)
summary(mdl)
car::vif(mdl)

# ~~~ correlation plot

### counts of words, utterances, unique nouns, recurring nouns

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
    n_unique_recurring = map_dbl(moving_window, nrow)
  ) %>% 
  select(contains("n_")) %>% 
  ggpairs() +
  hrbrthemes::theme_ipsum() +
  theme(panel.grid.minor = element_blank())
  

### of recurring nouns, correlation between max recurrence count and frequency

# recurrence to frequency

recurrence_to_freq <-
  map2(df$moving_window, df$childID, ~ {
    .x %>% 
      left_join(
        df %>% 
          filter(childID == .y) %>% 
          pull(nouns_data) %>%
          pluck(1) %>% 
          count(gloss),
        by = c("word" = "gloss")
      ) %>% 
      arrange(-max_times)
  })

map_dbl(recurrence_to_freq, ~ cor(.x$max_times, .x$n)) %>% 
  hist()

recurrence_to_freq[[1]] %>% 
  ggplot(aes(max_times, n)) +
  geom_point() +
  theme_classic()

# frequency to recurrence

freq_to_recurrence <-
  map2(df$nouns_data, recurrence_to_freq, ~ {
    .x %>% 
      count(gloss) %>% 
      rename(word = gloss, freq = n) %>% 
      left_join(select(.y, -n), by = "word") %>% 
      arrange(-freq)
  })

## table

library(reactable)

options(reactable.theme = reactableTheme(
  borderColor = "#dfe2e5",
  stripedColor = "#f6f8fa",
  highlightColor = "#f0f5f9",
  cellPadding = "8px 12px",
  style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
  searchInputStyle = list(width = "100%")
  )
)

recurrence_to_freq_df <- df %>% 
  select(-where(is.list)) %>% 
  mutate(
    correlation = map_dbl(recurrence_to_freq, ~ cor(.x$max_times, .x$n)),
    n_recurring = map_dbl(recurrence_to_freq, nrow)
  ) %>% 
  janitor::adorn_rounding(2) %>% 
  mutate(age = glue::glue("{start_age} ~ {end_age}")) %>% 
  select(correlation, childID, age, n_utterances, n_words, n_recurring)

reactable(
  recurrence_to_freq_df,
  details = function(i) {
    htmltools::div(
      style = "padding: 16px; background-color: #f1f1f1",
      reactable(
        freq_to_recurrence[[i]],
        outlined = TRUE,
        fullWidth = FALSE,
        showPageSizeOptions = TRUE
      )
    )
  })



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
  scale_x_continuous(
    limits = c(0, 25.5),
    expand = expansion(c(.02, .03))
  ) +
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

## Replace PLOT_NAME and PLOT_OBJECT
pngfile <- fs::path(
  getwd(), #knitr::fig_path(),
  paste("ridgelines_10", ".png")
)
ragg::agg_png(
  pngfile,
  width = 10,
  height = 16,
  units = "in",
  res = 300,
)
plot(ridge_plot) ; invisible(dev.off())

# ggsave(ridge_plot, "ridgelines_5.pdf", width = 10, height = 16, units = "in", device = cairo_pdf)

