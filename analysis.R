library(tidyverse)
library(here)
library(extrafont)
library(magrittr)
library(arrow)
library(grid)

theme_set(
  ggthemes::theme_clean(base_size = 12, base_family = "Roboto")
)

arrowdf <- open_dataset(here("data", "tokens_data"))
keys <- read_csv(here("data", "keys.csv"))

moving_window_data <- read_rds(here("data", "moving_windows", "moving_window_5.rds"))


# --- Analysis (Providence corpus)
### Are nouns that recur often more likely to be learned?

# Setup

providence <- moving_window_data %>% 
  filter(corpus == "Providence")

item_trajectories <- read_csv(here("data", "item_trajectory_table.csv")) %>% 
  select(-c(1:3)) %>% 
  pivot_longer(-age) %>% 
  filter(age == 18) %>% 
  mutate(name = str_extract(name, "^.*(?=\\n)")) %>%
  rowwise() %>% 
  mutate(name = ifelse(length(unique(str_extract_all(name, "\\w+")[[1]])) == 1, str_extract(name, "^\\w+"), name)) %>% 
  select(word = name, trajectory = value)

concreteness_df <- read_delim(here("data", "concreteness.txt"), delim = "\t", col_types = "cdddddddc") %>% 
  select(Word, concreteness = Conc.M)


# A1) correlation between normalized size and trajectory in repeating words

providence_repetitions <- providence$moving_window %>% 
  map(~ {
    .x %>% 
      transmute(word, log_freq, normalized_size) %>% 
      left_join(item_trajectories, by = "word") %>%  # add item trajectories by 16 months
      left_join(concreteness_df, by = c("word" = "Word"))
  })

providence_repetitions_corr <- providence_repetitions %>% 
  map_dbl(~ na.omit(.x) %$% cor(normalized_size, trajectory)) %>% 
  enframe(name = NULL) %>% 
  mutate(Name = providence$name)

repetition_trajectory_plot <- providence_repetitions %>% 
  map2_dfr(providence$name, ~mutate(.x, Name = .y)) %>% 
  na.omit() %>% 
  ggplot(aes(normalized_size, trajectory)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_label(
    aes(x = -10, y = .8, label = paste0(round(value, 3), "***")),
    data = providence_repetitions_corr, inherit.aes = FALSE
  ) +
  scale_y_continuous(limits = 0:1, expand = expansion(c(0.05, 0))) +
  lemon::coord_capped_cart(left = "both") +
  facet_wrap(~Name)
  
ggsave(here("img", "repetition_trajectory_plot.png"), repetition_trajectory_plot)


# A2) correlation between normalized size and frequency

providence_repetitions %>% 
  map2_dfr(providence$name, ~mutate(.x, Name = .y)) %>% 
  na.omit() %>% 
  ggplot(aes(normalized_size, log_freq)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_label(
    aes(x = -10, y = -6.5, label = paste0(round(value, 3), "***")),
    data = freq_rep_corr, inherit.aes = FALSE
  ) +
  lemon::coord_capped_cart(left = "both") +
  facet_wrap(~Name)

freq_rep_corr <- providence_repetitions %>% 
  map_dbl(~ na.omit(.x) %$% cor(normalized_size, log_freq)) %>% 
  enframe(name = NULL) %>% 
  mutate(Name = df$name)

# B) correlation between frequency and trajectory in non-repeating words

providence_non_repetitions <- providence$nouns_data %>% 
  map(~{count(.x, word = gloss, name = "frequency") %>% left_join(item_trajectories, by = "word")}) %>% 
  map2(map(df$moving_window, ~.x$word), ~ filter(.x, !word %in% .y)) %>% 
  map2(providence$n_words, ~ mutate(.x, log_freq = log(frequency/.y))) %>% 
  map(~ left_join(.x, concreteness_df, by = c("word" = "Word"))) %>%
  map(~ filter(.x, frequency > 1)) %>% 
  map(~ select(.x, word, log_freq, trajectory, concreteness))

providence_non_repetitions_corr <- providence_non_repetitions %>% 
  map_dbl(~ na.omit(.x) %$% cor(log_freq, trajectory))%>% 
  enframe(name = NULL) %>% 
  mutate(Name = df$name)

non_repetition_trajectory_plot <- providence_non_repetitions %>% 
  map2_dfr(providence$name, ~mutate(.x, Name = .y)) %>% 
  na.omit() %>% 
  ggplot(aes(log_freq, trajectory)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_label(
    aes(x = -11, y = .8, label = paste0(round(value, 3), "***")),
    data = providence_non_repetitions_corr, inherit.aes = FALSE
  ) +
  scale_y_continuous(limits = 0:1, expand = expansion(c(0.05, 0))) +
  lemon::coord_capped_cart(left = "both") +
  facet_wrap(~Name)

ggsave(here("img", "non_repetition_trajectory_plot.png"), non_repetition_trajectory_plot)


# C) Overall freq~trajectory relationship

providence_trajectories <- map2(
  map(providence_repetitions, ~ mutate(.x, repeating = TRUE)),
  map(providence_non_repetitions, ~ mutate(.x, repeating = FALSE)),
  bind_rows
) %>% 
  map2_dfr(providence$name, ~mutate(.x, Name = .y)) %>% 
  relocate(Name, repeating) %>% 
  filter(!is.na(trajectory))

# Noun repetition ratios
providence_trajectories %>%
  group_by(Name) %>%
  count(repeating) %>%
  mutate(prop = n/sum(n)) %>% 
  ungroup()

# Account 1
freq_trajectory_plot1 <- providence_trajectories %>% 
  ggplot(aes(log_freq, trajectory, color = repeating)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_y_continuous(limits = 0:1, expand = expansion(c(0.05, 0))) +
  lemon::coord_capped_cart(left = "both") +
  facet_wrap(~Name, scales = "free_x")

ggsave(here("img", "freq_trajectory_plot1.png"), freq_trajectory_plot1)

# Account 2
freq_trajectory_plot2 <- providence_trajectories %>% 
  ggplot(aes(log_freq, trajectory)) +
  geom_point(aes(color = repeating)) +
  geom_smooth(method = 'lm', color = 'black', se = FALSE, linetype = 2) +
  geom_smooth(aes(color = repeating), method = 'lm') +
  scale_y_continuous(limits = 0:1, expand = expansion(c(0.05, 0))) +
  lemon::coord_capped_cart(left = "both") +
  facet_wrap(~Name, scales = "free_x")

ggsave(here("img", "freq_trajectory_plot2.png"), freq_trajectory_plot2)


# D) Concreteness

concreteness_plot <- providence_trajectories %>% 
  ggplot(aes(concreteness, color = repeating, fill = repeating)) +
  geom_density(aes(y = after_stat(scaled)), alpha = .3) +
  scale_y_continuous(limits = 0:1, expand = expansion(c(0.05, 0))) +
  lemon::coord_capped_cart(left = "both") +
  facet_wrap(~Name, scales = "free_x") +
  ggthemes::theme_clean(base_size = 12, base_family = "Roboto")
