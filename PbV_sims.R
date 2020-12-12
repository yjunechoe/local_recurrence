####### Manual load
## rollins_training <- readRDS("rollins_training.rds")
## gold_parsed <- read_csv("gold_training_parsed.csv")
#######

set.seed(1234)

# ---- Models

PbV_easy_easy <- function(corpus = rollins_training, alpha0 = 0, alpha = 1) {
  
  # Create empty list for storing hypotheses
  meanings <- list()
  
  # Incrementally loop through each word in the corpus
  for (i in 1:nrow(corpus)) {
    
    # Grab the word and the co-present referents
    word <- corpus$Word[i]
    referents <- corpus$Referent[i][[1]]
    
    # First time seeing the word?
    if (!word %in% names(meanings)) {
      # Randomly pick a hypothesis from the referents present
      meanings[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE)
    } 
    
    else {
      
      # Current hypothesis not among the referents?
      if (!meanings[[word]]$Meaning %in% referents) {
        # Randomly pick a hypothesis from the referents present
        meanings[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE)
      }
      
      else {
        
        # Current hypothesis already been confirmed?
        if (meanings[[word]]$Confirmed) {
          
          # Fail to retrieve confirmed hypothesis?
          if (runif(1) > alpha) {
            # Randomly pick a hypothesis from the referents present
            meanings[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE)
          }
          
        }
        
        else {
          
          # Fail to retrieve the unconfirmed hypothesis?
          if (runif(1) < alpha0) {
            # Randomly pick a hypothesis from the referents present
            meanings[[word]]$Meaning <- sample(referents, 1)
          }
          
          else {
            # Confirm the current hypothesis
            meanings[[word]]$Confirmed <- TRUE
          }
          
        }
        
      }
      
    }
    
  }
  
  # A dataframe of all hypotheses proposed
  result <- tibble(
    Word = names(meanings),
    Learned = map_chr(meanings, 1)
  )
  
  # Return only those hypotheses that have been confirmed (= learned)
  result
  
}


PbV_easy_hard <- function(corpus = rollins_training, alpha0 = 0, alpha = 1, window = 10) {
  
  # Create empty list for storing hypotheses
  meanings <- list()
  alternatives <- list()
  
  # Incrementally loop through each word in the corpus
  for (i in 1:nrow(corpus)) {
    
    # Grab the word and the co-present referents
    word <- corpus$Word[i]
    referents <- corpus$Referent[i][[1]]
    
    # First time seeing the word?
    if (!word %in% names(meanings)) {
      # Randomly pick a hypothesis from the referents present
      meanings[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE)
    } 
    
    else {
      
      # Current hypothesis not among the referents?
      if (!meanings[[word]]$Meaning %in% referents) {
        
        # If unconfirmed, randomly pick a hypothesis from the referents present
        if(!meanings[[word]]$Confirmed) {
          meanings[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE)
        }
        
        else {
          # If first time counter-proposing, store in buffer; don't overwrite yet
          if (is.null(alternatives[[word]])) {
            alternatives[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE, counter = 0)
          }
          # If counter proposal confirmed, overwrite
          else if (alternatives[[word]]$Meaning %in% referents) {
            meanings[[word]] <- alternatives[[word]]
            meanings[[word]]$Confirmed <- TRUE
            alternatives[[word]] <- NULL
          }
        }
      }
      
      else {
        
        # Current hypothesis already been confirmed?
        if (meanings[[word]]$Confirmed) {
          
          # Fail to retrieve confirmed hypothesis?
          if (runif(1) > alpha) {
            # Randomly pick a hypothesis from the referents present
            meanings[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE)
          }
          
        }
        
        else {
          
          # Fail to retrieve the unconfirmed hypothesis?
          if (runif(1) < alpha0) {
            # Randomly pick a hypothesis from the referents present
            meanings[[word]]$Meaning <- sample(referents, 1)
          }
          
          else {
            # Confirm the current hypothesis
            meanings[[word]]$Confirmed <- TRUE
          }
          
        }
        
      }
      
    }
    
    # End of utterance updating
    if (corpus$Utterance[i] != corpus$Utterance[i+1] | i == nrow(corpus)) {
      
      # Update counters for alternatives
      for (k in names(alternatives)) {
        alternatives[[k]]$counter <- alternatives[[k]]$counter + 1
        # if a hypothesis is still not confirmed within window, discard it
        if (alternatives[[k]]$counter == window & !alternatives[[k]]$Confirmed) {
          alternatives[[k]] <- NULL
        }
      }
      
    }
    
  }
  
  # A dataframe of all hypotheses proposed
  result <- tibble(
    Word = names(meanings),
    Learned = map_chr(meanings, 1)
  )
  
  # Return only those hypotheses that have been confirmed (= learned)
  result
  
}



PbV_hard_easy <- function(corpus = rollins_training, alpha0 = 0, alpha = 1, window = 10) {
  
  # Create empty list for storing hypotheses
  meanings <- list()
  
  # Incrementally loop through each word in the corpus
  for (i in 1:nrow(corpus)) {
    
    # Grab the word and the co-present referents
    word <- corpus$Word[i]
    referents <- corpus$Referent[i][[1]]
    
    # First time seeing the word?
    if (!word %in% names(meanings)) {
      # Randomly pick a hypothesis from the referents present
      meanings[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE, counter = 0)
    } 
    
    else {
      
      # Current hypothesis not among the referents?
      if (!meanings[[word]]$Meaning %in% referents) {
        # Randomly pick a hypothesis from the referents present
        meanings[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE, counter = 0)
      }
      
      else {
        
        # Current hypothesis already been confirmed?
        if (meanings[[word]]$Confirmed) {
          
          # Fail to retrieve confirmed hypothesis?
          if (runif(1) > alpha) {
            # Randomly pick a hypothesis from the referents present
            meanings[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE, counter = 0)
          }
          
        }
        
        else {
          
          # Fail to retrieve the unconfirmed hypothesis?
          if (runif(1) < alpha0) {
            # Randomly pick a hypothesis from the referents present
            meanings[[word]]$Meaning <- sample(referents, 1)
          }
          
          else {
            # Confirm the current hypothesis
            meanings[[word]]$Confirmed <- TRUE
          }
          
        }
        
      }
      
    }
    
    # End of utterance updating
    if (corpus$Utterance[i] != corpus$Utterance[i+1] | i == nrow(corpus)) {
    
      # Update counters
      for (j in names(meanings)) {
        meanings[[j]]$counter <- meanings[[j]]$counter + 1
        # if a hypothesis is still not confirmed within window, discard it
        if (meanings[[j]]$counter == window & !meanings[[j]]$Confirmed) {
          meanings[[j]] <- NULL
        }
      }
    
    }
      
  }
  
  # A dataframe of all hypotheses proposed
  result <- tibble(
    Word = names(meanings),
    Learned = map_chr(meanings, 1)
  )
  
  # Return only those hypotheses that have been confirmed (= learned)
  result[map_lgl(meanings, "Confirmed"), ]
  
}


PbV_hard_hard <- function(corpus = rollins_training, alpha0 = 0, alpha = 1, window = 10) {
  
  # Create empty list for storing hypotheses
  meanings <- list()
  alternatives <- list()
  
  
  # Incrementally loop through each word in the corpus
  for (i in 1:nrow(corpus)) {
    
    # Grab the word and the co-present referents
    word <- corpus$Word[i]
    referents <- corpus$Referent[i][[1]]
    
    # First time seeing the word?
    if (!word %in% names(meanings)) {
      # Randomly pick a hypothesis from the referents present
      meanings[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE, counter = 0)
    } 
    
    else {
      
      # Current hypothesis not among the referents?
      if (!meanings[[word]]$Meaning %in% referents) {
        
        if (is.null(alternatives[[word]])) {
          # Randomly pick a hypothesis from the referents present
          alternatives[[word]] <- list(Meaning = sample(referents, 1), counter = 0)
        }
        
        else {
          
          if (alternatives[[word]]$Meaning %in% referents) {
            meanings[[word]] <- alternatives[[word]]
            meanings[[word]]$Confirmed <- TRUE
            alternatives[[word]] <- NULL
          }
          
          else {
            # Randomly pick a hypothesis from the referents present
            alternatives[[word]] <- list(Meaning = sample(referents, 1), counter = 0)
          }
          
        }
        
      }
      
      else {
        
        # Current hypothesis already been confirmed?
        if (meanings[[word]]$Confirmed) {
          
          # Fail to retrieve confirmed hypothesis?
          if (runif(1) > alpha) {
            # Randomly pick a hypothesis from the referents present
            meanings[[word]] <- list(Meaning = sample(referents, 1), Confirmed = FALSE, counter = 0)
          }
          
        }
        
        else {
          
          # Fail to retrieve the unconfirmed hypothesis?
          if (runif(1) < alpha0) {
            # Randomly pick a hypothesis from the referents present
            meanings[[word]]$Meaning <- sample(referents, 1)
          }
          
          else {
            # Confirm the current hypothesis
            meanings[[word]]$Confirmed <- TRUE
          }
          
        }
        
      }
      
    }
    
    # End of utterance updating
    if (corpus$Utterance[i] != corpus$Utterance[i+1] | i == nrow(corpus)) {
      
      # Update counters
      for (j in names(meanings)) {
        meanings[[j]]$counter <- meanings[[j]]$counter + 1
        # if a hypothesis is still not confirmed within window, discard it
        if (meanings[[j]]$counter == window & !meanings[[j]]$Confirmed) {
          meanings[[j]] <- NULL
        }
      }
  
      # Update counters for alternatives
      for (k in names(alternatives)) {
        alternatives[[k]]$counter <- alternatives[[k]]$counter + 1
        # if a hypothesis is still not confirmed within window, discard it
        if (alternatives[[k]]$counter == window) {
          alternatives[[k]] <- NULL
        }
      }    
    
    }
      
  }
  
  # A dataframe of all hypotheses proposed
  result <- tibble(
    Word = names(meanings),
    Learned = map_chr(meanings, 1)
  )
  
  # Return only those hypotheses that have been confirmed (= learned)
  result[map_lgl(meanings, "Confirmed"), ]
  
}




# ---- Simulations

with_progress({
  PbV_sims_easy_easy <- run_sim(PbV_easy_easy, 1000)
  PbV_sims_easy_easy_metrics <- eval_sims(PbV_sims_easy_easy)
})

with_progress({
  PbV_sims_easy_hard <- run_sim(PbV_easy_hard, 1000, window = 2)
  PbV_sims_easy_hard_metrics <- eval_sims(PbV_sims_easy_hard)
})

with_progress({
  PbV_sims_hard_easy <- run_sim(PbV_hard_easy, 1000, window = 3)
  PbV_sims_hard_easy_metrics <- eval_sims(PbV_sims_hard_easy)
})

with_progress({
  PbV_sims_hard_hard <- run_sim(PbV_hard_hard, 1000, window = 2)
  PbV_sims_hard_hard_metrics <- eval_sims(PbV_sims_hard_hard)
})



# ---- Table

F1_list <- list(
  future_map_dfr(PbV_sims_easy_easy, eval_algo)$F1,
  future_map_dfr(PbV_sims_easy_hard, eval_algo)$F1,
  future_map_dfr(PbV_sims_hard_easy, eval_algo)$F1,
  future_map_dfr(PbV_sims_hard_hard, eval_algo)$F1
)

N_list <- list(
  future_map_dbl(PbV_sims_easy_easy, nrow),
  future_map_dbl(PbV_sims_easy_hard, nrow),
  future_map_dbl(PbV_sims_hard_easy, nrow),
  future_map_dbl(PbV_sims_hard_hard, nrow)
)


tbl_df <- crossing(Accept = c("Easy", "Hard"), Shift = c("Easy", "Hard")) %>% 
  bind_cols(
    bind_rows(
      PbV_sims_easy_easy_metrics,
      PbV_sims_easy_hard_metrics,
      PbV_sims_hard_easy_metrics,
      PbV_sims_hard_hard_metrics
    )
  )
  
library(kableExtra)

tbl_df %>% 
  mutate(
    `F1-Dist` = "",
    across(where(is.double), ~round(.x, 3)),
    N = round(map_dbl(N_list, mean), 1)
  ) %>% 
  kbl() %>% 
  kable_classic(full_width = FALSE) %>% 
  add_header_above(c("Model Specification" = 2, "Performance Metrics" = 5)) %>% 
  column_spec(3, background = c("white", "white", "#ADADADFF", "white")) %>% 
  column_spec(4, background = c("#ADADADFF", "white", "white", "white")) %>% 
  column_spec(5, background = c("white", "white", "#ADADADFF", "white")) %>% 
  column_spec(6, image = spec_hist(F1_list, breaks = c(7, 7, 10, 10), col = "grey", border = "black")) %>%
  kable_styling(
    bootstrap_options = "none", 
    font_size = 22,
    html_font = "CMU Typewriter Text"
  ) %>% 
  as_image(file = here::here("img", "PbV_repetitions.png"))

