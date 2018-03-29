## load necessary libraries
library(dplyr)
library(tidytext)
library(sentiment)
setwd("~/GitHub/scrape-startrek-episodes/")


## load and parse script file
scripts <- read.table("star-trek-scripts.csv", 
                      header = TRUE, 
                      sep =";", 
                      stringsAsFactors = FALSE) %>% 
  filter(spoken_lines != "") %>% 
  select(-captains_log, 
         -stage_direction, 
         -speaker_notes)


## prepare stopword dictionary (i.e. remove duplicates from multiple lexicons)
stop_words <- stop_words %>% 
  group_by(word) %>% 
  summarize() %>% 
  mutate(stopword = TRUE)


## tokenize script for text analysis
token <- scripts %>% 
  unnest_tokens(word, 
                spoken_lines) %>% 
  # add unique token identifier
  mutate(tokenid = row_number())

## extract unique tokens
token_unique <- token %>% 
  # extract words only
  select(word) %>% 
  # boil down to unique words
  unique() %>% 
  # add unique token identifier
  mutate(polarity = get_polarity(word), 
         sentiment = get_emotion(word)) %>% 
  # add stopword classifier
  left_join(stop_words, 
            by = "word")

## join tokens with unique tokens' sentiment & polarity
token <- token %>% 
  left_join(token_unique, 
            by = "word")

## export tokenized version of script
write.table(token, 
            file = "star-trek-scripts-tokenized.csv", 
            append = FALSE, 
            sep = ";", 
            row.names = FALSE)
