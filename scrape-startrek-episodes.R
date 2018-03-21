#install.packages("dplyr")
#install.packages("rvest")
library(dplyr)
library(rvest)
setwd("~/GitHub/scrape-startrek-episodes/")


# download script
url <- "http://www.chakoteya.net/StarTrek/1.htm"
contents <- url %>%
  read_html() %>%
  html_node(xpath='/html/body/div/center/table') %>%
  ## remove unnecessary line breaks
  gsub("(\r?\n|\r)+", " ", .) %>% 
  ## encode <br>, </p>, and </font> as actual line breaks
  gsub("<(br|/p|/font)>+?", "|", .) %>% 
  ## remove all unnecessary HTML tags
  gsub("<.+?>", "", .) %>% 
  ## break at | and transform into data.frame
  strsplit("\\|") %>% 
  unlist() %>% 
  data.frame(contents = .) %>% 
  mutate(contents = trimws(contents), 
         ## extract speaker name
         speaker = ifelse(grepl("^(.+?): +?.+?$", contents), 
                          gsub("^(.+?): +?.+?$", "\\1", contents), 
                          ""),
         ## extract speaker note from speaker name
         speaker_note1 = ifelse(grepl("^.+?\\[(.+?)(\\]|})$", speaker), 
                                gsub("^.+?\\[(.+?)(\\]|})$", "\\1", speaker), 
                                ""),
         ## remove speaker note from speaker name
         speaker = gsub("^(.+?) +?\\[.+?(\\]|})$", "\\1", speaker),
         ## extract location
         location = ifelse(grepl("^\\[(.+?)\\]$", contents), 
                           gsub("^\\[(.+?)\\]$", "\\1", contents), 
                           ""), 
         ## extract stage directions
         stage_direction = ifelse(grepl("^\\((.+?)\\)$", contents), 
                                  gsub("^\\((.+?)\\)$", "\\1", contents), 
                                  ""), 
         ## extract spoken lines
         spoken_lines = ifelse(grepl("^.+?: +?(.+?)$", contents), 
                               gsub("^.+?: +?(.+?)$", "\\1", contents), 
                               ""), 
         ## extract additional speaker note from spoken lines
         speaker_note2 = ifelse(grepl("^\\((.+?)\\) +?.+?$", spoken_lines), 
                                gsub("^\\((.+?)\\) +?.+?$", "\\1", spoken_lines), 
                                ""), 
         ## remove speaker note from speaker name
         spoken_lines = gsub("^\\(.+?\\) +?(.+?)$", "\\1", spoken_lines), 
         ## combine speaker notes
         speaker_notes = paste(speaker_note1, speaker_note2), 
         ## fill location to all lines
         location = Reduce(function(x, y) if (y == "") x else y, 
                           location, 
                           accumulate = TRUE)) %>% 
  ## remove empty lines
  filter(speaker != "" | stage_direction != "") %>% 
  ## remove unnecessary columns
  select(-one_of(c("contents", 
                   "speaker_note1", 
                   "speaker_note2")))
