#install.packages("dplyr")
#install.packages("rvest")
library(dplyr)
library(rvest)
setwd("~/GitHub/scrape-startrek-episodes/")


# download script
url <- "http://www.chakoteya.net/StarTrek/6.htm"
## download page HTML
html <- url %>%
  read_html()
## extract metadata
episode_title <- html %>% 
  html_node(xpath = "/html/body/p[1]/font[1]") %>% 
  html_text() %>% 
  ## remove unnecessary line breaks
  gsub("(\r?\n|\r)+", " ", .)
dates <- html %>% 
  html_node(xpath = "/html/body/p[1]/font[2]") %>% 
  html_text() %>% 
  ## remove unnecessary line breaks
  gsub("(\r?\n|\r)+", " ", .)
stardate <- ifelse(grepl("^Stardate: +?(.+?) +?Original Airdate: +?(.+?)$", dates), 
                   gsub("^Stardate: +?(.+?) +?Original Airdate: +?(.+?)$", "\\1", dates), 
                   "") %>% 
  trimws()
airdate <- ifelse(grepl("^Stardate: +?(.+?) +?Original Airdate: +?(.+?)$", dates), 
                  gsub("^Stardate: +?(.+?) +?Original Airdate: +?(.+?)$", "\\2", dates), 
                  "") %>% 
  trimws() %>% 
  gsub(",", "", .)

## extract script
contents <- html %>%
  html_node(xpath = "/html/body/div/center/table") %>%
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
         ## extract captain's log
         captains_log = ifelse(grepl("^(Captain's log.+?)$", contents), 
                               gsub("^(Captain's log.+?)$", "\\1", contents), 
                               ""),
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
                           accumulate = TRUE), 
         ## remove location from captain's log
         location = ifelse(captains_log != "", "", location)) %>% 
  ## remove empty lines
  filter(speaker != "" | stage_direction != "" | captains_log != "") %>% 
  ## remove unnecessary columns
  select(-one_of(c("contents", 
                   "speaker_note1", 
                   "speaker_note2")))
