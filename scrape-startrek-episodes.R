#install.packages("dplyr")
#install.packages("rvest")
#install.packages("xlsx")
library(dplyr)
library(rvest)
library(xlsx)
setwd("~/GitHub/scrape-startrek-episodes/")


# download script
url <- "http://www.chakoteya.net/StarTrek/1.htm"
contents <- url %>%
  read_html() %>%
  html_node(xpath='/html/body/div/center/table') %>%
  html_table() %>% 
  ## mark actual line breaks with |
  gsub("(\r?\n|\r)+([A-Z]+?:) ", "|\\2", .) %>% 
  ## remove non-spoken lines
  gsub("(\r?\n|\r)+(\\(.+?\\)) ?", "|\\2", .) %>% 
  gsub("(\r?\n|\r)+(\\[.+?\\]) ?", "|\\2", .) %>% 
  ## remove unnecessary line breaks
  gsub("(\r?\n|\r)+", " ", .) %>% 
  ## break at |
  strsplit("\\|") %>% 
  unlist()
contents 
