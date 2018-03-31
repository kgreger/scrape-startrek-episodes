#install.packages("dplyr")
#install.packages("rvest")
library(dplyr)
library(rvest)
setwd("~/GitHub/scrape-startrek-episodes/")

# initialize
franchise <- c("The Original Series", 
               "The Next Generation", 
               "Deep Space Nine", 
               "Voyager", 
               "Enterprise")
episode_lists <- c("http://www.chakoteya.net/StarTrek/episodes.htm", 
                   "http://www.chakoteya.net/NextGen/episodes.htm", 
                   "http://www.chakoteya.net/DS9/episodes.htm", 
                   "http://www.chakoteya.net/Voyager/episode_listing.htm", 
                   "http://www.chakoteya.net/Enterprise/episodes.htm")
link_list <- data.frame(url.hef = character(), 
                        url.name = character(), 
                        franchise = character())


# gather links to episode scripts from episode listings
## collect links
for(i in 1:length(episode_lists)) {
  url <- episode_lists[i]
  base_url <- gsub("(http://www.chakoteya.net/(.+?)/)(.+?).htm", "\\1", url)
  ## download page HTML
  html <- url %>%
    read_html()
  ## extract links
  links <- html %>% 
    html_nodes("a") 
  links <- bind_rows(lapply(xml_attrs(links), 
                            function(x) data.frame(as.list(x), 
                                                   stringsAsFactors = FALSE)))$href %>% 
    paste0(base_url, .)
  links <- data.frame(url = links, 
                      franchise = franchise[i], 
                      stringsAsFactors = FALSE)
  link_list <- rbind(link_list, links)
}
## clean up link list to keep only true episode script links
link_list <- link_list %>% 
  filter(substr(link_list$url, 
                nchar(link_list$url) - 2, 
                nchar(link_list$url)) == "htm")



# download scripts
for(i in 1:nrow(link_list)) {
  cat(paste0(link_list$franchise[i], ": ", link_list$url[i], ": "))
  url <- link_list$url[i]
  ## download page HTML
  html <- url %>%
    read_html()
  ## extract metadata
  episode_id <- gsub("http://www.chakoteya.net/(.+?)/(.+?).htm", "\\2", url)
  franchise <- link_list$franchise[i]
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
  air_date <- ifelse(grepl("^Stardate: +?(.+?) +?Original Airdate: +?(.+?)$", dates), 
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
           ## remove quotation marks
           contents = gsub("\"", "", contents), 
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
           location = ifelse(captains_log != "", "", location), 
           ## add metadata
           episode_id = episode_id, 
           franchise = franchise, 
           episode_title = episode_title, 
           stardate = stardate, 
           air_date = air_date) %>% 
    ## remove empty lines
    filter(speaker != "" | stage_direction != "" | captains_log != "") %>% 
    ## remove unnecessary columns
    select(-one_of(c("contents", 
                     "speaker_note1", 
                     "speaker_note2")))
  
  cat(paste0(nrow(contents), " lines\n"))
  
  ## write results to disk
  if(i == 1) {
    write.table(contents, 
                file = "star-trek-scripts.csv", 
                append = FALSE, 
                sep = ";", 
                row.names = FALSE)
  } else {
    write.table(contents, 
                file = "star-trek-scripts.csv", 
                append = TRUE, 
                sep = ";", 
                row.names = FALSE, 
                col.name = FALSE)
  }
}
