library(knitr)
library(tidyverse)
library(broom)
library(sf)
library(readr)
library(rvest)
library(here)

# Data scraped from: https://www.wta.org/go-outside/hikes?b_start:int=1
# Data scraping algorithm and original dataset taken/adapted from: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-11-24/readme.md
# Hosts of TidyX: https://github.com/thebioengineer/TidyX

# This is the scrapping method used to generate hiking trail data in the state of Washington. THIS SCRAPING METHOD IS UTILIZED FROM THE HOSTS OF TIDYX TIDYTUESDAY DATASET.
scrape_trails <- function(start_int){
  page_url <- paste0(
    "https://www.wta.org/go-outside/hikes?b_start:int=",
    start_int
  )
  
  page_html <- read_html(page_url)
  
  page_html %>% 
    
    html_nodes(".search-result-item") %>% 
    
    map(
      function(hike){
        
        hike_name <- hike %>% html_nodes(".listitem-title") %>% html_nodes("span") %>%  html_text()
        hike_location <- hike %>% html_node("h3") %>% html_text()
        
        hike_stats <- hike %>% html_node(".hike-stats") 
        
        hike_length <- hike_stats %>% html_nodes(".hike-length") %>%html_nodes("span") %>%  html_text()
        hike_gain <- hike_stats %>% html_nodes(".hike-gain") %>%html_nodes("span") %>%  html_text()
        hike_highpoint <- hike_stats %>% html_nodes(".hike-highpoint") %>%html_nodes("span") %>%  html_text()
        hike_rating <- hike_stats %>% html_nodes(".hike-rating") %>%html_nodes(".current-rating") %>%  html_text()
        hike_votes <- hike_stats %>% html_nodes(".hike-rating") %>% html_nodes(".rating-count") %>% html_text() # Added data scraping of the number of ratings per trail
        
        hike_desc <- hike %>% html_nodes(".listing-summary") %>% html_text()
        
        hike_features <- hike %>% html_nodes(".trip-features") %>% html_nodes("img") %>% html_attr("title") %>% list()
        
        tibble(
          name = hike_name,
          location = hike_location,
          length = hike_length,
          gain = hike_gain,
          highpoint = hike_highpoint,
          rating = hike_rating,
          votes = hike_votes,
          features = hike_features,
          description = hike_desc
        )
      }) %>% 
    bind_rows() %>% 
    mutate(description = str_remove(description, "\n") %>% str_squish())
}

start_int <- c(1, seq(30, 3900, by = 30))
# start_int <- c(1, seq(30, 30, by = 30)) #testing

hike_updated_data <- start_int %>% 
  map_dfr(scrape_trails)

saveRDS(hike_updated_data,file = "hike_updated_data.rds")