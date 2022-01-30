library(tidyverse)
library(rvest)
library(here)
library(purrr)
library(dplyr)
library(tidytext)
library(wordcloud)
library(ggplot2)

#import list of companies and urls, separate urls into vector
company_url <- read_csv(here("data", "ycombinator_url_listing.csv"))
url_listing <- company_url$url

#define paths for season and company tags
path_season <- "/html/body/div[1]/section/div[1]/div[1]/div[2]/span"
path_founded <- "/html/body/div[1]/section[1]/div[2]/div/div[2]/div[1]/span"
path_location <- "/html/body/div[1]/section/div[2]/div/div[2]/div[3]/span"

#define function - reads name, headline, description into tibble
scraper <- function(url){
  page <- read_html(url)
  name <- page %>% html_nodes("h1.font-bold") %>% html_text()
  season <- page %>% html_nodes(xpath = path_season) %>% html_text()
  year_founded <- page %>% html_nodes(xpath = path_founded) %>% html_text()
  location <- page %>% html_nodes(xpath = path_location) %>% html_text()
  headline <- page %>% html_nodes("h3") %>% html_text() %>% .[1]
  description <- page %>% html_nodes("p") %>% html_text() %>% .[1]
  tibble(name, season, year_founded, location, headline, description, url)
}

#map scraper function to url_listing vector (warning: takes 20+ min to download)
data <- map_dfr(.x = company_url$url, .f = scraper)

#write to csv
write_csv(data, file = here("data", "ycombinator_data.csv"))

#select name/season/description and convert to tidy format
tidy_description <- data %>% 
  select(name, season, description) %>% 
  unnest_tokens(word, description)

#view most common words
tidy_description %>% count(word, sort = TRUE)

#remove stop words based on snowball lexicon
cleaned_tidy_description <- tidy_description %>% 
  anti_join(get_stopwords())

#view new most common words
cleaned_tidy_description %>% count(word, sort = TRUE)

#make wordcloud
cleaned_tidy_description %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 50))

