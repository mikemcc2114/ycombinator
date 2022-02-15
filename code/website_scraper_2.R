library(tidyverse)
library(xml2)
library(rvest)
library(here)
library(purrr)
library(dplyr)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(textstem)

# create list of companies from ycombinator sitemap (below) 
# i did by pasting into CSV and filtering - for project write code to do so

# Ycombinator website sitemap
sitemap <- "https://www.ycombinator.com/companies//sitemap.xml"
pattern <- "https://www.ycombinator.com/companies/"

# read sitemap XML and convert into dataframe of company urls
url_df <- sitemap %>% read_xml() %>% as_list() %>% 
  as_tibble() %>% unnest_longer(urlset) %>% 
  filter(urlset_id == "loc") %>% 
  unnest(cols = names(.)) %>% unnest(urlset) %>% rename("url" = "urlset") 
  
url_df2 <- url_df %>% slice(1:20)
company_url2 <- company_url %>% slice(1:20)

list <- url_df$url[1:10]
  
  
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
  Sys.sleep(.1)
}

#map scraper function to url_listing vector (warning: takes 20+ min to download)
possibly <- possibly(scraper, otherwise="This page could not be accessed")
data <- map_dfr(.x = company_url$url, .f = possibly)


#write to csv
write_csv(data, file = here("data", "ycombinator_data.csv"))

#import data from file
data <- read_csv(here("data", "ycombinator_data.csv"))

#select companies without description (333)
no_description <- data %>% filter(is.na(description))

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

#filter data with descriptions
description <- data %>% 
  filter(!is.na(description))

#group by season, remove stopwords and slice 10 most common words
most_common_words <- description %>% 
  unnest_tokens(word, description) %>% 
  anti_join(get_stopwords()) %>% 
  group_by(season) %>% 
  count(word) %>% 
  slice_max(n, n = 20)

#remove stopwords, lemmatize, group by season, slice 10 most common
lemmatized_most_common_words <- description %>% 
  unnest_tokens(word, description) %>% 
  anti_join(get_stopwords()) %>% 
  mutate(word_lemma = lemmatize_words(word)) %>% 
  group_by(season) %>% 
  count(word_lemma) %>% 
  slice_max(n, n = 10)

#lemmatized words with word cloud
lemmatized_words <- description %>% 
  unnest_tokens(word, description) %>% 
  anti_join(get_stopwords()) %>% 
  mutate(word_lemma = lemmatize_words(word))

lemmatized_words %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 50))

