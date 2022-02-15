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

# Y Combinator website sitemap
sitemap <- "https://www.ycombinator.com/companies//sitemap.xml"
pattern <- "https://www.ycombinator.com/companies/"

# read sitemap XML and convert into dataframe and vector of company urls
company_url <- sitemap %>% read_xml() %>% as_list() %>% 
  as_tibble() %>% unnest_longer(urlset) %>% 
  filter(urlset_id == "loc") %>% 
  unnest(cols = names(.)) %>% unnest(urlset) %>% rename("url" = "urlset") 
url_listing <- company_url$url

# define paths for company name, season, and status tags
path_name <- "/html/body/div/div[2]/div/div/div[1]/div[1]/div[2]/div[1]/h1"
path_season <- "/html/body/div/div[2]/div/div/div[1]/div[1]/div[2]/div[2]/span[1]/text()"
path_status <- "/html/body/div/div[2]/div/div/div[1]/div[1]/div[2]/div[2]/span[2]"

# data scraped using for loop - this one works
data <- tibble(name = character(), season = character(), status = character(),
               headline = character(), description = character(), 
               url = character())

for(i in 1:length(url_listing)){
  cat("Iteration", i, "out of", length(url_listing), "\n")
  page <- read_html(url_listing[i])
  name <- page %>% html_nodes(xpath = path_name) %>% html_text()
  season <- page %>% html_nodes(xpath = path_season) %>% html_text()
  status <- page %>% html_nodes(xpath = path_status) %>% html_text()
  headline <- page %>% html_nodes("h3") %>% html_text() %>% .[1]
  description <- page %>% html_nodes("p") %>% html_text() %>% .[1]
  data <- data %>%  
    add_row(name = name, season = season, status = status, headline = headline,
            description = description, url = url_listing[i])
}

# data scraped using map_dfr - does not work, generates error after running
# define scraper function
scraper <- function(url_listing){
  page <- read_html(url_listing)
  name <- page %>% html_nodes(xpath = path_name) %>% html_text()
  season <- page %>% html_nodes(xpath = path_season) %>% html_text()
  status <- page %>% html_nodes(xpath = path_status) %>% html_text()
  headline <- page %>% html_nodes("h3") %>% html_text() %>% .[1]
  description <- page %>% html_nodes("p") %>% html_text() %>% .[1]
  tibble(name, season, status, headline, description, url)
}

# map scraper function to url listing vector (warning: takes 20+ min to download)
data2 <- map_dfr(.x = url_listing, .f = scraper)

# map scraper function to url_listing using "possibly" to catch failures
possibly <- possibly(scraper, otherwise="This page could not be accessed")
data3 <- map_dfr(.x = url_listing[1:10], .f = possibly)
