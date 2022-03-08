library(tidyverse)
library(xml2)
library(rvest)
library(here)
library(dplyr)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(textstem)

### scrape company descriptions

# Y Combinator website sitemap
sitemap <- "https://www.ycombinator.com/companies//sitemap.xml"
pattern <- "https://www.ycombinator.com/companies/"

# read sitemap XML and convert into dataframe and vector of company name and urls
company_url <- sitemap %>% read_xml() %>% as_list() %>% 
  as_tibble() %>% unnest_longer(urlset) %>% 
  filter(urlset_id == "loc") %>% 
  unnest(cols = names(.)) %>% unnest(urlset) %>% rename("url" = "urlset") %>% 
  mutate(company_name = gsub("https://www.ycombinator.com/companies/", "", url))
url_listing <- company_url$url
company_listing <- company_url$company_name

# define paths for company name, season, and status tags
path_name <- "/html/body/div/div[2]/div/div/div[1]/div[1]/div[2]/div[1]/h1"
path_season <- "/html/body/div/div[2]/div/div/div[1]/div[1]/div[2]/div[2]/span[1]/text()"
path_status <- "/html/body/div/div[2]/div/div/div[1]/div[1]/div[2]/div[2]/span[2]"

# scrape company descriptions from Y Combinator website based on url vector
ycombinator_data <- tibble(name = character(), season = character(), status = character(),
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
  ycombinator_data <- ycombinator_data %>%  
    add_row(name = name, season = season, status = status, headline = headline,
            description = description, url = url_listing[i])
}

# write data to csv
write_csv(ycombinator_data, file = here("data", "ycombinator_data.csv"))

# read data from csv
ycombinator_data <- read_csv(here("data", "ycombinator_data.csv"))

### import and clean crunchbase funding data

# import funding data csv and add company name column
crunchbase_data <- read.csv(here("data", "CB_funding.csv")) %>% 
  mutate(company_name = gsub("\\d*_", "", filename)) %>% 
  mutate(company_name = gsub(".json", "", company_name))

# filter based on matching name from Y Combinator website
funding_data <- crunchbase_data %>% 
  filter(company_name %in% company_listing)

  


