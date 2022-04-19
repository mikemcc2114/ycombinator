library(tidyverse)
library(tm)
library(xml2)
library(rvest)
library(here)
library(dplyr)
library(tidytext)
library(textstem)
library(pdftools)
library(textclean)
library(text2vec)
library(lsa)
library(widyr)
library(kableExtra)
library(sjPlot)

################################################################################
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

# write data to csv
#write_csv(company_url, file = here("data", "company_url.csv"))

# define paths for company name, season, and status tags

path_name <- "/html/body/div/div[2]/div/div/div[1]/div[1]/div[2]/div[1]/h1"
path_location <- "/html/body/div/div[1]/div/div/div[2]/div/div[2]/div[3]/span[2]"

# scrape company locations from Y Combinator website based on url vector
location_data <- tibble(directory_name = character(), location = character())

for(i in 1:length(company_url$url)){
   cat("Iteration", i, "out of", length(company_url$url), "\n")
   page <- read_html(company_url$url[i])
   directory_name <- company_url$company_name[i]
   location <- page %>% html_nodes(xpath = path_location) %>% html_text()
   location_data <- location_data %>%
     add_row(directory_name = directory_name, location = location)
}

# write data to csv
write_csv(location_data, file = here("data", "location_data.csv"))

# read data from csv
location_data <- read_csv(here("data", "location_data.csv"))
