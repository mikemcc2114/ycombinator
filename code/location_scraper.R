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


# define path for company location
path_location <- "/html/body/div/div[1]/div/div/div[2]/div/div[2]/div[3]/span[2]"

# scrape company locations from Y Combinator website based on url vector
location_data <- tibble(directory_name = character(), location = character())

# for(i in 1377:length(company_url$url)){
#    cat("Iteration", i, "out of", length(company_url$url), "\n")
#    page <- read_html(company_url$url[i])
#    directory_name <- company_url$company_name[i]
#    location <- page %>% html_nodes(xpath = path_location) %>% html_text()
#    location_data <- location_data %>%
#      add_row(directory_name = directory_name, location = location)
# }

# write data to csv
#write_csv(location_data, file = here("data", "location_data.csv"))

# read data from csv
location_data <- read_csv(here("data", "location_data.csv"))

# summary table
location_summary <- location_data %>% 
  count(location)

# write data to csv, then manually categorize
#write_csv(location_summary, file = here("data", "location_summary.csv"))

# read manually categorized summary
location_summary <- read_csv(here("data", "location_summary.csv"))

# break down data by country
country_breakdown <- location_summary %>%
  group_by(country) %>% 
  summarize(count = sum(n))

# top 5 countries
top_5_countries <- country_breakdown %>%
  slice_max(count, n = 5)

top_5_countries_plot <- ggplot(top_5_countries, aes(x = reorder(country, -count), y = count)) +
  geom_col() +
  labs(x = "country", title = "Top five countries where Y Combinator companies are founded")

# break down data by US states
state_breakdown <- location_summary %>%
  filter(country == "USA") %>% 
  group_by(state) %>% 
  summarize(count = sum(n))

# top 5 states
top_5_states <- state_breakdown %>%
  slice_max(count, n = 5)

top_5_states_plot <- ggplot(top_5_states, aes(x = reorder(state, -count), y = count)) +
  geom_col() +
  labs(x = "state", title = "Top five states where Y Combinator companies are founded")
