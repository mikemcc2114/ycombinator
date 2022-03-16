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
#url_listing <- company_url$url
#company_listing <- company_url$company_name

# define paths for company name, season, and status tags
path_name <- "/html/body/div/div[2]/div/div/div[1]/div[1]/div[2]/div[1]/h1"
path_season <- "/html/body/div/div[2]/div/div/div[1]/div[1]/div[2]/div[2]/span[1]/text()"
path_status <- "/html/body/div/div[2]/div/div/div[1]/div[1]/div[2]/div[2]/span[2]"

# scrape company descriptions from Y Combinator website based on url vector
ycombinator_data <- tibble(directory_name = character(), 
                           website_name = character(), season = character(), 
                           status = character(), headline = character(), 
                           description = character(), url = character())

for(i in 1:length(company_url$url)){
  cat("Iteration", i, "out of", length(company_url$url), "\n")
  page <- read_html(company_url$url[i])
  directory_name <- company_url$company_name[i]
  website_name <- page %>% html_nodes(xpath = path_name) %>% html_text()
  season <- page %>% html_nodes(xpath = path_season) %>% html_text()
  status <- page %>% html_nodes(xpath = path_status) %>% html_text()
  headline <- page %>% html_nodes("h3") %>% html_text() %>% .[1]
  description <- page %>% html_nodes("p") %>% html_text() %>% .[1]
  ycombinator_data <- ycombinator_data %>%  
    add_row(directory_name = directory_name, website_name = website_name, 
            season = season, status = status, headline = headline,
            description = description, url = company_url$url[i])
}

# write data to csv
write_csv(ycombinator_data, file = here("data", "ycombinator_data.csv"))

# read data from csv
ycombinator_data <- read_csv(here("data", "ycombinator_data.csv"))

### tidy and clean company descriptions

# unnest words, remove numbers/whitespace/punctuation/blank rows/stopwords,
# lemmatize

remove <- "[:digit:]|[:blank:]|[:punct:]"

descriptions <- ycombinator_data %>% 
  select(directory_name, description) %>% 
  unnest_tokens(word, description) %>% 
  mutate(word = replace_non_ascii(word)) %>% 
  mutate(word = str_replace_all(word, remove, "")) %>% 
  filter(word != "") %>% 
  anti_join(stop_words) %>% 
  mutate(word = lemmatize_words(word))

# calculate term frequency per company
description_words <- descriptions %>% 
  count(directory_name, word, sort = TRUE)

# calculate total terms per company  
total_words <- description_words %>% 
  group_by(directory_name) %>% 
  summarise(total = sum(n))

# join tables together
description_words <- left_join(description_words, total_words)

# cast into dtm format and convert to matrix
description_dtm <- description_words %>% 
  cast_dtm(directory_name, word, n)

description_matrix <- as.matrix(description_dtm)

# tf-idf table

description_tf_idf <- description_words %>% 
  bind_tf_idf(word, directory_name, n)

description_tf_idf_sparse <- description_tf_idf %>% 
  cast_sparse(directory_name, word, tf)

### import textbooks - does not work yet - change textbooks into just table of contents!!!

pdf_list <- list.files(here("data", "textbooks"))

textbooks <- tibble(textbook = character(), text = character())

for(i in 1:length(pdf_list)){
  cat("Iteration", i, "out of", length(pdf_list), "\n")
  textbook <-  pdf_list[i]
  text <- here("data", "textbooks", pdf_list[i]) %>%
    pdf_text() %>% str_flatten()
  textbooks <- textbooks %>% add_row(text = text)
}

### import and clean crunchbase funding data

# import funding data csv and add company name column
crunchbase_data <- read.csv(here("data", "CB_funding.csv")) %>% 
  mutate(directory_name = gsub("\\d*_", "", filename)) %>% 
  mutate(directory_name = gsub(".json", "", directory_name))

# filter based on matching name from Y Combinator website, collapse to one row
# per company
funding_data <- crunchbase_data %>% 
  select(directory_name, num_funding_rounds) %>% 
  filter(directory_name %in% company_url$company_name) %>% 
  group_by(directory_name, num_funding_rounds) %>% 
  summarise(n = n_distinct(directory_name))

# join ycombinator with funding data - change to tf-idf table?

joined_data <- left_join(ycombinator_data, funding_data, by = "directory_name")