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

################################################################################
### import textbook extracts

pdf_list <- list.files(here("data", "textbooks"))
textbooks <- tibble(textbook = character(), text = character())

for(i in 1:length(pdf_list)){
  cat("Iteration", i, "out of", length(pdf_list), "\n")
  textbook <-  pdf_list[i]
  text <- here("data", "textbooks", pdf_list[i]) %>%
    pdf_text() %>% str_flatten()
  textbooks <- textbooks %>% add_row(textbook = textbook,
                                     text = text)
}

# remove extraneous title info and rename columns to match company tibble
textbooks <- textbooks %>% 
  mutate(textbook = textbook %>% 
           str_remove_all("extract_|.pdf")) %>% 
  rename(directory_name = textbook, description = text)

################################################################################
### combine company descriptions and textbook extracts

data <- ycombinator_data %>% select(directory_name, description) %>% 
  filter(!is.na(description)) %>% 
  add_row(textbooks) %>% 
  rename(document = directory_name, text = description)

################################################################################
### tidy and clean documents

# unnest words, remove numbers/whitespace/punctuation/blank rows/stopwords,
# lemmatize

remove <- "[:digit:]|[:blank:]|[:punct:]"

data_clean <- data %>% 
  select(document, text) %>% 
  unnest_tokens(word, text) %>% 
  mutate(word = replace_non_ascii(word)) %>% 
  mutate(word = str_replace_all(word, remove, "")) %>% 
  filter(word != "") %>% 
  anti_join(stop_words) %>% 
  mutate(word = lemmatize_words(word))

# calculate term frequency per company
tf <- data_clean %>% 
  count(document, word, sort = TRUE)

# calculate total terms per company  
total_words <- tf %>% 
  group_by(document) %>% 
  summarise(total = sum(n))

# join tables together
tf <- left_join(tf, total_words)

# cast into dtm format and convert to matrix
dtm <- tf %>% 
  cast_dtm(document, word, n)
inspect(dtm)

dtm_matrix <- as.matrix(dtm)

# tf-idf table

tf_idf <- tf %>% 
  bind_tf_idf(word, document, n)

tf_idf_sparse <- tf_idf %>% 
  cast_sparse(document, word, tf)


################################################################################
### compute cosine similarity scores between description and textbooks

# use this? https://www.brodrigues.co/blog/2019-06-04-cosine_sim/
# or this? https://www.r-bloggers.com/2018/09/who-wrote-the-anti-trump-new-york-times-op-ed-using-tidytext-to-find-document-similarity/

similarities <- sim2(tf_idf_sparse, method = "cosine", norm = "l2")


################################################################################
### attempt with prof data and cleaning processes

# extract vectors for each textbook
x_ent <- dtm_matrix["ent",]
x_fin <- dtm_matrix["fin",]
x_ldr <- dtm_matrix["ldr",]
x_mkt <- dtm_matrix["mkt",]
x_str <- dtm_matrix["str",]

i = 1
cosine_table <- tibble(document = character(), ent_sim = numeric())
for(i in 1:length(data$document)){
  cat("Iteration", i, "out of", length(data$document), "\n")
  document = data$document[i]
  y <- dtm_matrix[document,]
  
  ent_sim <- cosine(x_ent, y)
  fin_sim <- cosine(x_sim, y)
  ldr_sim <- cosine(x_ldr, y)
  mkt_sim <- cosine(x_mkt, y)
  str_sim <- cosine(x_str, y)
  
  cosine_table <- cosine_table %>%  
    add_row(document = document, ent_sim = ent_sim)
}




y_airbnb <- dtm_matrix["80-000-hours",]

cosine(x_ent, y_airbnb)

cosin_table <- data %>% select(document) %>% 
  mutate(ent_cosin = cosine(x_ent, dtm_matrix[document,]))

# do this with a for loop instead?

################################################################################
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