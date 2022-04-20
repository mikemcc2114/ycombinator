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
library(MASS)
library(effects)

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
path_season <- "/html/body/div/div[2]/div/div/div[1]/div[1]/div[2]/div[2]/span[1]/text()"
path_status <- "/html/body/div/div[2]/div/div/div[1]/div[1]/div[2]/div[2]/span[2]"

# scrape company descriptions from Y Combinator website based on url vector
ycombinator_data <- tibble(directory_name = character(), 
                           website_name = character(), season = character(), 
                           status = character(), headline = character(), 
                           description = character(), url = character())
# 
#  for(i in 1:length(company_url$url)){
#    cat("Iteration", i, "out of", length(company_url$url), "\n")
#    page <- read_html(company_url$url[i])
#    directory_name <- company_url$company_name[i]
#    website_name <- page %>% html_nodes(xpath = path_name) %>% html_text()
#    season <- page %>% html_nodes(xpath = path_season) %>% html_text()
#    status <- page %>% html_nodes(xpath = path_status) %>% html_text()
#    headline <- page %>% html_nodes("h3") %>% html_text() %>% .[1]
#    description <- page %>% html_nodes("p") %>% html_text() %>% .[1]
#    ycombinator_data <- ycombinator_data %>%  
#      add_row(directory_name = directory_name, website_name = website_name, 
#              season = season, status = status, headline = headline,
#              description = description, url = company_url$url[i])
#    Sys.sleep(.01)
# }

# write data to csv

# read previously downloaded data from csv
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
### descriptive statistics

# ycombinator initial data

yco_init <- ycombinator_data %>% select(directory_name, description) %>%
  mutate(description = str_trim(description, side = "both")) %>% 
    rename(document = directory_name, text = description) %>% 
  unnest_tokens(word, text) %>% 
  group_by(document) %>% 
  summarize(count_words = n())

yco_init_stats <- yco_init %>% 
  summarize("document count" = n_distinct(document),
            minimum = min(count_words),
            "lower quartile" = quantile(count_words, .25),
            mean = mean(count_words),
            median = median(count_words),
            "upper quartile" = quantile(count_words, .75),
            maximum = max(count_words),
            "standard deviation" = sd(count_words)) %>% 
  mutate(data = "companies, pre-processing") %>% 
  relocate(data)

yco_init_hist <- yco_init %>% 
  ggplot(aes(x = count_words)) +
  geom_histogram()

# ycombinator post-processing

remove <- "[:digit:]|[:blank:]|[:punct:]"

yco_post <- ycombinator_data %>% select(directory_name, description) %>%
  mutate(description = str_trim(description, side = "both")) %>% 
  rename(document = directory_name, text = description) %>% 
  unnest_tokens(word, text) %>% 
  mutate(word = replace_non_ascii(word)) %>% 
  mutate(word = str_replace_all(word, remove, "")) %>%
  filter(word != "") %>% 
  anti_join(stop_words) %>% 
  mutate(word = lemmatize_words(word)) %>% 
  group_by(document) %>% 
  summarize(count_words = n()) 

yco_post_stats <- yco_post %>% 
  summarize("document count" = n_distinct(document),
            minimum = min(count_words),
            "lower quartile" = quantile(count_words, .25),
            mean = mean(count_words),
            median = median(count_words),
            "upper quartile" = quantile(count_words, .75),
            maximum = max(count_words),
            "standard deviation" = sd(count_words)) %>% 
  mutate(data = "companies, post-processing") %>% 
  relocate(data)

yco_post_hist <- yco_post %>% 
  ggplot(aes(x = count_words)) +
  geom_histogram()

# textbook initial data

textbooks_init <- textbooks %>% select(directory_name, description) %>%
  mutate(description = str_trim(description, side = "both")) %>% 
  rename(document = directory_name, text = description) %>% 
  unnest_tokens(word, text) %>% 
  group_by(document) %>% 
  summarize(count_words = n())

textbooks_init_stats <- textbooks_init %>% 
  summarize("document count" = n_distinct(document),
            minimum = min(count_words),
            "lower quartile" = quantile(count_words, .25),
            mean = mean(count_words),
            median = median(count_words),
            "upper quartile" = quantile(count_words, .75),
            maximum = max(count_words),
            "standard deviation" = sd(count_words)) %>% 
  mutate(data = "textbooks, pre-processing") %>% 
  relocate(data)

# textbooks post-processing

remove <- "[:digit:]|[:blank:]|[:punct:]"

textbooks_post <- textbooks %>% select(directory_name, description) %>%
  mutate(description = str_trim(description, side = "both")) %>% 
  rename(document = directory_name, text = description) %>% 
  unnest_tokens(word, text) %>% 
  mutate(word = replace_non_ascii(word)) %>% 
  mutate(word = str_replace_all(word, remove, "")) %>%
  filter(word != "") %>% 
  anti_join(stop_words) %>% 
  mutate(word = lemmatize_words(word)) %>% 
  group_by(document) %>% 
  summarize(count_words = n()) 

textbooks_post_stats <- textbooks_post %>% 
  summarize("document count" = n_distinct(document),
            minimum = min(count_words),
            "lower quartile" = quantile(count_words, .25),
            mean = mean(count_words),
            median = median(count_words),
            "upper quartile" = quantile(count_words, .75),
            maximum = max(count_words),
            "standard deviation" = sd(count_words)) %>% 
  mutate(data = "textbooks, post-processing") %>% 
  relocate(data)

# combined summary table

descr_stats <- yco_init_stats %>% 
  add_row(yco_post_stats) %>% 
  add_row(textbooks_init_stats) %>% 
  add_row(textbooks_post_stats) 

descr_stats_table <- kbl(descr_stats) %>% 
  kable_classic() %>% 
  add_header_above(c(" " = 1, " " = 1, "words per document" = 7))

# combined histogram

yco_init <- yco_init %>% 
  mutate(data = "textbooks, pre-processing")

yco_post <- yco_post %>% 
  mutate(data = "textbooks, post-processing")

yco_init_post <- yco_init %>% add_row(yco_post)
yco_init_post$data <- factor(yco_init_post$data, 
                             levels = c("textbooks, pre-processing",
                                        "textbooks, post-processing"))

yco_hist <- yco_init_post %>% 
  ggplot(aes(x = count_words)) +
  geom_histogram() +
  facet_grid(~data)

yco_hist
################################################################################
### combine company descriptions and textbook extracts, initial processing to 
### trim whitespace and NA values

data <- ycombinator_data %>% select(directory_name, description) %>%
  mutate(description = str_trim(description, side = "both")) %>% 
  filter(!is.na(description)) %>%
  filter(description != "") %>% 
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

data_clean_companies <- data_clean %>% 
  count(document)

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
### calculate cosine similarity per textbook and company description

# extract vectors for each textbook
x_ent <- dtm_matrix["entrepeneurship",]
x_fin <- dtm_matrix["finance",]
x_ldr <- dtm_matrix["leadership",]
x_mkt <- dtm_matrix["marketing",]
x_str <- dtm_matrix["strategy",]

# convert to jaccard logical vectors for each textbook
x_ent_j <- x_ent > 0
x_fin_j <- x_fin > 0
x_ldr_j <- x_ldr > 0 
x_mkt_j <- x_mkt > 0
x_str_j <- x_str > 0

# create empty cosine tibble
cosine_table <- tibble(directory_name = character(), ent_c = numeric(), 
                       fin_c = numeric(), ldr_c = numeric(), mkt_c = numeric(),
                       str_c = numeric(), ent_j = numeric(), fin_j = numeric(),
                       ldr_j = numeric(), mkt_j = numeric(), str_j = numeric())

# define jaccard similiary function
jaccard <- function(x, y) {
  intersection = x %*% y
  union = length(x) + length(y) - intersection
  return(intersection/union)
}

# calculate cosine and jaccard similarity for each company and textbook
for(i in 1:length(data_clean_companies$document)){
  cat("Iteration", i, "out of", length(data_clean_companies$document), "\n")
  doc <-  data_clean_companies$document[i]
  y <- dtm_matrix[doc,]
  y_j <- y > 0

  #calculate cosine similarity
  ent_c <- cosine(x_ent, y)
  fin_c <- cosine(x_fin, y)
  ldr_c <- cosine(x_ldr, y)
  mkt_c <- cosine(x_mkt, y)
  str_c <- cosine(x_str, y)

  #calculate jaccard similiary
  ent_j <- jaccard(x_ent_j, y_j)
  fin_j <- jaccard(x_fin_j, y_j)
  ldr_j <- jaccard(x_ldr_j, y_j)
  mkt_j <- jaccard(x_mkt_j, y_j)
  str_j <- jaccard(x_str_j, y_j)

  cosine_table <- cosine_table %>%  
    add_row(directory_name = doc, ent_c = ent_c, fin_c = fin_c,
            ldr_c = ldr_c, mkt_c = mkt_c, str_c = str_c, ent_j = ent_j,
            fin_j = fin_j, ldr_j = ldr_j, mkt_j = mkt_j, str_j = str_j)
}

# remove textbooks from cosine_table and rename columns
remove <- c("entrepeneurship", "finance", "leadership", "marketing", "strategy")

cosine_table <- cosine_table %>% 
  filter(!directory_name %in% remove) %>% 
  mutate(ent_c = ent_c[,1], fin_c = fin_c[,1], ldr_c = ldr_c[,1],
         mkt_c = mkt_c[,1], str_c = str_c[,1])

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
  summarise(n = n_distinct(directory_name)) %>% 
  rename(funded = n)

################################################################################
### create dummy variables for exit status from ycombinator data

exit <- ycombinator_data %>% 
  select(directory_name, status) %>% 
  mutate(acquired = ifelse(status == "Acquired", 1, 0)) %>% 
  mutate(active = ifelse(status == "Active", 1, 0)) %>% 
  mutate(inactive = ifelse(status == "Inactive", 1, 0)) %>% 
  mutate(public = ifelse(status == "Public", 1, 0))

################################################################################
### join similarity, funding, and exit tables

# join cosine table with funding data
join1 <- left_join(cosine_table, funding_data, by = "directory_name")

# join status table (exit info)
data_joined <- left_join(join1, exit, by = "directory_name")

# replace NA values with 0
data_joined$num_funding_rounds <- data_joined$num_funding_rounds %>% 
  replace_na(0)
data_joined$funded <- data_joined$funded %>% 
  replace_na(0)

################################################################################
### combined table summary statistics

combined_cosine <- data_joined %>% 
  group_by(status) %>% 
  summarize(count = n(),
            entrepreneurship = mean(ent_c), finance = mean(fin_c), 
            leadership = mean(ldr_c), marketing = mean(mkt_c),
            strategy = mean(str_c)) %>% 
  kbl() %>% 
  kable_classic() %>% 
  add_header_above(c(" " = 1, " " = 1, "Mean cosine similarity scores" = 5))

combined_jaccard <- data_joined %>% 
  group_by(status) %>% 
  summarize(count = n(),
            entrepreneurship = mean(ent_j), finance = mean(fin_j), 
            leadership = mean(ldr_j), marketing = mean(mkt_j),
            strategy = mean(str_j)) %>% 
  kbl() %>% 
  kable_classic() %>% 
  add_header_above(c(" " = 1, " " = 1, "Mean Jaccard similarity scores" = 5))


################################################################################
### funding data statistics

funding_summary <- data_joined %>% 
  summarize(total = n(),
            active = sum(active),
            funded = sum(funded),
            inactive = sum(inactive),
            acquired = sum(acquired),
            public = sum(public)) %>% 
  pivot_longer(everything(), names_to = "status", values_to = "count")

funding_summary_plot <- ggplot(funding_summary, 
                               aes(x = reorder(status, -count), y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "funding / status category",
       title = "Table X - Company funding and status statistics")


################################################################################
### regression analysis

# ordinal logistic regression of company status

data_polr <- data_joined
data_polr$status <- factor(data_polr$status, 
                           levels = c("Inactive", "Active", "Acquired", "Public"), ordered = TRUE)

log_status <- polr(status ~ ent_c + fin_c + ldr_c + mkt_c + str_c + ent_j + fin_j +
                ldr_j + mkt_j + str_j, data = data_polr, Hess = TRUE)
summary(log_status)

# multivariable logistic for funded, exits?

# active multivariable logistic regression
log_active <- glm(formula = active ~ ent_c + fin_c + ldr_c + mkt_c + str_c +
                    ent_j + fin_j + ldr_j + mkt_j + str_j,
                  data = data_joined, family = "binomial"(link = "logit"))

# funded multivariable logistic regression
log_funded <- glm(formula = funded ~ ent_c + fin_c + ldr_c + mkt_c + str_c +
                    ent_j + fin_j + ldr_j + mkt_j + str_j,
                  data = data_joined, family = "binomial"(link = "logit"))

# inactive multivariable logistic regression
log_inactive <- glm(formula = inactive ~ ent_c + fin_c + ldr_c + mkt_c + str_c +
                      ent_j + fin_j + ldr_j + mkt_j + str_j,
                    data = data_joined, family = "binomial"(link = "logit"))

# acquired multivariable logistic regression
log_acquired <- glm(formula = acquired ~ ent_c + fin_c + ldr_c + mkt_c + str_c +
                    ent_j + fin_j + ldr_j + mkt_j + str_j,
                  data = data_joined, family = "binomial"(link = "logit"))

# public multivariable logistic regression
log_public <- glm(formula = public ~ ent_c + fin_c + ldr_c + mkt_c + str_c +
                      ent_j + fin_j + ldr_j + mkt_j + str_j,
                    data = data_joined, family = "binomial"(link = "logit"))

# should we use odds ratio or summary output in report?

summary(log_active)




