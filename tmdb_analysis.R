rm(list = ls())
library(tidyverse)
library(textstem)
library(tm)
theme_set(theme_bw(base_size = 16))
theme_update(panel.grid.minor = element_blank())

data <- read.csv("tmdb_5000_movies.csv", sep = ",") %>% 
  as_tibble()

## Data preprocessing
# One-hot encoding for genres
df_genres <- data %>% 
  select(id, genres) %>% 
  mutate(genres = genres %>% 
           as.character() %>% 
           str_remove_all("\"|\\[|\\{|\\]|\\}") %>% 
           strsplit(",")) %>% 
  unnest() %>% 
  filter(str_detect(genres, "name")) %>% 
  mutate(genres = genres %>% 
           str_remove_all("name:") %>% 
           str_trim() %>% 
           paste0("genre.", .)) %>% 
  mutate(value = 1) %>% 
  spread(key = genres, value = value, fill = 0)
# One-hot encoding for country
df_country <- data %>% 
  select(id, production_countries) %>% 
  transmute(id = id, 
            country = production_countries %>% 
            as.character() %>% 
            str_remove_all("\"|\\[|\\{|\\]|\\}") %>% 
            strsplit(",")) %>% 
  unnest() %>% 
  filter(str_detect(country, "name")) %>% 
  mutate(country = country %>% str_remove_all("name:") %>% str_trim() %>% paste0("country.", .)) %>% 
  mutate(value = 1) %>% 
  spread(key = country, value = value, fill = 0)
# Preprocessing and encoding of text-based information
df_text <- data %>% 
  select(id, title, tagline, overview) %>% 
  transmute(id = id, 
            text = paste(title, tagline, overview) %>% 
              strsplit(" ")) %>% 
  unnest() %>% 
  mutate(text = text %>% 
           tolower() %>% 
           str_replace_all("[[:punct:] ]+", " ") %>% 
           str_replace_all("[[:digit:] ]+", " ") %>% 
           str_remove_all(" ")) %>% 
  filter(!(text %in% stopwords())) %>% 
  filter(text != "") %>% 
  mutate(text = text %>% 
           lemmatize_strings())
document_frequency <- df_text %>% 
  group_by(text) %>% 
  summarise(df = n()) %>% 
  mutate(idf = 1 / df) %>% 
  ungroup()
term_frequency <- df_text %>% 
  left_join(document_frequency, by = "text") %>%
  filter(df > 12) %>% 
  select(id, text) %>% 
  mutate(value = 1) %>% 
  spread(key = text, value = value, fill = 0)

## Predictive models
# Revenue predicting model and a wordcloud with terms that improve revenue
model.revenue <- lm(revenue ~ ., data = term_frequency %>% left_join(data %>% select(id, revenue), by = "id"))
summary_data <- summary(model.revenue)$coefficients %>% 
  as_tibble(rownames = "term") %>% 
  arrange(`Pr(>|t|)`) %>% 
  head(200) %>% 
  mutate(term = term %>% str_remove("lemma.")) %>% 
  filter(term != "(Intercept)") %>% 
  filter(term != "id")
wordcloud(words = summary_data$term, freq = summary_data$Estimate, colors = brewer.pal(8, "Dark2"))

# genre.Action predicting model and a wordcloud that corresponds to this genre
model.Action <- glm(genre.Action ~ ., data = term_frequency %>% left_join(df_genres %>% select(id, genre.Action), by = "id"), family = "binomial")
summary_data <- summary(model.Action)$coefficients %>% 
  as_tibble(rownames = "term") %>% 
  arrange(`Pr(>|z|)`) %>% 
  head(200) %>% 
  mutate(term = term %>% str_remove("lemma.")) %>% 
  filter(term != "(Intercept)") %>% 
  filter(term != "id")
wordcloud(words = summary_data$term, freq = summary_data$Estimate, max.words = 50, colors = brewer.pal(8, "Dark2"))

# country.Canada predicting model and a wordcloud that corresponds to this country
model.Canada <- glm(country.Canada ~ ., data = term_frequency %>% left_join(df_country %>% select(id, country.Canada), by = "id"), family = "binomial")
summary_data <- summary(model.Canada)$coefficients %>% 
  as_tibble(rownames = "term") %>% 
  arrange(`Pr(>|z|)`) %>% 
  head(200) %>% 
  mutate(term = term %>% str_remove("lemma.")) %>% 
  filter(term != "(Intercept)") %>% 
  filter(term != "id")
wordcloud(words = summary_data$term, freq = summary_data$Estimate, max.words = 50, colors = brewer.pal(8, "Dark2"))
