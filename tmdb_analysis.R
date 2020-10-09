rm(list = ls())
library(tidyverse)
library(textstem)
library(tm)
theme_set(theme_bw(base_size = 16))
theme_update(panel.grid.minor = element_blank())

# Чтение данных
data <- read.csv("tmdb_5000_movies.csv", sep = ",") %>% 
  as_tibble()

## Предварительная обработка данных
# Кодирование колонки с жанрами
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
# Кодирование колонки со страной производства
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
# Кодирование текстовой информации
df_text <- data %>% 
  select(id, title, tagline) %>% 
  transmute(id = id, 
            text = paste(title, tagline) %>% 
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
lemmas.freq <- df_text %>% 
  select(text) %>% 
  group_by(text) %>% 
  summarise(count = n())
df_text <- df_text %>% 
  distinct() %>% 
  left_join(lemmas.freq, by = "text") %>% 
  filter(count > 2) %>% 
  select(-count) %>% 
  mutate(text = paste0("lemma.", text)) %>% 
  mutate(value = 1) %>% 
  spread(key = text, value = value, fill = 0)

# Объединение данных
data_revenue <- data %>% 
  select(id, budget, revenue) %>% 
  left_join(df_genres, by = "id") %>% 
  left_join(df_country, by = "id") %>% 
  left_join(df_text, by = "id")

# Графики для быстрой оценки данных
ggplot(data = data_revenue) + 
  geom_point(aes(x = budget, y = revenue)) + 
  scale_x_log10(name = "Бюджет фильма") + 
  scale_y_log10(name = "Кассовые сборы") + 
  theme_bw(base_size = 16)

# Оценка способностей предсказательных моделей
summary(lm(revenue ~ budget, data = data_revenue))
model <- lm(revenue ~ ., data = data_revenue)
summary_data <- summary(model)$coefficients %>% 
  as_tibble(rownames = "term") %>% 
  filter(`Pr(>|t|)` < 0.001)