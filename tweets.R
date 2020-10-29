
# Load Libraries
library(tidyverse)
library(stringr)
library(tidytext)

# Load datasets
train <- read_csv("./PBC/train.csv")
test <- read_csv("./PBC/test.csv")
ss <- read_csv("./PBC/sample_submission.csv")

# Data Prep 
train.v1 <- train %>%
  mutate(text = str_squish(gsub("[^[:alnum:][:space:]]", "", tolower(text)))) %>%
  mutate(selected_text = str_squish(gsub("[^[:alnum:][:space:]]", "", tolower(selected_text)))) %>%
  group_by(textID) %>%
  unnest_tokens(word, text) %>%
  select(textID, word)


train.v2 <- train.v1 %>%
  anti_join(get_stopwords())

sent <- get_sentiments("bing")

train.v3 <- left_join(train.v2, sent, by = "word")

train.v4 <- train.v3 %>%
  mutate(sentiment = replace_na(sentiment, "neutral")) %>%
  mutate(sent_num = case_when(
    sentiment == "positive" ~ 1,
    sentiment == "negative" ~ -1,
    sentiment == "neutral" ~ 0
  ))


train.v5 <- train.v4 %>%
  group_by(textID) %>%
  summarize(sent_num = sum(sent_num)) %>%
  mutate(sentiment_new = case_when (
    sent_num > 0 ~ "positive",
    sent_num == 0 ~ "neutral",
    sent_num < 0 ~ "negative"
  ))

train.compare <- left_join(train.v5, train, by = "textID")
