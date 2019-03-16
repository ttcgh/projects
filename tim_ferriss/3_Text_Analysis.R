# libraries
library(tidytext)
library(dplyr)
library(stopwords)
library(ggplot2)

# term frequency
podcastWords <- corpus %>%
  unnest_tokens(output = word, input = text) %>%
  count(podcast, word, sort = TRUE) %>%
  ungroup()

# remove stop words
podcastWords <- podcastWords %>%
  anti_join(stop_words, by = c("word" = "word"))

# remove link stop words
podcastWords <- 
  podcastWords[!(podcastWords$word %in% linkStopWords), ]

# remove apostrophe's
podcastWords$word <- gsub("’", "'", podcastWords$word)

# remove stop words from stopwords library
podcastWords <- 
  podcastWords[!(podcastWords$word %in% stopwords(language = "en")), ]

# manual stop words
timFerrissStopWords <- c("copyright",
                         "reserved",
                         "tim",
                         "ferriss",
                         "podcast",
                         "2018",
                         "2007",
                         "voice",
                         "male",
                         "inaudible",
                         # start here are words after iterating thru topics
                         "people",
                         "yeah",
                         "lot",
                         "time",
                         "rights",
                         "book",
                         "life",
                         "sort",
                         "ferris",
                         "world",
                         "love",
                         "stuff",
                         "d'agostino",
                         "guy",
                         "guys",
                         "person",
                         "mager",
                         "start",
                         "started")

# remove manual stop words
podcastWords <- 
  podcastWords[!(podcastWords$word %in% timFerrissStopWords), ]

# total words per podcast, minus the stop words
total_words <- podcastWords %>% 
  group_by(podcast) %>% 
  summarize(total = sum(n))

# add total words to the table
podcastWords <- left_join(podcastWords, total_words)

# term frequence, inverse document frequency
podcastWords <- podcastWords %>%
  bind_tf_idf(word, podcast, n)

# order
podcastWords <- podcastWords %>%
  arrange(desc(tf_idf))

# plot
podcastPlot <- podcastWords %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(podcast) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = podcast)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~podcast, ncol = 2, scales = "free_x") +
  coord_flip()