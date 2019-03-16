# libraries
library(dplyr)
library(ggplot2)
library(topicmodels)
library(tidytext)

# turn tidy data, i.e., tf-idf dataframe, into document-term matrix
podcastTermMatrix <- podcastWords %>%
  cast_dtm(podcast, word, n)

# Latent Dirichlet allocation (LDA)
podcastLDA <- LDA(podcastTermMatrix, 
                  k = 6, 
                  control = list(seed = 31415))

# word-topic probabilities
podcastTopics <- tidy(podcastLDA, matrix = "beta")

# topics
podcastTopTerms <- podcastTopics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# plot top terms
termsPerTopic <- podcastTopTerms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
print(termsPerTopic)

# topic mixtures
podcastDocs <- tidy(podcastLDA, matrix = "gamma")
podcastDocs