# libraries
library(dplyr)
library(ggplot2)

# name each topic
topicNames <- c("1. Creativity",
                "2. Psychology",
                "3. Fitness",
                "4. Health",
                "5. Mindfullness",
                "6. Business")
topicNames <- as.data.frame(cbind(1:6, topicNames))
colnames(topicNames) <- c("topic", "topic_name")

# join podcastDocs with topicNames
podcastDocs <- merge(podcastDocs,
                     topicNames,
                     by = "topic",
                     all.x = TRUE)

# top 2
podcastDocsTop2 <- podcastDocs %>%
  group_by(document) %>%
  top_n(2, gamma) %>%
  ungroup() %>%
  arrange(document, -gamma)

podcastDocsTop2 <- podcastDocsTop2[, -1]
colnames(podcastDocsTop2) <- c("title", "percent", "topic")
podcastDocsTop2$percent <- round(podcastDocsTop2$percent * 100)
summary(podcastDocsTop2$topic)

# top
podcastDocsTop1 <- podcastDocs %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  arrange(document, -gamma)

podcastDocsTop1 <- podcastDocsTop1[, -1]
colnames(podcastDocsTop1) <- c("title", "percent", "topic")
podcastDocsTop1$percent <- round(podcastDocsTop1$percent * 100)
summary(podcastDocsTop1$topic)

# replot top terms with names
podcastTopTerms <- merge(podcastTopTerms,
                         topicNames,
                         by = "topic",
                         all.x = TRUE)
termsPerTopic <- podcastTopTerms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic_name))) +
  geom_col(show.legend = FALSE) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank()) +
  facet_wrap(~ topic_name, scales = "free") +
  coord_flip()
print(termsPerTopic)
