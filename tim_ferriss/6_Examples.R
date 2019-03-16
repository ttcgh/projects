# test a few that i've listened to
fitnessAndHealth <- c("kellystarrett",
                      "rhondapatrick",
                      "arnoldschwarzenegger") 

fitnessAndHealthTopics <- 
  podcastDocsTop2[grep(paste(fitnessAndHealth,collapse = "|"), 
                       podcastDocsTop2$title),]
fitnessAndHealthTopics %>% 
  tbl_df %>% 
  print(n = Inf)

entertainment <- c("arnoldschwarzenegger",
                   "jamiefoxx",
                   "costner",
                   "bjnovak",
                   "sethrogen",
                   "jonfavreau")  

entertainmentTopics <- 
  podcastDocsTop2[grep(paste(entertainment,collapse = "|"), 
                       podcastDocsTop2$title),]
entertainmentTopics %>% 
  tbl_df %>% 
  print(n = Inf)

randomPodcasts <- c("jocko",
                    "samharris",
                    "stephendubner",
                    "random")  

randomPodcastsTopics <- 
  podcastDocsTop2[grep(paste(randomPodcasts,collapse = "|"), 
                       podcastDocsTop2$title),]
randomPodcastsTopics %>% 
  tbl_df %>% 
  print(n = Inf)
