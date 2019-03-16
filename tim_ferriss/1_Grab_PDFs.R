# libraries
library(stringr)

# data source
url <- "https://tim.blog/2018/09/20/all-transcripts-from-the-tim-ferriss-show/"

# setup
timFerrisHtml <- paste(readLines(url), collapse="\n")
matched <- str_match_all(timFerrisHtml, "<a href=\"https://fhww.files.wordpress.com/(.*?)\"")
links <- matched[[1]][, 2]
pdfNames <- sapply(links, function(x) gsub("/", "-", x))

# stopwords for future
linkStopWords <- pdfNames[-grep("tim-ferriss", pdfNames)]
linkStopWords <- gsub("[0-9]+", "", linkStopWords)
linkStopWords <- gsub("---", "", linkStopWords)
linkStopWords <- gsub(".pdf", "", linkStopWords)
linkStopWords <- strsplit(linkStopWords, split = "-")
linkStopWords <- sort(unique(unlist(linkStopWords)))

# this first process only retrieves the first 150
# as of 3/13/2018, there are 363 shows

# loop thru available 
n <- length(links)
for (i in 1:n) {
  transcriptURL <- paste("https://fhww.files.wordpress.com/", links[i], sep = "")
  download.file(transcriptURL, 
                paste0(root,
                       "pdf/",
                       pdfNames[i]))
}