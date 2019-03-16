# libraries
library(pdftools)
library(magrittr)

# files
timFerrissDirectory <- paste0(root,"pdf/")
timFerrisPDFNames <- list.files(timFerrissDirectory)

# creating corpus
corpus <- data.frame("podcast" = c(), "text" = c())

# turn all pdf's into text and storing into corpus
for (i in 1:n) {
    pdf_text(paste0(timFerrissDirectory,
                    pdfNames[i])) %>% 
    strsplit(split = "\n") %>% 
    unlist() -> temporaryText
  podcast <- data.frame(
    "company" = gsub("-", "", pdfNames[i]),
    "text" = temporaryText,
    stringsAsFactors = FALSE)
  colnames(podcast) <- c("podcast", "text")
  corpus <- rbind(corpus, podcast, row.names = NULL)
}