# names
root <- '/folder/'
filenames <- list.files(root)
filenames <- paste0(root, filenames)
filenames <- filenames[-grep("consolidated.R", filenames)]
filenames <- filenames[-grep("pdf", filenames)]

# must create folder "pdf" in your root folder above before executing next line
sapply(filenames, source)