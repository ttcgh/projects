# Libraries
library(shiny)
library(maps)
library(ggplot2)
library(animation)
library(tweenr)

# Data
us <- map_data("state") # creates lat-long for each state
setwd('/Users/Home/Desktop')
carbonintensity <- read.csv('mapsLines/data/carbonintensity.csv')
carbonintensity <- carbonintensity[, -1]
statedata <- carbonintensity[, -c(3, 4)]
statedata$Year <- as.factor(statedata$Year) # avoid "tweening" for years
states <- unique(statedata$ST)

# tween data
statefunction <- function(mystate) {
  as.data.frame(statedata[statedata$ST == mystate, ])
}

mylist <- lapply(states, statefunction)

tlist <- tween_states(mylist,
                      tweenlength = 2,
                      statelength = 1,
                      ease = rep('cubic-in-out', 51),
                      nframes = 300)

minframe <- min(tlist$.frame)
maxframe <- max(tlist$.frame)

# expanding state abbreviations
states <- read.csv('mapsLines/data/states.csv')
states <- states[-c(3:7)]
colnames(states)[2] <- "ST"
colnames(carbonintensity)[2] <- "ST"

carbonintensity <- merge(x = carbonintensity, 
                         y = states, 
                         by = "ST", 
                         all.x = TRUE)

carbonintensity$State <- sapply(carbonintensity$State,tolower)
ymax <- max(carbonintensity$Carbon.Intensity)

# create US data
carbonintensity$GWh <- carbonintensity$MWh / 1e6
carbonintensity$MMtCO2e <- carbonintensity$Carbon / 1e6
USCO2 <- aggregate(MMtCO2e ~ Year, carbonintensity, sum)
USGWh <- aggregate(GWh ~ Year, carbonintensity, sum)
USdata <- cbind(USCO2, USGWh$GWh, USCO2$MMtCO2e / USGWh$GWh)
colnames(USdata) <- c('year', 'MMtCO2e', 'GWh', 'Carbon.Intensity')
USdata$year <- as.factor(USdata$year)

# Function to create map
mapfunction <- function(usmap, mapdata, year) {
  
  mapplot <- ggplot()
  
  mapplot <- mapplot + geom_map(data = usmap, 
                                map = usmap,
                                aes(x = long, y = lat, map_id = region),
                                fill = "#ffffff", color = "#ffffff", size = 0.15)
  
  mapplot <- mapplot + geom_map(data = mapdata, map = usmap,
                                aes(fill = `Carbon.Intensity`, map_id = region),
                                color = "#ffffff", size = 0.15)
  
  mapplot <- mapplot + scale_fill_gradient2(low = 'lightblue', 
                                            mid = 'grey',
                                            high = 'black',
                                            guide = 'colorbar',
                                            midpoint = 0.5,
                                            breaks = c(0, 0.5, 1),
                                            labels = c(0, 0.5, 1),
                                            limits = c(0,1))
  
  mapplot <- mapplot + labs(x = NULL, y = NULL) 
  mapplot <- mapplot + theme(panel.background = element_blank()) 
  mapplot <- mapplot + theme(axis.ticks = element_blank()) 
  mapplot <- mapplot + theme(axis.text = element_blank()) 
  mapplot <- mapplot + ggtitle(paste(year)) +
    theme(plot.title = element_text(color = '#666666', size = 32, face = 'bold', hjust = 0.5))
  
}

