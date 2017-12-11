# Libraries
library(shiny)
library(maps)
library(ggplot2)
library(animation)
library(tweenr)
library(dplyr)
library(reshape2)

# Data
setwd('/Users/Home/Desktop/')
carbonintensity <- read.csv('bubbles/data/carbonintensity.csv')
carbonintensity <- carbonintensity[, -1]
carbonintensity[, c(3,4)] <- carbonintensity[, c(3,4)] / 10^6
names(carbonintensity)[names(carbonintensity) == 'MWh'] <- 'GWh'

# economic Data
gdpPre1997 <- read.csv('bubbles/data/gdpsic.csv')
gdpPost1997 <- read.csv('bubbles/data/gdpnaics.csv')
gdpPre1997 <- gdpPre1997[(gdpPre1997$IndustryId == 1), ]
gdpPost1997 <- gdpPost1997[(gdpPost1997$IndustryId == 1), ]
regional <- gdpPost1997[-1, 2:3]
regional$GeoName <- sapply(regional$GeoName, tolower)
regional <- regional[complete.cases(regional), ]

# fix states
states <- read.csv('bubbles/data/states.csv')
states <- states[-c(3:7)]
colnames(states)[2] <- "ST"

carbonintensity <- merge(x = carbonintensity, 
                         y = states, 
                         by = "ST", 
                         all.x = TRUE)

carbonintensity$State <- sapply(carbonintensity$State, tolower)

# combine carbon and regions
carbonintensity <- merge(x = carbonintensity,
                         y = regional,
                         by.x = 'State',
                         by.y = 'GeoName',
                         all.x = T)

regions <- cbind(c('New England', 'Mideast', 'Great Lakes', 'Plains', 
                   'Southeast', 'Southwest', 'Rockies', 'Far West'),
                 c(1:8))

carbonintensity <- merge(x = carbonintensity,
                         y = regions,
                         by.x = 'Region',
                         by.y = 'V2',
                         all.x = T)

names(carbonintensity)[names(carbonintensity) == 'Region'] <- 'RegionNumber'
names(carbonintensity)[names(carbonintensity) == 'V1'] <- 'Region'

# economic data
gdpPre1997 <- gdpPre1997[-1, c(2, 3, 36:43)]
newcolnames <- c('State', 'Region', 1990:1997)
colnames(gdpPre1997) <- newcolnames
gdpPre1997 <- gdpPre1997[complete.cases(gdpPre1997), ]
gdp1 <- melt(gdpPre1997, id.vars = c('State', 'Region'))
gdp1$value <- as.numeric(gdp1$value)

gdpPost1997 <- gdpPost1997[-1, c(2, 3, 10:27)]
newcolnames <- c('State', 'Region', 1998:2015)
colnames(gdpPost1997) <- newcolnames
gdpPost1997 <- gdpPost1997[complete.cases(gdpPost1997), ]
gdp2 <- melt(gdpPost1997, id.vars = c('State', 'Region'))
gdp2$value <- as.numeric(gdp2$value)

gdp <- rbind(gdp1, gdp2)
gdp$State <- sapply(gdp$State, tolower)
gdp$value <- gdp$value / 10^6

# combine economic and carbon data
carbonintensity <- merge(x = carbonintensity,
                         y = gdp,
                         by.x = c('State', 'Year'),
                         by.y = c('State', 'variable'),
                         all.x = T)

# post 2000 data
carbonintensity <- carbonintensity[(carbonintensity$Year >= 2000), ]
# to remove 'outliers' manually
carbonintensity <- carbonintensity[!(carbonintensity$ST %in% c('CA', 'NY', 'TX')), ]

# tween data
ciNew <- carbonintensity

ciNewEdit <- ciNew %>%
  arrange(ST, Year) %>%
  select(Year, ST, GWh, Carbon, value) %>%
  rename(time = Year, id = ST, x = value, y = Carbon) %>%
  mutate(ease = 'linear')

tlist <- tween_elements(ciNewEdit,
                        'time', 'id', 'ease',
                        nframes = 50) %>%
  mutate(Year = round(time), ST = .group) %>%
  left_join(ciNew, by = c('Year', 'ST')) %>%
  rename(GWh = GWh.x)

minframe <- min(tlist$.frame)
maxframe <- max(tlist$.frame)
xmax <- max(ciNew$value)
ymax <- max(ciNew$Carbon)
bubblemin <- min(ciNew$GWh)
bubblemax <- max(ciNew$GWh)
