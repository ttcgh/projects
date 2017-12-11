# Visualizing Carbon Intensity
# Map and Line Charts

setwd('/Users/Home/Desktop')
source('mapsLines/helpers.R')

##########################################################################################
### Shiny
##########################################################################################

# User Interface

ui <- fluidPage(
  
  titlePanel('Carbon Intensity of US'),
  
  sidebarLayout(
    sidebarPanel(
      helpText('Visualized by State and Year'),

    sliderInput('year', 
                label = 'Dynamic: Play to View Changing CO2 Intensity by Year',
                min = 1990, max = 2015, value = 1990,
                ticks = F,
                sep = '',
                animate = animationOptions(interval = 535, 
                                           loop = T)),
    
    sliderInput('num', 
                label = 'Dynamic: Play to View State Level CO2 Intensity vs. US',
                min = minframe, max = maxframe, value = minframe, step = 1,
                ticks = F,
                sep = '',
                animate = animationOptions(interval = 335,
                                           loop = T)),
    
    selectInput('state', 
                label = 'Static: Choose State to Compare Against US',
                choices = as.list(states$ST))
    
    
    ),
    
    mainPanel(plotOutput('map'), 
              plotOutput('movingline'), 
              plotOutput('line'))
  )
)

# Server Logic

server <- function(input, output) {
    
  output$map <- renderPlot({
    
    datamap <- merge(us, 
                     carbonintensity[(
                       carbonintensity$Year == input$year & carbonintensity$ST != 'DC'),
                       c('Year','State','Carbon.Intensity')],
                     by.x = "region",
                     by.y = "State")
    
    functionplot <- mapfunction(usmap = us, mapdata = datamap, year = input$year)
    
    functionplot
    
  }, height = 350, width = 700)
  
  output$movingline <- renderPlot({
    
    stlabel <- tlist$ST[tlist$.frame == input$num & tlist$Year == 2015]
    
    ggplot(data = tlist[(tlist$.frame == input$num), ],
           aes(x = Year,
               y = Carbon.Intensity,
               group = 1)) +
      geom_line(color = 'red', size = 1.2) +
      geom_text(data = tlist[tlist$.frame == input$num & tlist$Year == 2015,],
                aes(label = stlabel),
                vjust = 2) +
      geom_line(data = USdata,
                aes(x = year,
                    y = Carbon.Intensity),
                color = 'black') +
      ylim(0, ymax)
    
  })
  
  output$line <- renderPlot({
  
    ggplot(data = statedata[(statedata$ST == input$state), ],
           aes(x = Year,
               y = Carbon.Intensity,
               group = 1)) +
    geom_line(color = 'red', size = 1.2) +
    geom_text(data = statedata[statedata$Year == 2015 & statedata$ST == input$state,],
              aes(label = input$state),
              vjust = 2) +
    geom_line(data = USdata,
              aes(x = year,
                  y = Carbon.Intensity),
              color = 'black') +
    ylim(0, ymax)
    
  })
  
}

# Run App

shinyApp(ui = ui, server = server)
