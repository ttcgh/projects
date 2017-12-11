# Visualizing Carbon Intensity
# Bubble Plots

setwd('/Users/Home/Desktop')
source('bubbles/helpers.R')

##########################################################################################
### Shiny
##########################################################################################

# User Interface

ui <- fluidPage(
  
  titlePanel('Carbon Intensity of US'),
  
  sidebarLayout(
    sidebarPanel(
      helpText('Visualized by State and Year'),

    sliderInput('num', 
                label = 'Dynamic: Play to View State Level CO2 Intensity vs. US',
                min = minframe, max = maxframe, value = minframe, step = 1,
                ticks = F,
                sep = '',
                animate = animationOptions(interval = 110,
                                           loop = T))
    
    ),
    
    mainPanel(plotOutput('scatter'))
  )
)

# Server Logic

server <- function(input, output) {
    
  output$scatter <- renderPlot({
    
    yearLabel <- tlist[(tlist$.frame == input$num), 7]
    
    ggplot(data = tlist[(tlist$.frame == input$num), ],
           aes(x = x, y = y, frame = .frame)) +
      geom_point(aes(size = Carbon, color = ST), alpha = 0.8) +
      geom_smooth(method = 'lm', se = F) +
      scale_size_continuous(range = c(1, 10)) +
      ggtitle(paste(yearLabel)) +
        theme(plot.title = element_text(color = '#666666', size = 32, face = 'bold', hjust = 0.5)) +
      theme(legend.position = 'none') +
      labs(x = 'State GDP in 2009 Constant $ (T)', y = 'Million Metric Tons of CO2') +
      xlim(0, xmax) +
      ylim(0, ymax)

  })
  
}

# Run App

shinyApp(ui = ui, server = server)
