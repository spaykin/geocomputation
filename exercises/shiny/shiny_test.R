#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(spData)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("My first Shiny app"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            
            sliderInput("year",
                        "Year:",
                        min = 1900,
                        max = 2020,
                        value = 2010,
                        sep = "",
                        step = 5)
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           leafletOutput("leafPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$leafPlot <- renderLeaflet({
        
        dplyr::filter(urban_agglomerations, 
                      year == input$year) %>%

        leaflet() %>%
            addTiles() %>%
            addMarkers()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
