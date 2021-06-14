#a
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Estimate Biomass"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("years",
                        "Years:",
                        min = 1956,
                        max = 2019,
                        value = 1956)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        ggplot(bt2[bt2$yr==input$years,],aes(x=bt,fill=factor(ell_rep)))+
            geom_histogram()
        #hist(bt2[bt2$yr==input$years,]$bt)
        
        # draw the histogram with the specified number of bins
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
