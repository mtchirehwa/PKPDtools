#
# This is a Shiny web application to visualize clearance values for a hepatic model. You can run the application by clicking
# the 'Run App' button above.

# Check if required packages are already installed otherwise install
if (!require(shiny)) install.packages('shiny')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(ggthemes)) install.packages('ggthemes')
if (!require(plotly)) install.packages('plotly')

# Load packages
library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exploring hepatic model"),
    
    # Sidebar with a slider input for QH 
    sidebarLayout(
        sidebarPanel(
            sliderInput("QH",
                        "Select QH:",
                        min = 1,
                        max = 200,
                        value = 90),
            
            # Sidebar to select maximum CLint
            sliderInput("CLintmax",
                        "Select upper limit of CLint:",
                        min = 1,
                        max = 200,
                        value = 90),
            
            # Sidebar to select Fu
            sliderInput("fu",
                        "Select fraction unbound:",
                        min = 0,
                        max = 1,
                        value = 0.5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
      CLINT <- seq(0, input$CLintmax, input$CLintmax/100)
      CLHEP <- CLINT*input$QH*input$fu/(input$QH + CLINT*input$fu)
      EH <- CLINT*input$fu/(input$QH + CLINT*input$fu)
      FH <- 1 - EH
      CLF <- CLHEP/FH
      
      ay <- list(
        tickfont = list(color = "black"),
        overlaying = "y",
        #nticks = ,
        side = "right",
        title = "Hepatic extraction")

      
    plot_ly() %>%
        add_lines(x = CLINT, y = CLHEP, name = "Hepatic CL", color = I("green")) %>%
        add_lines(x = CLINT, y = CLF, name = "Oral CL", color = I("blue")) %>%
        add_lines(x = CLINT, y = EH*100, name = "Hepatic extraction", 
                  yaxis = "y2", color = I("red")) %>%
        layout(
          title = "", 
          yaxis2 = ay,
          yaxis = list(title = "Clearance"),
          xaxis = list(title = "CLint"),
          legend = list(x = 105, 
                        y = .9)
        )
     
    })
}

# Run the application
shinyApp(ui = ui, server = server)

# To test launching in browser
