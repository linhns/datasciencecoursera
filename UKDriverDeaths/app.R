library(shiny)
library(plotly)
library(dplyr)
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("UK Driver Deaths 1969 - 1984"),
    sidebarLayout(
        sidebarPanel(
            h5("Move the slider to select start/end time"),
            sliderInput("Period",
                        "From / To Year:",
                        min = 1969,
                        max = 1984,
                        value = c(1980, 1984))
        ),

        mainPanel(
            h3("Plot"),
           plotlyOutput("plot")
        )
    )
)

server <- function(input, output) {

    output$plot <- renderPlotly({
        # generate bins based on input$bins from ui.R
        data <- window(UKDriverDeaths, start=c(input$Period[1], 1), 
                       end=c(input$Period[2], 12))
        df <- as.data.frame(data)
        df <- df %>% 
            gather(key, accidents) %>%
            mutate(time = time(data))
        plot <- plot_ly(df, x = ~time, y = ~accidents, type = "scatter", 
                        mode ="lines", color = I("violet"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
