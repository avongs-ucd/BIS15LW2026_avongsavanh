library(tidyverse)
library(shiny)
library(shinythemes)
library(janitor)

ui <- fluidPage(
  
  theme = shinythemes::shinytheme("cyborg"), 
  
  titlePanel("Elephant Age/Height vs. Sex"), 
  

    selectInput("y", 
                "Select Variable of Interest",
                choices = c("age",
                            "height"),
                selected = "age"),
  
  
    helpText("Data source: Lee et al. (2013) Enduring consequences of early experiences: 40 year effects on survival and success among African elephants (Loxodonta africana)"),
    
    plotOutput("plot",
               width = "600px",
               height = "500px")
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    elephants %>% 
      ggplot(aes(x = sex, y = .data[[input$y]], fill = sex)) +
      geom_boxplot(alpha = 0.8) +
      labs(title = "Distribution of Age/Height by Sex", 
           x = "sex", 
           y = input$y) +
      theme_minimal()
  })
  
}

shinyApp(ui, server)