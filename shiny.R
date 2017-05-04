# Shiny Tutorial
# Erin Keller
# 26 April 2017

# Template

library(shiny)
ui <- fluidPage()

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)



# Basic Example

library(shiny)
ui <- fluidPage(
  selectInput(inputId= "num", # unlike the first example of a histogram of random numbers, we can use a drop-down menu rather than a slider. Easy fix by replacing sliderInput with selectInput and change values to choices
              label = "Choose a number",
              choices = c(10,20,30,40,50,60,70,80,90,100)), # choices reflect what values the user can select
  plotOutput("hist"))

server <- function(input, output) {
  output$hist <- renderPlot({
    title <- "random normal values"
    hist(rnorm(input$num), main= title)})
}

shinyApp(ui = ui, server = server)
