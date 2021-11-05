library(shinyvalidate)
library(DT)
myData <- read.csv("./table.csv")

ui <- fluidPage( 
 includeCSS("styles.css"),
  tags$h1("World's Population stats"),
  
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          sliderInput(inputId = "year", 
                      label="Choose how many years you want to see", 
                      min = 1, 
                      max = 70,
                      value = 5)
          
        ),
        wellPanel(
          checkboxGroupInput(
            inputId = "variables",
            label = "Choose the options to display:",
            choices = names(myData),
            selected = names(myData)
          ),
          submitButton(text = "Apply changes", icon = NULL, width = "100%")
        ),
        wellPanel(
          tags$h3("All the data was taken from www.worldometers.info"),
          tags$ul(),
          tags$li("Year"),
          tags$li("World population"),
          tags$li("Yearly change (in %)"),
          tags$li("Net change (plain number)"),
          tags$li("Density ( people per sq.km)"),
          tags$li("Urban population"),
          tags$li("Per cent of urban population")
        )
      ),
      mainPanel(
        wellPanel(tags$h3("You are viewing stats for", 
                          textOutput(outputId = "year", 
                                    inline = TRUE),
                          "years"
                          )
                    ),
        wellPanel(
          DTOutput("mainTable")
        )
      )
    )
)

server <- function(input, output) {
# checking values for age
  observeEvent(input$age, {
    
      inputNum <- input$age
      defaultNum <- 5
      if(inputNum > 70) {
        inputNum <- defaultNum
        showNotification( type = "error", 
                          "The number you tried to use exceeds max value of 100, 
                        so default was applied.")
      }
      if(inputNum < 1) {
        inputNum <- defaultNum
        showNotification( type = "error", 
                          "The number you tried to use is invalid, 
                        so default was applied.")
      }
      
      output$headerAge <- renderText( { inputNum } ) 
  })
  reactYearNum <- reactive(input$year)
 
  
  output$year <- renderText( { input$year } )
 
  output$headerGender <- renderText( { input$gender } ) 
  output$mainTable <- renderDT(
    myData[input$variables], options = list(
      columnDefs = list(list(className = 'dt-center')),
      pageLength = reactYearNum(),
      lengthMenu = c(1, 5, 10, 15, 30, 70)
    )
  )
}

shinyApp(ui = ui, server = server) 