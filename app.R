library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)

ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("Relative Fat Mass",
                           tabPanel("Preface", 
                                    div(includeMarkdown("preface.md"), 
                                        align="justify")
                           ), # Preface
                           
                           tabPanel("RFM Calculator",
                                    # Input values
                                    sidebarPanel(
                                      radioButtons("Gender", label = h4("Gender"),
                                                   choices = list("Male" = 1, "Female" = 2), 
                                                   selected = 2),
                                      numericInput("Height", label = "Your Height (in cm)", value = 150, min = 0),
                                      numericInput("WC", label = "Your Waist Circumference (in cm)", value = 55, min = 0),
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Output')), # Status/Output Text Box
                                      verbatimTextOutput('contents'),
                                      tableOutput('tabledata'), # Results table
                                      div(style="display: inline-block;vertical-align:top; width: 300px;",uiOutput("show"))
                                    )
                           ), # Calculator
                           
                           tabPanel("Obesity World Map",
                                    # Input values
                                    mainPanel(
                                      titlePanel(h2("Obesity World Map 2016")),
                                      titlePanel(h4("The map shows the prevalence of obesity among adults in 2016 collected by World Health Organization (WHO).")),
                                      titlePanel(h4("The gradient colour of the dot map represent the the gradient from highest average to the lowest index.")),
                                      leafletOutput("map", width = "1400px", height = "500px")
                                    )
                           ), # Map
                           
                           tabPanel("Categories", 
                                    titlePanel("Categories"), 
                                    div(includeMarkdown("categories.md"), 
                                        align="justify")
                           ) # Categories
                ) # Navigation Bar
) # web Page

server <- function(input, output, session) {
  # Input Data
  datasetInput <- reactive({  
    
    if(input$Gender == 1){
      rfm <- 64 - (20 * (input$Height / input$WC))
    }
    else if(input$Gender == 2){
      rfm <- 76 - (20 * (input$Height / input$WC))
    }
    rfm <- data.frame(rfm)
    names(rfm) <- "RFM"
    print(rfm)
  })
  
  checking <- reactive({  
    
    if(input$Gender == 1){
      rfm <- 64 - (20 * (input$Height / input$WC))
    }
    else if(input$Gender == 2){
      rfm <- 76 - (20 * (input$Height / input$WC))
    }
    return(rfm)
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    datasetInput()
    
  })
  
  output$show <- renderUI({
    if (input$Gender == 2) {
      if(checking()<11)
        h2("Under essential fat",br(),tags$image(src= 'SF1.jpg', type = 'image/jpg', width='200px',height='600px'))
      else if(checking()<14)
        h2("Essential fats",br(),tags$image(src= 'SF2.jpg', type = 'image/jpg', width='200px',height='600px'))
      else if(checking()<21)
        h2("Athletic",br(),tags$image(src= 'SF3.jpg', type = 'image/jpg', width='200px',height='600px'))
      else if(checking()<25)
        h2("Fit",br(),tags$image(src= 'SF4.jpg', type = 'image/jpg', width='200px',height='600px'))
      else if(checking()<32)
        h2("Average",br(),tags$image(src= 'SF5.jpg', type = 'image/jpg', width='200px',height='600px'))
      else if(checking()>=32)
        h2("Obese",br(),tags$image(src= 'SF6.jpg', type = 'image/jpg', width='200px',height='600px'))
    }
    else if (input$Gender == 1) {
      if(checking()<2)
        h2("Under essential fat",br(),tags$image(src= 'SM1.jpg', type = 'image/jpg', width='200px',height='600px'))
      else if(checking()<6)
        h2("Essential fats",br(),tags$image(src= 'SM2.jpg', type = 'image/jpg', width='200px',height='600px'))
      else if(checking()<14)
        h2("Athletic",br(),tags$image(src= 'SM3.jpg', type = 'image/jpg', width='200px',height='600px'))
      else if(checking()<18)
        h2("Fit",br(),tags$image(src= 'SM4.jpg', type = 'image/jpg', width='200px',height='600px'))
      else if(checking()<25)
        h2("Average",br(),tags$image(src= 'SM5.jpg', type = 'image/jpg', width='200px',height='600px'))
      else if(checking()>=25)
        h2("Obese",br(),tags$image(src= 'SM6.jpg', type = 'image/jpg', width='200px',height='600px'))
    }
  })
  
  output$map <- renderLeaflet({
    dataRFM <- read.csv("data.csv")
    dataRFM <- dataRFM%>%mutate(popup_info = paste("Prevelence Obesity among adults in",Location,"<br/>",Value))
    colours <- c("green","red")
    pal <- colorFactor(colours, dataRFM$FactValueNumeric)
    leaflet()%>%addTiles()%>% addCircleMarkers(data = dataRFM, lng = ~longitude, lat = ~latitude, radius = ~3,popup = ~popup_info, color = ~pal(FactValueNumeric))
  }) 
  
}

shinyApp(ui = ui, server = server)