library(shiny)
library(shinythemes)


# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  # theme = "yeti",  # <--- To use a theme, uncomment this
                  "HF Calculator",
                  tabPanel("Opis danych", 
                           sidebarPanel(),
                           mainPanel(
                             h1("Header 1"))
                           
                           
                           
                           
                           
                           
                           ),
                  
                  
                  
                  
                  
                  
                  tabPanel("Sprawdź swojego pacjenta",
                           sidebarPanel(
                             numericInput("ST_Slope",label="ST_Slope",value="5"),
                             numericInput("ChestPainType",label="ChestPainType",value="1"),
                             numericInput("Oldpeak",label="Oldpeak",value="1"),
                             numericInput("MaxHR",label="MaxHR",value="172"),
                             numericInput("Cholesterol",label="Cholesterol",value="289"),
                             numericInput("ExcerciseAngina",label="ExcerciseAngina",value="1"),
                             numericInput("Age",label="Age",value="49"),
                             numericInput("RestingBP",label="RestingBP",value="140"),
                             actionButton("Action", "Analyze")
                           ), # sidebarPanel
                           mainPanel(
                             h1("Header 1"),
                             
                             h4("Output 1"),
                             verbatimTextOutput("txtout"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  
                  tabPanel("Navbar 3", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)