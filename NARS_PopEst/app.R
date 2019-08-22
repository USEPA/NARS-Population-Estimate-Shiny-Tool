#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(spsurvey)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NARS Population Estimate Calculation Tool"),
   
   # Sidebar panel for inputs
   sidebarLayout(
      sidebarPanel(
        # Input: Select a file ---
        fileInput(inputId='file1', buttonLabel='Browse...', 
                  label='Select CSV file for analysis',
                  multiple=FALSE, accept=c('text/csv','text/comma-separated-values,text/plain','.csv')
                  ),
      # Horizontal line ----
      tags$hr(),
      # Input: checkbox if file has header, default to TRUE ----
      checkboxInput('header','Header',TRUE),
      # Input: Select delimiter ----
      radioButtons("sep","Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Horizontal line
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp","Display",
                   choices = c(Head = 'head',
                               All = 'all'),
                   selected='head'),
      
      selectizeInput("siteVar","Site variable", choices=NULL, multiple=FALSE),
      selectizeInput("weightVar","Weight variable", choices=NULL, multiple=FALSE),
      selectizeInput("respVar","Response variable(s)", choices=NULL, multiple=TRUE),
      selectizeInput("subpopVar","Subpopulation variable(s)", choices=NULL, multiple=TRUE)
      
   ),
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("contents")
      )
    )
  )
# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   
  dataIn <- reactive({
    file1 <- input$file1
    req(file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep)
    vars <- colnames(df)
    
    updateSelectizeInput(session, "weightVar", "Select weight variable", choices=vars)
    updateSelectizeInput(session, 'respVar', 'Select up to 5 response variables', choices=vars, selected = NULL, 
                         options = list(maxItems=5))
    updateSelectizeInput(session, 'siteVar', 'Select site variable', choices=vars)
    updateSelectizeInput(session, 'subpopVar', 'Select up to 5 subpopulation variables', choices=vars, selected=NULL,
                         options = list(maxItems=5))
    
    df
  })
  
  
  output$contents <- renderTable({
  
     if(input$disp == 'head'){
       return(head(dataIn()))
     } else{
       return(dataIn())
     }
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

