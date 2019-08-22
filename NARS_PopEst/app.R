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
                   selected='head')
      
   ),
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("contents")
      )
    )
  )
# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$contents <- renderTable({
      
     # input$file1 will be NULL initially. After the user selects
     # and uploads a file, head of that data file by default,
     # or all rows if selected, will be shown.
     req(input$file1)
     
     df <- read.csv(input$file1$datapath,
                    header = input$header,
                    sep = input$sep)
     
     if(input$disp == 'head'){
       return(head(df))
     } else{
       return(df)
     }
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

