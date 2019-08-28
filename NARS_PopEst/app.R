#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("global.r")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NARS Population Estimate Calculation Tool"),
   
   # Sidebar panel for inputs
   
      fluidRow(
        # Input: Select a file ---
        column(3,
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
                         selected='head')),
        
      column(4,
        selectizeInput("siteVar","Select site variable", choices=NULL, multiple=FALSE),
        selectizeInput("coordxVar","Select the Albers X coordinate variable", choices=NULL, multiple=FALSE),
        selectizeInput("coordyVar","Select the Albers Y coordinate variable", choices=NULL, multiple=FALSE),
        selectizeInput("weightVar","Select weight variable", choices=NULL, multiple=FALSE),
        selectizeInput("respVar","Select up to 5 response variables", choices=NULL, multiple=TRUE),
        selectizeInput("subpopVar","Select up to 5 subpopulation variables", choices=NULL, multiple=TRUE),
        checkboxInput('natpop','Include national estimates?',TRUE)
      ),
      
      column(2,
             radioButtons("xy", "Convert latitude/longitude \nto Albers Projection",
                          choices = c(Yes = 'latlong', No = 'utm'),
                          selected = 'utm')
      )
      
      
   ),
   
      actionButton("subsetBtn", "Show selected data"),
   
      # Show a table of the data
      tableOutput("contents")
      
    
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
    updateSelectizeInput(session, 'coordxVar', 'Select the Albers X coordinate variable', choices=vars, selected = NULL)
    updateSelectizeInput(session, 'coordyVar', 'Select the Albers Y coordinate variable', choices=vars, selected = NULL)
    updateSelectizeInput(session, 'siteVar', 'Select site variable', choices=vars)
    updateSelectizeInput(session, 'subpopVar', 'Select up to 5 subpopulation variables', choices=vars, selected=NULL,
                         options = list(maxItems=5))
    
    df
  })
  
  # dataOut <- eventReactive(input$subsetBtn,{
  #   if(input$subsetBtn > 0){
  #     df1 <- subset(dataIn(), select=c(input$siteVar, input$coordxVar, input$coordyVar, input$weightVar, input$respVar, input$subpopVar))
  #   }else{
  #     df1 <- dataIn()
  #   }
  # }
  #   
  # )
  # 
  # output$contents <- renderTable({
  # 
  #    # if(input$disp == 'head'){
  #    #   return(head(dataIn()))
  #    # } else{
  #    #   return(dataIn())
  #    # }
  #   return(head(dataIn()))
  #  })
  # 
  # output$dataOut <- renderTable({
  #   dataIn() %>%
  #     subset(select=input$siteVar, input$coordxVar, input$coordyVar, input$weightVar, input$respVar, input$subpopVar) %>%
  #     head 
  # })
  
  output$contents <- renderTable({
    if(input$subsetBtn > 0){
      dataIn() %>%
        subset(select=c(input$siteVar, input$coordxVar, input$coordyVar, input$weightVar, input$respVar, input$subpopVar)) %>%
        head 
    }else{
      if(input$disp == 'head'){
        return(head(dataIn()))
      }else{
        return(dataIn())
      }
    }
  })

  session$onSessionEnded(function() {
    stopApp()
  })  
}

# Run the application 
shinyApp(ui = ui, server = server)

