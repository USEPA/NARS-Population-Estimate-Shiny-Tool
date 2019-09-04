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
   navbarPage(title="NARS Population Estimate Calculation Tool",
              selected='prepdata',position='static-top',
   
      tabPanel(title='Prepare Data for Analysis',value="prepdata",
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
              selectizeInput("coordxVar","Select the X coordinate variable (or longitude)", choices=NULL, multiple=FALSE),
              selectizeInput("coordyVar","Select the Y coordinate variable (or latitude)", choices=NULL, multiple=FALSE),
              selectizeInput("weightVar","Select weight variable", choices=NULL, multiple=FALSE),
              selectizeInput("respVar","Select up to 5 response variables", choices=NULL, multiple=TRUE),
              selectizeInput("subpopVar","Select up to 5 subpopulation variables", choices=NULL, multiple=TRUE),
              checkboxInput('natpop','Include national estimates?',TRUE)
            ),
            
            column(4,
                   checkboxInput('locvar',"Use local neighborhood variance"),
                   conditionalPanel(condition = 'input.locvar == true',
                                    checkboxInput("xy", "Convert latitude/longitude \nto Albers Projection (This is necessary \nif using local neighborhood variance.)",
                                FALSE),
                                     conditionalPanel(condition = "input.xy == true",
                                                      selectInput('sph',"Spheroid options",list('GRS80','Clarke1866','WGS84')),
                                                      textInput('clon','Center longitude (dec. deg.)',value=-96),
                                                      textInput('clat','Center latitude (dec. deg.)',value=23),
                                                      textInput('sp1','Standard parallel 1 (dec. deg.)',value=29.5),
                                                      textInput('sp2','Standard parallel 2 (dec. deg.)',value=45.5)))
            )
         ),
         
          actionButton("subsetBtn", "Prepare data for analysis"),
       
          # Show a table of the data
          tableOutput("contents")
          
        
      ),
      
      tabPanel(title="Run Population Estimates",value="runest",
          fluidRow(     
             column(3,
               radioButtons("atype","Type of Analysis (pick one)",
                            choices = c(Categorical = 'categ', Continuous = 'contin'),
                            selected='categ'),
             
             conditionalPanel(condition = "input.atype == 'contin'",
                              radioButtons("cdf_pct", "Show CDF or percentile results",
                                           choices = c(CDF = 'cdf', Percentiles = 'pct'),
                                           selected = 'pct')),
             
             actionButton('runBtn', "Run population estimates"),
             downloadButton("dwnldcsv","Save Results as .csv file")),
            
             column(6,
                    tableOutput("popest"))
          )

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
                   sep = input$sep,
                   stringsAsFactors=F)
    vars <- colnames(df)
    
    updateSelectizeInput(session, "weightVar", "Select weight variable", choices=vars)
    updateSelectizeInput(session, 'respVar', 'Select up to 5 response variables', choices=vars, selected = NULL, 
                         options = list(maxItems=5))
    updateSelectizeInput(session, 'coordxVar', 'Select the X coordinate variable (or longitude)', choices=vars, selected = NULL)
    updateSelectizeInput(session, 'coordyVar', 'Select the Y coordinate variable (or latitude)', choices=vars, selected = NULL)
    updateSelectizeInput(session, 'siteVar', 'Select site variable', choices=vars)
    updateSelectizeInput(session, 'subpopVar', 'Select up to 5 subpopulation variables', choices=vars, selected=NULL,
                         options = list(maxItems=5))
    
    df
  })
  
  dataOut <- eventReactive(input$subsetBtn,{
    if(input$subsetBtn > 0){
      df1 <- subset(dataIn(), select=c(input$siteVar, input$coordxVar, input$coordyVar, input$weightVar, input$respVar, input$subpopVar))
      if(input$xy == TRUE){
        xyCoord <- geodalbers(dataIn()[,input$coordxVar],dataIn()[,input$coordyVar],input$sph,
                              as.numeric(input$clon),as.numeric(input$clat),
                              as.numeric(input$sp1),as.numeric(input$sp2))
        
        df1 <- cbind(df1,xyCoord) %>%
          mutate(siteID=eval(as.name(input$siteVar)), wgt = eval(as.name(input$weightVar))) %>%
          subset(select=c('siteID','wgt','xcoord','ycoord',input$respVar,input$subpopVar))
         
      }else{
        #df1 <- dplyr::rename(df1, c(input$coordxVar='xcoord',input$coordyVar='ycoord'))
        df1 <- mutate(df1, xcoord = eval(as.name(input$coordxVar)), ycoord = eval(as.name(input$coordyVar)),
                      siteID = eval(as.name(input$siteVar)), wgt = eval(as.name(input$weightVar))) %>%
          subset(select = c('siteID','wgt','xcoord','ycoord',input$respVar,input$subpopVar))
        
      }
    }else{
      df1 <- dataIn()
    }
  }

  )

  output$contents <- renderTable({
    if(input$subsetBtn > 0){
      dataOut() %>%
        head 
    }else{
      if(input$disp == 'head'){
        return(head(dataIn()))
      }else{
        return(dataIn())
      }
    }
  }, digits=5)
  
  dataEst <- reactive({
    if(input$runBtn > 0){
      dfIn <- dataOut() %>%
        mutate(Active=TRUE)
      
      if(input$atype=='categ'){
        if(input$locvar == TRUE){
          cat.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                       subpop=subset(dfIn,select=c('siteID',input$subpopVar)),
                       design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                       data.cat=subset(dfIn,select=c('siteID',input$respVar)),vartype='local')
        }else{
          cat.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                       subpop=subset(dfIn,select=c('siteID',input$subpopVar)),
                       design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                       data.cat=subset(dfIn,select=c('siteID',input$respVar)))
        }
      }else{
        if(length(input$respVar)>1){
          dfIn[,input$respVar] <- lapply(dfIn[,input$respVar], as.numeric)
        }else{
          dfIn[,input$respVar] <- as.numeric(dfIn[,input$respVar])
        }
        
        if(input$locvar==TRUE){
          if(input$cdf_pct=='cdf'){
            cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                          subpop=subset(dfIn,select=c('siteID',input$subpopVar)),
                          design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                          data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='local')$CDF
          }else{
            cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                          subpop=subset(dfIn,select=c('siteID',input$subpopVar)),
                          design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                          data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='local')$Pct
          }
        }else{
          if(input$cdf_pct=='cdf'){
            cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                                      subpop=subset(dfIn,select=c('siteID',input$subpopVar)),
                                      design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                                      data.cont=subset(dfIn,select=c('siteID',input$respVar)))$CDF
            
          }else{
            cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                                      subpop=subset(dfIn,select=c('siteID',input$subpopVar)),
                                      design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                                      data.cont=subset(dfIn,select=c('siteID',input$respVar)))$Pct
          }
          
        }
      }
      
    }
  })
  
  # Output the population estimates to a table
  output$popest <- renderTable({
    dataEst()
    
  })
  
  output$dwnldcsv <- downloadHandler(
    filename = function() {
      paste("PopulationEstimateOutput_",Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataEst(), file, row.names = FALSE)
    }
  )
  
  session$onSessionEnded(function() {
    stopApp()
  })  
}

# Run the application 
shinyApp(ui = ui, server = server)

