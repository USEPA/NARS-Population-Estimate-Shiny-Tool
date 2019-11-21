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
ui <- fluidPage(theme = shinytheme("united"),
   shinyjs::useShinyjs(),
   # Application title
   navbarPage(title="NARS Population Estimate Calculation Tool",
              selected='instructions',position='static-top',
      tabPanel(title='Instructions for Use',value='instructions',
               p('This Shiny app allows for calculation of population estimates as performed for the National Aquatic Resource Surveys. Estimates based on categorical and continuous variables are possible. '),
               br(),
               p('Minimum requirements: All variables must be contained in one file. Only delimited files, such as comma- and tab-delimited, are accepted for upload.'),
               br(),
               p('Contact Karen Blocksom at blocksom.karen@epa.gov with questions or feedback. '),
               br(),
               h3('Disclaimer'),
               p('The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use.  EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information.  Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA.  The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.')),
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
              # selectizeInput("coordxVar","Select the X coordinate variable (or longitude) \n(required only for local neighborhood variance)", 
              #                choices=NULL, multiple=FALSE),
              # selectizeInput("coordyVar","Select the Y coordinate variable (or latitude) \n(required only for local neighborhood variance)", 
              #                choices=NULL, multiple=FALSE),
              selectizeInput("weightVar","Select weight variable", choices=NULL, multiple=FALSE),
              selectizeInput("respVar","Select up to 10 response variables - must all be either categorical or numeric", choices=NULL, multiple=TRUE),
              conditionalPanel(condition = 'input.natpop == false',
                               selectizeInput("subpopVar","Select up to 10 subpopulation variables \n(required if not national estimates only)", 
                                              choices=NULL, multiple=TRUE)),
              checkboxInput('natpop','Only overall (all sites) estimates? Select if no \nsubpopulations of interest',FALSE)
            ),
            
            column(4,
                   radioButtons('locvar',"Type of variance estimate to use (select one)",
                                choices = c('Local neighborhood variance' = 'local',
                                            'Simple Random Sample (requires stratum)' = 'srs'),
                                select = 'srs'),
                   conditionalPanel(condition = "input.locvar == 'local'",
                                    selectizeInput("coordxVar","Select the X coordinate variable (or longitude) \n(required only for local neighborhood variance)", 
                                                   choices=NULL, multiple=FALSE),
                                    selectizeInput("coordyVar","Select the Y coordinate variable (or latitude) \n(required only for local neighborhood variance)", 
                                                   choices=NULL, multiple=FALSE),
                                    
                                    checkboxInput("xy", "Convert latitude/longitude \nto Albers Projection (This is necessary \nif using local neighborhood variance.). Current projection information:",
                                FALSE),
                                     conditionalPanel(condition = "input.xy == true",
                                                      selectInput('proj',"Projection options (otherwise provide as x and y coordinates). If selecting other than GRS80 (standard NARS), supply necessary projection information.)",
                                                                  list('GRS80 (standard NARS)','Other')),
                                                                       selectInput('sph',"Spheroid options",list('GRS80','Clarke1866','WGS84')),
                                                                       textInput('clon','Center longitude (dec. deg.)',value=-96),
                                                                       textInput('clat','Center latitude (dec. deg.)',value=23),
                                                                       textInput('sp1','Standard parallel 1 (dec. deg.)',value=29.5),
                                                                       textInput('sp2','Standard parallel 2 (dec. deg.)',value=45.5)
                                                                       )
                                                      )
                   
                                                      ,
                   conditionalPanel(condition = "input.locvar == 'srs'",
                                    selectizeInput("stratumVar", "Select the stratum variable in order to calculate variance based on a simple random sample",
                                                   choices=NULL, multiple=FALSE))
            )  
         ),
         
          actionButton("subsetBtn", "Prepare data for analysis - necessary to run estimates"),
       
          # Show a table of the data
          tableOutput("contents")
        
        
      ),
      
      tabPanel(title="Run Population Estimates",value="runest",
          fluidRow(     
             column(3,
               radioButtons("atype","Type of Analysis (pick one)",
                            choices = c('Categorical ( for character variables)' = 'categ', 'Continuous (for numeric variables)' = 'contin'),
                            selected='categ'),
             
             conditionalPanel(condition = "input.atype == 'contin'",
                              radioButtons("cdf_pct", "Show CDF or percentile results",
                                           choices = c(CDF = 'cdf', Percentiles = 'pct'),
                                           selected = 'pct')),
             
             actionButton('runBtn', "Run/Refresh population estimates"),
             hr(),
             
             downloadButton("dwnldcsv","Save Results as .csv file")),
            
             column(6,
                    tableOutput("popest"))
          )

      )
     )
   )
# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  #shinyjs::disable("dwnldcsv")
  dataIn <- reactive({
    file1 <- input$file1
    req(file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   stringsAsFactors=F)
    vars <- colnames(df)
    
    updateSelectizeInput(session, "weightVar", "Select weight variable", choices=vars)
    updateSelectizeInput(session, 'respVar', 'Select up to 10 response variables - All must be either categorical or numeric', choices=vars, selected = NULL, 
                         options = list(maxItems=10))
    updateSelectizeInput(session, 'coordxVar', "Select the X coordinate variable (or longitude) \n(required only for local neighborhood variance)", 
                         choices=vars, selected = NULL)
    updateSelectizeInput(session, 'coordyVar', "Select the Y coordinate variable (or latitude) \n(required only for local neighborhood variance)", 
                         choices=vars, selected = NULL)
    updateSelectizeInput(session, 'siteVar', 'Select site variable', choices=vars)
    updateSelectizeInput(session, 'subpopVar', 'Select up to 10 subpopulation variables \n(required if not national estimates only)', choices=vars, selected=NULL,
                         options = list(maxItems=10))
    updateSelectizeInput(session, "stratumVar", "Select the stratum variable in order to calculate variance based on a simple random sample",
                         choices=vars)
    
    df
  })
  
  observe({
    x <- input$proj
    
    if(x=='GRS80 (standard NARS)'){
      updateSelectInput(session, 'sph', choices='GRS80')
      updateTextInput(session, 'clon', value = 96)
      updateTextInput(session, 'clat', value = 37.5)
      updateTextInput(session, 'sp1', value = 29.5)
      updateTextInput(session, 'sp2', value = 45.5)
    }else if(x=='Other'){
      updateSelectInput(session, 'sph', choices=x)
      updateTextInput(session, 'clon', value = 96)
      updateTextInput(session, 'clat', value = 23)
      updateTextInput(session, 'sp1', value = 29.5)
      updateTextInput(session, 'sp2', value = 45.5)
      
    }
  })
  
  dataOut <- eventReactive(input$subsetBtn,{
    if(input$subsetBtn > 0){ # Add more to this section to test for variable types 
      if(input$natpop == FALSE){
        # Use function below to validate input variables as the appropriate type and to make sure the selections do not overlap
        validate(
          need(input$subpopVar %nin% c(input$siteVar,input$weightVar,input$respVar),
               "Subpopulation variable(s) cannot overlap with other variable selections."),
          need(input$respVar %nin% c(input$siteVar,input$subpopVar,input$weightVar),
               "Response variable(s) cannot overlap with other variable selections."),
          need(input$weightVar %nin% c(input$siteVar,input$subpopVar,input$respVar),
               "Weight variable cannot overlap with other variable selections."),
          need(input$siteVar %nin% c(input$respVar,input$subpopVar,input$weightVar),
               'Site variable cannot overlap with other variable selections.')
          )
     
          if(input$locvar == 'local'){
            validate(
              need(input$coordxVar %nin% c(input$siteVar,input$coordyVar,input$respVar,input$subpopVar,input$weightVar),
                 "X-coordinate variable cannot overlap with other variable selections."),
              need(input$coordyVar %nin% c(input$siteVar,input$coordxVar,input$respVar,input$subpopVar,input$weightVar),
                 "Y-coordinate variable cannot overlap with other variable selections.")
              )
              
              df1 <- subset(dataIn(), select=c(input$siteVar, input$coordxVar, input$coordyVar, input$weightVar, input$respVar, input$subpopVar))
              if(input$xy == TRUE){
                xyCoord <- geodalbers(dataIn()[,input$coordxVar],dataIn()[,input$coordyVar],input$sph,
                                      as.numeric(input$clon),as.numeric(input$clat),
                                      as.numeric(input$sp1),as.numeric(input$sp2))
                
                df1 <- cbind(df1,xyCoord) %>%
                  mutate(siteID=eval(as.name(input$siteVar)), wgt = eval(as.name(input$weightVar))) %>%
                  subset(select=c('siteID','wgt','xcoord','ycoord',input$respVar,input$subpopVar)) %>%
                  mutate(National='National')
                
              }else{
                df1 <- mutate(df1, xcoord = eval(as.name(input$coordxVar)), ycoord = eval(as.name(input$coordyVar)),
                              siteID = eval(as.name(input$siteVar)), wgt = eval(as.name(input$weightVar))) %>%
                  subset(select = c('siteID','wgt','xcoord','ycoord',input$respVar,input$subpopVar)) %>%
                  mutate(allSites='All Sites')
                
              } 
            
          }else{
            validate(
              need(input$stratumVar %nin% c(input$siteVar,input$respVar,input$subpopVar,input$weightVar),
                   "Stratum variable cannot overlap with other variable selections.")
              )
            
            df1 <- subset(dataIn(), select=c(input$siteVar, input$stratumVar, input$weightVar, input$respVar, input$subpopVar))

            
              df1 <- mutate(df1, stratum = eval(as.name(input$stratumVar)),
                            siteID = eval(as.name(input$siteVar)), wgt = eval(as.name(input$weightVar))) %>%
                subset(select = c('siteID','wgt','stratum',input$respVar,input$subpopVar)) %>%
                mutate(allSites='All Sites')

          }
       
        

      }else{
        # Use function below to validate input variables as the appropriate type and to make sure the selections do not overlap
        validate(
          need(input$respVar %nin% c(input$siteVar,input$coordxVar,input$coordyVar,input$weightVar),
               "Response variable(s) cannot overlap with other variable selections."),
          need(input$weightVar %nin% c(input$siteVar,input$coordxVar,input$coordyVar,input$respVar),
               "Weight variable cannot overlap with other variable selections."),
          need(input$siteVar %nin% c(input$coordxVar,input$coordyVar,input$respVar,input$weightVar),
               'Site variable cannot overlap with other variable selections.')
        )
        if(input$locvar == 'local'){
          validate(
            need(input$coordxVar %nin% c(input$siteVar,input$coordyVar,input$respVar,input$subpopVar,input$weightVar),
                 "X-coordinate variable cannot overlap with other variable selections."),
            need(input$coordyVar %nin% c(input$siteVar,input$coordxVar,input$respVar,input$subpopVar,input$weightVar),
                 "Y-coordinate variable cannot overlap with other variable selections.")
          )
        }else{
          validate(
            need(input$stratumVar %nin% c(input$siteVar,input$respVar,input$subpopVar,input$weightVar),
                 "Stratum variable cannot overlap with other variable selections.")
          )
        }
        
        
        df1 <- subset(dataIn(), select=c(input$siteVar, input$coordxVar, input$coordyVar, input$weightVar, input$respVar))
        if(input$xy == TRUE){
          xyCoord <- geodalbers(dataIn()[,input$coordxVar],dataIn()[,input$coordyVar],input$sph,
                                as.numeric(input$clon),as.numeric(input$clat),
                                as.numeric(input$sp1),as.numeric(input$sp2))
          
          df1 <- cbind(df1,xyCoord) %>%
            mutate(siteID=eval(as.name(input$siteVar)), wgt = as.numeric(eval(as.name(input$weightVar)))) %>%
            subset(select=c('siteID','wgt','xcoord','ycoord',input$respVar))
          
        }else{
          #df1 <- dplyr::rename(df1, c(input$coordxVar='xcoord',input$coordyVar='ycoord'))
          df1 <- mutate(df1, xcoord = as.numeric(eval(as.name(input$coordxVar))), ycoord = as.numeric(eval(as.name(input$coordyVar))),
                        siteID = eval(as.name(input$siteVar)), wgt = as.numeric(eval(as.name(input$weightVar)))) %>%
            subset(select = c('siteID','wgt','xcoord','ycoord',input$respVar))
        } 

      }
      # Look for missing values among coordinates, weights, and response variables.
      validate(
        need(!any(is.na(df1$xcoord)), "Non-numeric or missing values for x coordinates."),
        need(!any(is.na(df1$ycoord)), "Non-numeric or missing values for y coordinates."),
        need(!any(is.na(df1$wgt)), "Non-numeric or missing values for weights.")
#        need(nrow(df1[complete.cases(df1[,input$respVar]),])==nrow(df1),'There are missing values among response variables.')
      )
      
      df1
      
    }else{
      df1 <- dataIn()
    }
  }

  )

  output$contents <- renderTable({
    
    if(input$subsetBtn > 0){
      shinyjs::disable('dwnldcsv')
      
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
  
  dataEst <- eventReactive(input$runBtn,{
  #if(input$runBtn > 0){
    dfIn <- dataOut() %>%
    mutate(Active=TRUE)
      
    withProgress(message="Calculating estimates",detail="This might take a while...",
                   value=0,{  
      if(input$atype=='categ'){
        if(input$locvar == 'local'){
          if(input$natpop == FALSE){
            cat.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                         subpop=subset(dfIn,select=c('siteID','allSites',input$subpopVar)),
                         design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                         data.cat=subset(dfIn,select=c('siteID',input$respVar)),vartype='local')
          }else{
            cat.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                         design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                         data.cat=subset(dfIn,select=c('siteID',input$respVar)),vartype='local')
            
          }
        }else{
          if(input$natpop == FALSE){
            cat.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                         subpop=subset(dfIn,select=c('siteID','allSites',input$subpopVar)),
                         design=subset(dfIn,select=c('siteID','wgt','stratum')),
                         data.cat=subset(dfIn,select=c('siteID',input$respVar)),vartype='SRS')
          }else{
            cat.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                         design=subset(dfIn,select=c('siteID','wgt','stratum')),
                         data.cat=subset(dfIn,select=c('siteID',input$respVar)),vartype='SRS')
          }
        }
        
      }else{
        if(length(input$respVar)>1){
          dfIn[,input$respVar] <- lapply(dfIn[,input$respVar], as.numeric)
        }else{
          dfIn[,input$respVar] <- as.numeric(dfIn[,input$respVar])
        }
        
        # validate(
        #   need(nrow(dfIn[complete.cases(dfIn[,input$respVar]),])==nrow(dfIn),'There are non-numeric values among response variables.')
        # )
        # 
        
        if(input$locvar=='local'){
          if(input$natpop == FALSE){
            if(input$cdf_pct=='cdf'){
              cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                            subpop=subset(dfIn,select=c('siteID','allSites',input$subpopVar)),
                            design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                            data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='local')$CDF
            }else{
              cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                            subpop=subset(dfIn,select=c('siteID','allSites',input$subpopVar)),
                            design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                            data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='local')$Pct
            }
          }else{
            if(input$cdf_pct=='cdf'){
              cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                            design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                            data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='local')$CDF
            }else{
              cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                            design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                            data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='local')$Pct
            }
            
          }
        }else{
          if(input$natpop == FALSE){
            if(input$cdf_pct=='cdf'){
              cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                                        subpop=subset(dfIn,select=c('siteID','allSites',input$subpopVar)),
                                        design=subset(dfIn,select=c('siteID','wgt','stratum')),
                                        data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='SRS')$CDF
              
            }else{
              cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                                        subpop=subset(dfIn,select=c('siteID','allSites',input$subpopVar)),
                                        design=subset(dfIn,select=c('siteID','wgt','stratum')),
                                        data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='SRS')$Pct
            }
          }else{
            if(input$cdf_pct=='cdf'){
              cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                            design=subset(dfIn,select=c('siteID','wgt','stratum')),
                            data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='SRS')$CDF
              
            }else{
              cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                            design=subset(dfIn,select=c('siteID','wgt','stratum')),
                            data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='SRS')$Pct
            }
            
          }
          
        }
      }
     
    })
    
  })
  
  
  # Output the population estimates to a table
  output$popest <- renderTable({
        dataEst()
  })

  
  observe({shinyjs::toggleState('dwnldcsv',length(dataEst())!=0)})
  
  output$dwnldcsv <- downloadHandler(
    filename = function() {
      if(input$atype=='categ'){
        paste("Categorical_PopEstOutput_",Sys.Date(), ".csv", sep = "")
      }else{
        if(input$cdf_pct=='cdf'){
          paste("Continuous_CDF_Output_",Sys.Date(), ".csv", sep = "")
        }else{
          paste("Continuous_Percentiles_Output_",Sys.Date(), ".csv", sep = "")
        }
      }
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

