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
      # Panel with instructions for using this tool
      tabPanel(title='Instructions for Use',value='instructions',
               h2("Overview"),
               p('This Shiny app allows for calculation of population estimates as performed for the National Aquatic Resource Surveys (NARS). Estimates based on categorical and continuous variables are possible. This app does not include all possible options but does allow typical settings used by NARS for creating population estimates.'),
               h3("Instructions for Use"),
               h4("Prepare Data for Analysis tab"),
               tags$ol(
                 tags$li("Select data file and upload."),
                 tags$li("The variables in that file will populate dropdown lists on that tab."),
                 tags$li("Select variables to serve as site IDs, weights, response variables, and subpopulations (if desired). If only overall or 'national' estimates are desired, check the box for overall analysis."),
                tags$li("Select the type of variance you want to use.",
                        tags$ul(
                        tags$li("For simple random sample variance, select a stratum variable to better estimate variance."), 
                        tags$li("For local neighborhood variance (as used in NARS estimates), select coordinate variables (either in latitude/longitude or Albers projection). If coordinates are in latitude and longitude, you must provide projection information in order to convert them to Albers projection."))),
               tags$li("Click on the button above the data to subset the data before proceeding to the Run Population Estimates tab")
               ),
               br(),
               h4("Run Population Estimates"),
               tags$ol(
                 tags$li("Select the type of analysis (categorical or continuous)."),
                 tags$li("For continuous analysis, select either CDFs (cumulative distribution functions) or Percentiles."),
                 tags$li("Click on the Run/Refresh Population Estimates button. Depending on the number of responses, subpopulations, and type of analysis, it may take a few minutes."),
                 tags$li("If desired, download results to a comma-delimited file by clicking the Save Results button.")
               )
               ,
               br(),
               h4("Minimum requirements:"),
               tags$ul(
                         tags$li("All variables must be contained in one file and include site IDs, weights, response variables, subpopulations (if any), and coordinates or design stratum (depending on type of variance desired)." ),
                         tags$li("Only delimited files, such as comma- and tab-delimited, are accepted for upload."),
                         tags$li("If local neighborhood variance is desired, coordinates must be provided, either in latitude/longitude (decimal degrees) or Albers projection. If provided in latitude/longitude, projection information is needed to convert values. Only spheroid need be supplied for conversion."),
                         tags$li("If variance based on a simple random sample is desired (or if coordinates or projection information are not available), the design stratum is needed to better estimate variance.")
                       ),
               br(),
               p('Contact Karen Blocksom at blocksom.karen@epa.gov with questions or feedback.'),
               br(),
               h3('Disclaimer'),
               p('The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use.  EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information.  Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA.  The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.')),

      # Panel to import and select data to analyze
      tabPanel(title='Prepare Data for Analysis',value="prepdata",
       
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
                
                # Input: Select number of rows to display 
                radioButtons("disp","Display",
                             choices = c(Head = 'head',
                                         All = 'all'),
                             selected='head')),
            # Provide dropdown menus to allow user to select site, weight, and response variables from those in the imported dataset
            column(4,
              selectizeInput("siteVar","Select site variable", choices=NULL, multiple=FALSE),
              selectizeInput("weightVar","Select weight variable", choices=NULL, multiple=FALSE),
              selectizeInput("respVar","Select up to 10 response variables - must all be either categorical or numeric", choices=NULL, multiple=TRUE),

              # If national estimates box is NOT checked, show subpopulation dropdown list              
              conditionalPanel(condition = 'input.natpop == false',
                               selectizeInput("subpopVar","Select up to 10 subpopulation variables \n(required if not national estimates only)", 
                                              choices=NULL, multiple=TRUE)),
              checkboxInput('natpop','Only overall (all sites) estimates? Select if no \nsubpopulations of interest',FALSE)
            ),
            # Set up type of variance to use in estimates: local or SRS
            column(4,
                   radioButtons('locvar',"Type of variance estimate to use (select one)",
                                choices = c('Local neighborhood variance' = 'local',
                                            'Simple Random Sample (requires stratum)' = 'srs'),
                                select = 'srs'),
                   # If local, user must select x and y coordinates and convert to Albers if in lat/long
                   conditionalPanel(condition = "input.locvar == 'local'",
                                    selectizeInput("coordxVar","Select the X coordinate variable (or longitude) \n(required only for local neighborhood variance)", 
                                                   choices=NULL, multiple=FALSE),
                                    selectizeInput("coordyVar","Select the Y coordinate variable (or latitude) \n(required only for local neighborhood variance)", 
                                                   choices=NULL, multiple=FALSE),
                                    
                                    checkboxInput("xy", "Convert latitude/longitude \nto Albers Projection (This is necessary \nif using local neighborhood variance.). Current projection information:",
                                FALSE),
                                # If conversion from lat/long is needed, provide default as NARS typical projection info, otherwise allow user to enter
                                     conditionalPanel(condition = "input.xy == true",
                                                     p("Projection options (otherwise provide as x and y coordinates). "),
                                                     tags$body(p("Use spheroid ",strong("GRS80"),"for NAD83, ",strong("Clarke1866"),"for NAD27, and ", strong("WGS84"), "for WGS84 datum. Other values are defaults assumed for NARS.")),
                                                                       selectInput('sph',"Spheroid options",list('GRS80','Clarke1866','WGS84')),
                                                                       textInput('clon','Center longitude (dec. deg.)',value=-96),
                                                                       textInput('clat','Center latitude (dec. deg.)',value=37.5),
                                                                       textInput('sp1','Standard parallel 1 (dec. deg.)',value=29.5),
                                                                       textInput('sp2','Standard parallel 2 (dec. deg.)',value=45.5)
                                                                       )
                                                      ),
                   
                   # Select stratum if Simple Random Sample                                 
                   conditionalPanel(condition = "input.locvar == 'srs'",
                                    selectizeInput("stratumVar", "Select the stratum variable in order to calculate variance based on a simple random sample",
                                                   choices=NULL, multiple=FALSE))
            )  
         ),
          # Press button to subset data for analysis - must click here first
          actionButton("subsetBtn", "Click HERE to prepare data for population estimates"),
       
          # Show a table of the data
          h4("Data for Analysis"),
          tableOutput("contents")
        
        
      ),
      # Tab to run population estimates
      tabPanel(title="Run Population Estimates",value="runest",
          fluidRow(     
             column(3, 
               # User must select categorical or continuous variable analysis, depending on response variables selected
               radioButtons("atype","Type of Analysis (pick one)",
                            choices = c('Categorical ( for character variables)' = 'categ', 'Continuous (for numeric variables)' = 'contin'),
                            selected='categ'),
             # If continuous analysis selected, select whether CDFs or percentiles are desired.  
             conditionalPanel(condition = "input.atype == 'contin'",
                              radioButtons("cdf_pct", "Show CDF or percentile results",
                                           choices = c(CDF = 'cdf', Percentiles = 'pct'),
                                           selected = 'pct')),
             # Once data are prepared, user needs to click to run estimates, or rerun estimates on refreshed data
             actionButton('runBtn', "Run/Refresh population estimates"),
             hr(),
             # Click to download results into a comma-delimited file
             downloadButton("dwnldcsv","Save Results as .csv file")),
             # Show results here
             column(8,
                    h4("Warnings"),
                    tableOutput("warnest"),
                    h4("Analysis Output"),
                    tableOutput("popest"))
          )

      )
     )
   )
# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  # Read in data file as selected
  dataIn <- reactive({
    file1 <- input$file1
    req(file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   stringsAsFactors=F)
    vars <- colnames(df)
    
    df
  })
  # Use current dataset to refresh dropdown list of variables.
  observe({
    vars <- colnames(dataIn())
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
    
  })
    

  # Once subset button is clicked, validate selections to make sure any variable only occurs in set of selections
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
          # If local variance selected, make sure x and y coordinate variables do not overlap with any already selected     
          if(input$locvar == 'local'){
            validate(
              need(input$coordxVar %nin% c(input$siteVar,input$coordyVar,input$respVar,input$subpopVar,input$weightVar),
                 "X-coordinate variable cannot overlap with other variable selections."),
              need(input$coordyVar %nin% c(input$siteVar,input$coordxVar,input$respVar,input$subpopVar,input$weightVar),
                 "Y-coordinate variable cannot overlap with other variable selections.")
              )
              # For conversion to Albers projection, select necessary variables and use geodalbers function from spsurvey
              df1 <- subset(dataIn(), select=c(input$siteVar, input$coordxVar, input$coordyVar, input$weightVar, input$respVar, input$subpopVar))
              if(input$xy == TRUE){
                xyCoord <- geodalbers(dataIn()[,input$coordxVar],dataIn()[,input$coordyVar],input$sph,
                                      as.numeric(input$clon),as.numeric(input$clat),
                                      as.numeric(input$sp1),as.numeric(input$sp2))
                # Combine x and y coordinates back with set of selected variables, select
                df1 <- cbind(df1, xyCoord)
                df1 <- subset(df1, select=c(input$siteVar,'xcoord','ycoord',input$weightVar,input$respVar,input$subpopVar))
               # If conversion not needed, select variables and rename them to required names 
              }else{
                df1 <- subset(df1, select=c(input$siteVar,input$coordxVar,input$coordyVar,input$weightVar,input$respVar,input$subpopVar))
                df1$xcoord <- with(df1, eval(as.name(input$coordxVar)))
                df1$ycoord <- with(df1, eval(as.name(input$coordyVar)))

              } 
              df1$siteID <- with(df1, eval(as.name(input$siteVar)))
              df1$wgt <- with(df1, eval(as.name(input$weightVar)))
              df1 <- subset(df1, select = c('siteID','wgt','xcoord','ycoord',input$respVar,input$subpopVar))
              
              
          # If local variance not used (SRS selected), need stratum variable but not coordinates 
          }else{
            # validate variable for stratum to make sure it does not overlap with other variables selected
            validate(
              need(input$stratumVar %nin% c(input$siteVar,input$respVar,input$subpopVar,input$weightVar),
                   "Stratum variable cannot overlap with other variable selections.")
              )
            # Subset the data to the variables selected, then rename any to required names.
            df1 <- subset(dataIn(), select=c(input$siteVar, input$stratumVar, input$weightVar, input$respVar, input$subpopVar))

            
            df1$stratum <- with(df1, eval(as.name(input$stratumVar)))
            df1$siteID <- with(df1, eval(as.name(input$siteVar)))
            df1$wgt <- with(df1, eval(as.name(input$weightVar)))
            df1 <- subset(df1, select = c('siteID','wgt','stratum',input$respVar,input$subpopVar))

          }
          df1$allSites <- 'All Sites'
        
      # If All Sites only estimates selected, changes selection of data for analysis
      }else{
        # Use function below to validate input variables as the appropriate type and to make sure the selections do not overlap
        validate(
          need(input$respVar %nin% c(input$siteVar,input$weightVar),
               "Response variable(s) cannot overlap with other variable selections."),
          need(input$weightVar %nin% c(input$siteVar,input$respVar),
               "Weight variable cannot overlap with other variable selections."),
          need(input$siteVar %nin% c(input$respVar,input$weightVar),
               'Site variable cannot overlap with other variable selections.')
        )
        # if local neighborhood variance used here, make sure coordinates variables do not overlap with other selections
        if(input$locvar == 'local'){
          validate(
            need(input$coordxVar %nin% c(input$siteVar,input$coordyVar,input$respVar,input$weightVar),
                 "X-coordinate variable cannot overlap with other variable selections."),
            need(input$coordyVar %nin% c(input$siteVar,input$coordxVar,input$respVar,input$weightVar),
                 "Y-coordinate variable cannot overlap with other variable selections.")
          )
          # Subset the data to selected variables
          df1 <- subset(dataIn(), select=c(input$siteVar, input$coordxVar, input$coordyVar, input$weightVar, input$respVar))
          # If conversion from lat/long is necessary, convert, then rename variables with required names
          if(input$xy == TRUE){
            xyCoord <- geodalbers(dataIn()[,input$coordxVar],dataIn()[,input$coordyVar],input$sph,
                                  as.numeric(input$clon),as.numeric(input$clat),
                                  as.numeric(input$sp1),as.numeric(input$sp2))
            
            df1 <- cbind(df1, xyCoord)

          }else{ # Conversion is not necessary, so just rename and subset variables
            
            df1$xcoord <- as.numeric(eval(as.name(input$coordxVar)))
            df1$ycoord <- as.numeric(eval(as.name(input$coordyVar)))
          } 
          df1$siteID <- eval(as.name(input$siteVar))
          df1$wgt <- as.numeric(eval(as.name(input$weightVar)))
          df1 <- subset(df1, select = c('siteID','wgt','xcoord','ycoord',input$respVar))
          
        # if SRS selected, make sure stratum does not overlap with other variable selections
        }else{
          validate(
            need(input$stratumVar %nin% c(input$siteVar,input$respVar,input$weightVar),
                 "Stratum variable cannot overlap with other variable selections.")
          )
          
          # subset to the variables needed
          df1 <- subset(dataIn(), select = c(input$siteVar, input$weightVar, input$stratumVar, input$respVar))
          df1$siteID <- eval(as.name(input$siteVar))
          df1$wgt <- as.numeric(eval(as.name(input$weightVar)))
          df1$stratum <- eval(as.name(input$stratumVar))
          df1 <- subset(df1, select = c('siteID','wgt','stratum',input$respVar))

        }
        
        # If all sites data coordinates need to be converted to Albers projection - NEED TO ADD STRATUM VAR HERE
    

      }
      # Look for missing values among coordinates and weights only - response and subpopulation variables can have missing values
      validate(
        need(!any(is.na(df1$xcoord)), "Non-numeric or missing values for x coordinates."),
        need(!any(is.na(df1$ycoord)), "Non-numeric or missing values for y coordinates."),
        need(!any(is.na(df1$wgt)), "Non-numeric or missing values for weights.")
      )
      
      df1
    # This is what shows up before the subset button is clicked  
    }else{
      df1 <- dataIn()
    }
  }

  )
  # Show table on data prep tab to 5 digits
  output$contents <- renderTable({
    
    if(input$subsetBtn > 0){
      shinyjs::disable('dwnldcsv')
      
      dataOut()
      head(dataOut())

    }else{
      if(input$disp == 'head'){
        return(head(dataIn()))
      }else{
        return(dataIn())
      }
    }
  }, digits=5)
  
  
  # Calculate population estimates 
  dataEst <- eventReactive(input$runBtn,{
    if(exists("warn.df") && is.data.frame(get("warn.df"))){
      rm("warn.df", envir=.GlobalEnv)
      print('exists')
    }else{
      print("does not")
    }
    
    dfIn <- dataOut()
    dfIn$Active <- TRUE
    # Show progress bar for a certain about of time while calculations are running  
    withProgress(message="Calculating estimates",detail="This might take a while...",
                   value=0,{  
      # User selected categorical analysis, set up cat.analysis function depending on previous selections
      if(input$atype=='categ'){
        if(input$locvar == 'local'){ # Local neighborhood variance selected
          if(input$natpop == FALSE){ # Not overall analysis,  includes subpopulations
            estOut <- cat.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                         subpop=subset(dfIn,select=c('siteID','allSites',input$subpopVar)),
                         design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                         data.cat=subset(dfIn,select=c('siteID',input$respVar)),vartype='Local')
            
          }else{ # No subpopulations selected
            estOut <- cat.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                         design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                         data.cat=subset(dfIn,select=c('siteID',input$respVar)),vartype='Local')
            
          }
        }else{ # SRS variance
          if(input$natpop == FALSE){ # Not overall analysis, includes subpopulations
            estOut <- cat.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                         subpop=subset(dfIn,select=c('siteID','allSites',input$subpopVar)),
                         design=subset(dfIn,select=c('siteID','wgt','stratum')),
                         data.cat=subset(dfIn,select=c('siteID',input$respVar)),vartype='SRS')

            
          }else{ # No subpopulations selected
            estOut <- cat.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                         design=subset(dfIn,select=c('siteID','wgt','stratum')),
                         data.cat=subset(dfIn,select=c('siteID',input$respVar)),vartype='SRS')
          
          }
        }
        
      }else{ # continuous analysis selected, begin by converting to numeric
        if(length(input$respVar)>1){ # if more than one response variable selected
          dfIn[,input$respVar] <- lapply(dfIn[,input$respVar], as.numeric)
        }else{ # If only one response variable selected
          dfIn[,input$respVar] <- as.numeric(dfIn[,input$respVar])
        }
        
        if(input$locvar=='local'){ # Local neighborhood variance selected
          if(input$natpop == FALSE){ # Subpopulations selected
            if(input$cdf_pct=='cdf'){ # Produce CDFs
              estOut <- cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                            subpop=subset(dfIn,select=c('siteID','allSites',input$subpopVar)),
                            design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                            data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='Local')$CDF
            }else{ # Just produce percentiles
              estOut <- cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                            subpop=subset(dfIn,select=c('siteID','allSites',input$subpopVar)),
                            design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                            data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='Local')$Pct
            }
          }else{ # No subpopulations selected, only overall analysis
            if(input$cdf_pct=='cdf'){ # CDFs of interest
              estOut <- cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                            design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                            data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='Local')$CDF
            }else{ # Percentiles of interest
              estOut <- cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                            design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord')),
                            data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='Local')$Pct
            }
            
          }
        }else{ # SRS variance selected
          if(input$natpop == FALSE){ # Subpopulations selected
            if(input$cdf_pct=='cdf'){ # CDFs of interest
              estOut <- cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                                        subpop=subset(dfIn,select=c('siteID','allSites',input$subpopVar)),
                                        design=subset(dfIn,select=c('siteID','wgt','stratum')),
                                        data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='SRS')$CDF
              
            }else{ # Percentiles of interest
              estOut <- cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                                        subpop=subset(dfIn,select=c('siteID','allSites',input$subpopVar)),
                                        design=subset(dfIn,select=c('siteID','wgt','stratum')),
                                        data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='SRS')$Pct
            }
          }else{ # No subpopulations selected
            if(input$cdf_pct=='cdf'){ # CDFs 
              estOut <- cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                            design=subset(dfIn,select=c('siteID','wgt','stratum')),
                            data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='SRS')$CDF
              
            }else{ # Percentiles
              estOut <- cont.analysis(sites=subset(dfIn,select=c('siteID','Active')),
                            design=subset(dfIn,select=c('siteID','wgt','stratum')),
                            data.cont=subset(dfIn,select=c('siteID',input$respVar)),vartype='SRS')$Pct
            }
            
          }
          
        }
      }
                    
    })
    if(exists('warn.df') && ncol(warn.df)>1){
      outdf <- list(estOut=estOut, warndf=warn.df)
    }else{
      outdf <- list(estOut=estOut, warndf=data.frame(warnings='none'))
    }
    
    
  })
  
  
  # Output the population estimates to a table
  output$popest <- renderTable({
    dataEst()[['estOut']]
  })
  
  output$warnest <- renderTable({
    dataEst()[['warndf']]
  })

  # Only enable download button once population estimates are produced
  observe({shinyjs::toggleState('dwnldcsv',length(dataEst()[['estOut']])!=0)})
  # Name output file based on type of analysis selected and write to comma-delimited file
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
      write.csv(dataEst()[['estOut']], file, row.names = FALSE)
    }
  )
  
  session$onSessionEnded(function() {
    rm(warn.df,envir=.GlobalEnv)
    stopApp()
  })  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

