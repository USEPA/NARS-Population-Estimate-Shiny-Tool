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
               p('This Shiny app allows for calculation of population estimates as performed for the National 
                 Aquatic Resource Surveys (NARS). Estimates based on categorical and continuous variables are possible. 
                 This app does not include all possible options but does allow typical settings used by NARS for creating 
                 population estimates.'),
               h3("Instructions for Use"),
               h4("Prepare Data for Analysis tab"),
               tags$ol(
                 tags$li("Select data file and upload. If the data are to be loaded from a URL, check the box to do so and paste or enter the URL for the file."),
                 tags$li("The variables in that file will populate dropdown lists on that tab."),
                 tags$li("Select variables to serve as site IDs, weights, response variables, and subpopulations (if desired). If only overall or 'national' estimates are desired, check the box for overall analysis."),
                 tags$li("If data are to be used for change analysis, select year variable (or other variable to distinguish design cycles)."),
                tags$li("Select the type of variance you want to use.",
                        tags$ul(
                        tags$li("For simple random sample variance, select a stratum variable to better estimate variance."), 
                        tags$li("For local neighborhood variance (as used in NARS estimates), select coordinate variables (either in latitude/longitude or Albers projection). If coordinates are in latitude and longitude, you must provide projection information in order to convert them to Albers projection."))),
                tags$li("You may subset the data for analysis by up to one categorical variable. To do this, select the check box to subset, then select the variable to subset by. Finally, select one or more categories by which to subset data."),
               tags$li("Click on the button above the data to subset the data before proceeding to the Run Population Estimates tab")
               ),
               br(),
               h4("Run Population Estimates"),
               tags$ol(
                 tags$li("Select the type of analysis (categorical or continuous)."),
                 tags$li("If year or design cycle variable was select on data preparation tab, select year or cycle of interest."),
                 tags$li("For continuous analysis, select either CDFs (cumulative distribution functions) or Percentiles."),
                 tags$li("Click on the Run/Refresh Population Estimates button. Depending on the number of responses, subpopulations, and type of analysis, it may take a few minutes."),
                 tags$li("If desired, download results to a comma-delimited file by clicking the Save Results button.")
               ),
               br(),
               h4("Run Change Analysis"),
               tags$ol(
                 tags$li("First select the two years (or sets of years) to compare."),
                 tags$li("Select type of data to analyze (categorical or continuous."),
                 tags$li("If continuous data are selected, select parameter on which to test for differences (mean or median)."),
                 tags$li("If repeated visits to sites are included in dataset across years or cycles, check box. If selected, note that site ID variable selected must contain the same value for both years or cycles of data.")
               ),
               br(),
               h4("Minimum requirements:"),
               tags$ul(
                         tags$li("All variables must be contained in one file and include site IDs, weights, response variables,
                                 subpopulations (if any), and coordinates or design stratum (depending on type of variance desired)." ),
                         tags$li("All sites included in the dataset should have weight > 0. Any records with a missing weight or a weight of 0 will be dropped before analysis."),
                         tags$li("Input data should include only one visit per site and year/survey cycle (based on the variables for site ID and year/survey cycle selected)."),
                         tags$li("Only delimited files, such as comma- and tab-delimited, are accepted for upload."),
                         tags$li("If local neighborhood variance is desired, coordinates must be provided, either in 
                                 latitude/longitude (decimal degrees) or Albers projection. If provided in latitude/longitude, projection information is needed to convert values. Only spheroid need be supplied for conversion."),
                         tags$li("If variance based on a simple random sample is desired (or if coordinates or projection 
                                 information are not available), the design stratum is needed to better estimate variance."),
                         tags$li("If change analysis is intended, all desired years of data must be contained in one file.")
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
                checkboxInput("websource", 'Input file from URL instead of local directory', FALSE),
                # Read in file from local computer
                conditionalPanel(condition="input.websource == false",
                  fileInput(inputId='file1', buttonLabel='Browse...', 
                      label='Select a delimited file for analysis',
                      multiple=FALSE, accept=c('text/csv','text/comma-separated-values,text/plain','.csv')
                      )),
                # Read in file from website URL - need to add a button to signal it should start uploading
                conditionalPanel(condition="input.websource == true",
                                 textInput("urlfile", "Paste or enter full URL here."),
                                 actionButton("urlbtn", "Load file from URL")),
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
                             selected='head'),
                checkboxInput("subcheck","Subset data using a single categorical variable", FALSE),
                conditionalPanel(condition="input.subcheck == true",
                                 selectizeInput('subvar', "Select variable to use for subsetting", choices=NULL, multiple=FALSE),
                                  selectizeInput("subcat","Select one or more categories by which to subset data", choices=NULL, multiple=TRUE))),
            
            # Provide dropdown menus to allow user to select site, weight, and response variables from those in the imported dataset
            column(4,
              selectizeInput("siteVar","Select site variable", choices=NULL, multiple=FALSE),
              selectizeInput("weightVar","Select weight variable", choices=NULL, multiple=FALSE),
              selectizeInput("respVar","Select up to 10 response variables - must all be either categorical or numeric", 
                             choices=NULL, multiple=TRUE),
              checkboxInput('chboxYear', 'Check box if performing change analysis or need to subset data by year or cycle for population estimates'),
              conditionalPanel(condition="input.chboxYear == true",
                               selectizeInput("yearVar","Select year variable",
                             choices=NULL, multiple=FALSE)),
              
              # If national estimates box is NOT checked, show subpopulation dropdown list              
              conditionalPanel(condition = 'input.natpop == false',
                               selectizeInput("subpopVar","Select up to 10 subpopulation variables (required if not 
                                              national estimates only)", 
                                              choices=NULL, multiple=TRUE)),
              checkboxInput('natpop','Only overall (all sites) estimates? Select if no subpopulations of interest',FALSE)
            ),
            # Set up type of variance to use in estimates: local or SRS
            column(4,
                   radioButtons('locvar',"Type of variance estimate to use (select one)",
                                choices = c('Local neighborhood variance' = 'local',
                                            'Simple Random Sample (requires stratum)' = 'srs'),
                                select = 'srs'),
                   # If local, user must select x and y coordinates and convert to Albers if in lat/long
                   conditionalPanel(condition = "input.locvar == 'local'",
                                    selectizeInput("coordxVar","Select the X coordinate variable (or longitude) 
                                                   (required only for local neighborhood variance)", 
                                                   choices=NULL, multiple=FALSE),
                                    selectizeInput("coordyVar","Select the Y coordinate variable (or latitude) 
                                                   (required only for local neighborhood variance)", 
                                                   choices=NULL, multiple=FALSE),
                                    
                                    checkboxInput("xy", "Convert latitude/longitude \nto Albers Projection (This is necessary 
                                                  if using local neighborhood variance.). Current projection information:",
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
                            choices = c('Categorical (for character variables)' = 'categ', 'Continuous (for numeric variables)' = 'contin'),
                            selected='categ'),
             # If continuous analysis selected, select whether CDFs or percentiles are desired.  
             conditionalPanel(condition = "input.atype == 'contin'",
                              radioButtons("cdf_pct", "Show CDF or percentile results",
                                           choices = c(CDF = 'cdf', Percentiles = 'pct'),
                                           selected = 'pct')),
             conditionalPanel(condition="input.chboxYear==true",
                              selectizeInput('selYear', 'Select the year for analysis', choices=NULL, multiple=FALSE)),
            
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

      ),
      tabPanel(title="Run Change Analysis", value='change',
               fluidRow(
                 h4("If a different set of response variables is necessary from those 
                 used in the population estimates, 
                    return to the Prepare Data for Analysis tab to re-select variables."),
                 column(3, 
                        # User must select years to compare
                        selectizeInput('chgYear1',"Select two years of data to compare in desired order", choices=NULL, multiple=TRUE),
                        radioButtons("chgCatCont", "Type of variables to analyze",
                                     choices = c(Categorical = 'chgCat', Continuous = 'chgCont'),
                                     select = 'chgCat'),
                        conditionalPanel(condition = "input.chgCatCont == 'chgCont'",
                                         radioButtons("testType", "Base test on mean or median",
                                                      choices = c(Mean='mean', Median='median'),
                                                      selected = 'mean')),
                        
                 # Once data are prepared, user needs to click to run estimates, or rerun estimates on refreshed data
                 checkboxInput("repeatBox", "Are there repeat visits for any sites? If so, 
                               be sure the selected Site ID variable is the same for both 
                               visits to a site.",
                               value=FALSE),
                 hr(),
                 actionButton('chgBtn', "Run/Refresh change estimates"),
                 hr(),
                 # Click to download results into a comma-delimited file
                 downloadButton("chgcsv","Save Change Results as .csv file")),
                 column(8,
                        h4("Warnings"),
                        tableOutput("warnchg"),
                        h4("Change Analysis Output"),
                        tableOutput("changes"))
               )
               
          )
     )
   )
# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  # Read in data file as selected
  dataIn <- reactive({
    if(input$websource==FALSE){
      file1 <- input$file1
      req(file1)
      df <- read.table(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     stringsAsFactors=F)
    }else{
      if(input$urlbtn>0){
        df <- read.table(url(input$urlfile),
                         header = input$header,
                         sep = input$sep,
                         stringsAsFactors=F)
      }
    }
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
    updateSelectizeInput(session, "yearVar","Select year variable",
                         choices=c('', vars))
#    if(input$subcheck==TRUE){
      updateSelectizeInput(session, "subvar", choices=vars)
#    }
    
  })
    

  # Once subset button is clicked, validate selections to make sure any variable only occurs in set of selections
  dataOut <- eventReactive(input$subsetBtn,{
    if(input$subsetBtn > 0){ 
      if(input$chboxYear==TRUE){
        yearVName <- input$yearVar
      }else{
        yearVName <- NULL
      }
      
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
          if(input$chboxYear==TRUE){
             validate(
               need(yearVName %nin% c(input$respVar,input$subpopVar,input$weightVar,input$siteVar),
                    "Year variable cannot overlap with other variable selections")
             )
          }
          # If local variance selected, make sure x and y coordinate variables do not overlap with any already selected     
          if(input$locvar == 'local'){
            validate(
              need(input$coordxVar %nin% c(input$siteVar,input$coordyVar,input$respVar,input$subpopVar,input$weightVar),
                 "X-coordinate variable cannot overlap with other variable selections."),
              need(input$coordyVar %nin% c(input$siteVar,input$coordxVar,input$respVar,input$subpopVar,input$weightVar),
                 "Y-coordinate variable cannot overlap with other variable selections.")
              )
            if(input$chboxYear==TRUE){
              validate(
                need(input$yearVar %nin% c(input$coordxVar, input$coordyVar),
                     "Year variable cannot overlap with other variable selections")
              ) 
            }
              # For conversion to Albers projection, select necessary variables and use geodalbers function from spsurvey
              df1 <- subset(dataIn(), select=c(input$siteVar, input$coordxVar, input$coordyVar, input$weightVar, input$respVar, 
                                               input$subpopVar, yearVName))
              if(input$xy == TRUE){
                xyCoord <- geodalbers(dataIn()[,input$coordxVar],dataIn()[,input$coordyVar],input$sph,
                                      as.numeric(input$clon),as.numeric(input$clat),
                                      as.numeric(input$sp1),as.numeric(input$sp2))
                # Combine x and y coordinates back with set of selected variables, select
                df1 <- cbind(df1, xyCoord)
                df1 <- subset(df1, select=c(input$siteVar,'xcoord','ycoord',input$weightVar,input$respVar,input$subpopVar,yearVName))
               # If conversion not needed, select variables and rename them to required names 
              }else{
                df1 <- subset(df1, select=c(input$siteVar,input$coordxVar,input$coordyVar,input$weightVar,input$respVar,
                                            input$subpopVar, yearVName))
                df1$xcoord <- with(df1, eval(as.name(input$coordxVar)))
                df1$ycoord <- with(df1, eval(as.name(input$coordyVar)))

              } 
              df1$siteID <- with(df1, eval(as.name(input$siteVar)))
              df1$wgt <- with(df1, eval(as.name(input$weightVar)))
              df1 <- subset(df1, select = c('siteID','wgt','xcoord','ycoord',input$respVar,input$subpopVar, yearVName))
              
              
          # If local variance not used (SRS selected), need stratum variable but not coordinates 
          }else{
            # validate variable for stratum to make sure it does not overlap with other variables selected
            validate(
              need(input$stratumVar %nin% c(input$siteVar,input$respVar,input$subpopVar,input$weightVar,yearVName),
                   "Stratum variable cannot overlap with other variable selections.")
              )
            if(input$chboxYear==TRUE){
              validate(
                need(yearVName %nin% c(input$stratumVar),
                     "Year variable cannot overlap with other variable selections")
              ) 
            }
            # Subset the data to the variables selected, then rename any to required names.
            df1 <- subset(dataIn(), select=c(input$siteVar, input$stratumVar, input$weightVar, input$respVar, 
                                             input$subpopVar, yearVName)) 
            
            df1$stratum <- with(df1, eval(as.name(input$stratumVar)))
            df1$siteID <- with(df1, eval(as.name(input$siteVar)))
            df1$wgt <- with(df1, eval(as.name(input$weightVar)))
            df1 <- subset(df1, select = c('siteID','wgt','stratum',input$respVar,input$subpopVar, yearVName))

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
        if(input$chboxYear==TRUE){
          validate(
            need(input$yearVar %nin% c(input$siteVar, input$weightVar, input$respVar),
                 "Year variable cannot overlap with other variable selections")
          ) 
        }
        # if local neighborhood variance used here, make sure coordinates variables do not overlap with other selections
        if(input$locvar == 'local'){
          validate(
            need(input$coordxVar %nin% c(input$siteVar,input$coordyVar,input$respVar,input$weightVar),
                 "X-coordinate variable cannot overlap with other variable selections."),
            need(input$coordyVar %nin% c(input$siteVar,input$coordxVar,input$respVar,input$weightVar),
                 "Y-coordinate variable cannot overlap with other variable selections.")
          )
          if(input$chboxYear==TRUE){
            validate(
              need(input$yearVar %nin% c(input$coordxVar, input$coordyVar),
                   "Year variable cannot overlap with other variable selections")
            )
          }
          
          # Subset the data to selected variables if year selected
          df1 <- subset(dataIn(), select=c(input$siteVar, input$coordxVar, input$coordyVar, input$weightVar, 
                                           input$respVar, yearVName))
          
          # If conversion from lat/long is necessary, convert, then rename variables with required names
          if(input$xy == TRUE){
            xyCoord <- geodalbers(dataIn()[,input$coordxVar],dataIn()[,input$coordyVar],input$sph,
                                  as.numeric(input$clon),as.numeric(input$clat),
                                  as.numeric(input$sp1),as.numeric(input$sp2))
            
            df1 <- cbind(df1, xyCoord)

          }else{ # Conversion is not necessary, so just rename and subset variables
            
            df1$xcoord <- with(df1, as.numeric(eval(as.name(input$coordxVar))))
            df1$ycoord <- with(df1, as.numeric(eval(as.name(input$coordyVar))))
          } 
          df1$siteID <- with(df1, eval(as.name(input$siteVar)))
          df1$wgt <- with(df1, as.numeric(eval(as.name(input$weightVar))))
          df1 <- subset(df1, select = c('siteID','wgt','xcoord','ycoord',input$respVar,yearVName))
          
        # if SRS selected, make sure stratum does not overlap with other variable selections
        }else{
          validate(
            need(input$stratumVar %nin% c(input$siteVar,input$respVar,input$weightVar),
                 "Stratum variable cannot overlap with other variable selections.")
          )
          if(input$chboxYear==TRUE){
            validate(
              need(input$yearVar %nin% c(input$stratumVar),
                   "Year variable cannot overlap with other variable selections")
            ) 
          }
           # subset to the variables needed
          df1 <- subset(dataIn(), select = c(input$siteVar, input$weightVar, input$stratumVar, input$respVar, yearVName, input$subvar))
          
          df1$siteID <- with(df1, eval(as.name(input$siteVar)))
          df1$wgt <- with(df1, as.numeric(eval(as.name(input$weightVar))))
          df1$stratum <- with(df1, eval(as.name(input$stratumVar)))
          df1 <- subset(df1, select = c('siteID','wgt','stratum',input$respVar,yearVName,input$subvar))
        }

      }
      # Drop any rows with weights missing or 0
      df1 <- subset(df1, !is.na(wgt) & wgt>0)
      if(input$subcheck==TRUE){
        df1 <- subset(df1, eval(as.name(input$subvar)) %in% input$subcat)
      }
      # Look for missing values among coordinates and weights only - response and subpopulation variables can have missing values
      # If local neighborhood variance not used, these values are null (not in df1) and thus return TRUE.
      validate(
        need(!any(is.na(df1$xcoord)), "Non-numeric or missing values for x coordinates."),
        need(!any(is.na(df1$ycoord)), "Non-numeric or missing values for y coordinates.")
 #       need(!any(is.na(df1$wgt)), "Non-numeric or missing values for weights.")
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
  
  # Allow user to select specific year of interest for analysis
  observe({ 
                 ychoices <- as.character(sort(unique(dataOut()[[input$yearVar]])))
  
                 updateSelectizeInput(session, 'selYear', 'Select the year for analysis', 
                                      choices = ychoices, selected=NULL)
                
                 updateSelectizeInput(session, 'chgYear1', "Select two years of data to compare", 
                                      choices = ychoices, selected=NULL, options = list(maxItems=2)) 
                
                 }
               )
  
  observe({
      catchoices <- as.character(sort(unique(dataIn()[[input$subvar]])))
      
      updateSelectizeInput(session, 'subcat', choices = catchoices, selected=NULL)
    }
  )
  
  # Change estimate code
  chgEst <- eventReactive(input$chgBtn,{
    if(exists("warn.df") && is.data.frame(get("warn.df"))){
      rm("warn.df", envir=.GlobalEnv)
      print('exists')
    }else{
      print("does not")
    }
    
      chgIn <- dataOut()
      
      chgIn <- subset(chgIn, eval(as.name(input$yearVar)) %in% input$chgYear1)
      
      # Check for duplicate rows for siteID
      freqSiteChg <- as.data.frame(table(siteID = chgIn$siteID,Year = chgIn[,input$yearVar]))
      # print(nrow(subset(freqSiteChg, Freq>1)>0))
      
      validate(
        need(nrow(subset(freqSiteChg, Freq>1))==0, paste("There are", nrow(subset(freqSiteChg, Freq>1)), 
                                                         "duplicated sites in this dataset within years or cycles."))
      )
      
      
    withProgress(message="Calculating estimates",detail="This might take a while...",
                   value=0,{
      # Need to order by siteID, yearVar
      chgIn <- chgIn[order(chgIn[,input$yearVar],chgIn$siteID),]
      
      print(input$chgYear1[[1]])
      print(input$chgYear1[[2]])
      
      chgIn$Survey1 <- ifelse(chgIn[,input$yearVar]==input$chgYear1[[1]], TRUE, FALSE)
      chgIn$Survey2 <- ifelse(chgIn[,input$yearVar]==input$chgYear1[[2]], TRUE, FALSE)

      # Need to identify sites (based on siteID) that are repeats
      if(input$repeatBox==TRUE){
        repeatSites <- as.data.frame(table(siteID=chgIn$siteID))
        repeatSites <- subset(repeatSites, Freq==2)
        repeatSites$siteID_1 <- repeatSites$siteID
        repeatSites$siteID_2 <- repeatSites$siteID
        print("Repeat sites")

        repeatSites <- repeatSites[,c('siteID_1','siteID_2')]
      }
      
      sites <- chgIn[,c('siteID','Survey1','Survey2')]
      
      if(input$natpop==FALSE){
        subpop <- chgIn[,c('siteID','allSites',input$subpopVar)]
      }else{
        chgIn$allSites <- 'All Sites'
        subpop <- chgIn[, c('siteID','allSites')]
      }
      # if local neighborhood variance
      if(input$locvar=='local'){
        design <- chgIn[, c('siteID', 'wgt', 'xcoord', 'ycoord')]
        
      }else{ # if simple random sample
        design <- chgIn[, c('siteID', 'wgt', 'stratum')]
      }
      
      in.data <- chgIn[, c('siteID', input$respVar)]
      
      switch(input$locvar,
             local = {
               local_1 <- 'local'
               local_2 <- 'local'
               },
             srs = {
               local_1 <- 'SRS'
               local_2 <- 'SRS'
             })
      
      if(input$chgCatCont == 'chgCont'){
        if(input$testType == 'mean'){
          ttype <- 'mean'
        }else{
          ttype <- 'median'
        }
      }
      
      #if(input$natpop=='FALSE'){
        if(input$repeatBox==TRUE){
          if(input$chgCatCont == 'chgCat'){
            chgOut <- change.analysis(sites=sites, repeats=repeatSites, subpop=subpop, design=design,
                                    data.cat=in.data, vartype_1=local_1, vartype_2=local_2)
          }else{
            chgOut <- change.analysis(sites=sites, repeats=repeatSites, subpop=subpop, design=design,
                                      data.cont=in.data, vartype_1=local_1, vartype_2=local_2, test=ttype)
          }
        }else{
          if(input$chgCatCont=='chgCat'){
            chgOut <- change.analysis(sites=sites, subpop=subpop, design=design, repeats=NULL,
                                      data.cat=in.data, vartype_1=local_1, vartype_2=local_2)
          }else{
            chgOut <- change.analysis(sites=sites, subpop=subpop, design=design, repeats=NULL,
                                      data.cont=in.data, vartype_1=local_1, vartype_2=local_2, test=ttype)
          }
          
        }
    }) 
    if(input$chgCatCont == 'chgCat'){
      chgOut.1 <- chgOut$catsum
    }else{
      if(input$testType == 'mean'){
        chgOut.1 <- chgOut$contsum_mean
      }else{
        chgOut.1 <- chgOut$contsum_median
      }
    }
    
    if(exists('warn.df') && ncol(warn.df)>1){
      outdf <- list(chgOut=chgOut.1, warndf=warn.df)
    }else{
      outdf <- list(chgOut=chgOut.1, warndf=data.frame(warnings='none'))
    }
    
  })
  # Use change output to create a table
  output$changes <- renderTable({
    chgEst()[['chgOut']]
  })
  
  output$warnchg <- renderTable({
    chgEst()[['warndf']]
  })
  
  
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
    if(input$chboxYear==TRUE){
      dfIn <- subset(dfIn, eval(as.name(input$yearVar)) == as.character(input$selYear))
    }
    
    # Check for duplicate rows for siteID
    freqSite <- as.data.frame(table(siteID=dfIn$siteID))
    # print(nrow(subset(freqSite, Freq>1)>0))
    
    validate(
      need(nrow(subset(freqSite, Freq>1))==0, 
           paste("There are", nrow(subset(freqSite, Freq>1)),"duplicated sites in this dataset."))
    )

    # If categorical data, automatically reorder any response variables that are Good/Fair/Poor or 
    # Low/Moderate/High (allow for all caps versions)
    if(input$atype=='categ'){
      for(i in 1:length(input$respVar)){
        if(all(unique(dfIn[,input$respVar[[i]]]) %in% c('Good','GOOD','Low','LOW','Fair','FAIR','MODERATE','Moderate',
                                                        'Poor','POOR','High','HIGH','Very High','VERY HIGH','Not Assessed'))){
          dfIn[,input$respVar[[i]]] <- factor(dfIn[,input$respVar[[i]]], 
                                              levels=c('Good','GOOD','Low','LOW','Fair','FAIR','MODERATE','Moderate',
                                                    'Poor','POOR','High','HIGH','Very High','VERY HIGH','Not Assessed'), 
                                              ordered=TRUE)
        }
      }
    }
    
    # Show progress bar for a certain about of time while calculations are running  
    withProgress(message="Calculating estimates",detail="This might take a while...",
                   value=0,{  
      # Create sites data frame, which is the same regardless of other options               
      sites <- subset(dfIn, select=c('siteID','Active'))
      
      # Create subpop data frame if subpopulations are of interest (National only box not checked)
      if(input$natpop == FALSE){
        subpop=subset(dfIn, select = c('siteID','allSites',input$subpopVar))
      }
      # Create design data frame depending on options selected
      if(input$locvar == 'local'){
        design=subset(dfIn,select=c('siteID','wgt','xcoord','ycoord'))
      }else{
        design=subset(dfIn,select=c('siteID','wgt','stratum'))
      }
      
      # Create data.cat if categorical and data.cont if continuous data
      if(input$atype=='categ'){
        data.cat=subset(dfIn,select=c('siteID',input$respVar))
      }else{
        # Make sure continuous variables are numeric
        if(length(input$respVar)>1){ # if more than one response variable selected
          dfIn[,input$respVar] <- lapply(dfIn[,input$respVar], as.numeric)
        }else{ # If only one response variable selected
          dfIn[,input$respVar] <- as.numeric(dfIn[,input$respVar])
        }
        
        # Select correct variables for input data
        data.cont=subset(dfIn,select=c('siteID',input$respVar))
      }
      

      # Create varype variable depending on option selected
      if(input$locvar == 'local'){
        vartype <- 'Local'
      }else{
        vartype <- 'SRS'
      }
      
      # User selected categorical analysis, set up cat.analysis function depending on previous selections
      if(input$atype=='categ'){
          if(input$natpop == FALSE){ # Not overall analysis,  includes subpopulations
            estOut <- cat.analysis(sites=sites, subpop=subpop, design = design,
                                   data.cat=data.cat, vartype=vartype)
            
          }else{ # No subpopulations selected
            estOut <- cat.analysis(sites=sites, design=design, data.cat=data.cat, vartype=vartype)
          }
      }else{
          if(input$natpop == FALSE){ # Subpopulations selected
            print(input$cdf_pct)
            if(input$cdf_pct=='cdf'){ # Produce CDFs
              estOut <- cont.analysis(sites=sites, subpop=subpop, design=design, 
                                      data.cont=data.cont, vartype=vartype)$CDF
                            
            }else{ # Just produce percentiles
              estOut <- cont.analysis(sites=sites, subpop=subpop, design=design, 
                                      data.cont=data.cont, vartype=vartype)$Pct
            }
          }else{ # No subpopulations selected, only overall analysis
            if(input$cdf_pct=='cdf'){ # CDFs of interest
              estOut <- cont.analysis(sites=sites, design=design, data.cont=data.cont,
                                      vartype=vartype)$CDF
             }else{ # Percentiles of interest
              estOut <- cont.analysis(sites=sites, design=design, data.cont=data.cont,
                                      vartype=vartype)$Pct
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
  
  # Only enable download button once population estimates are produced
  observe({shinyjs::toggleState('chgcsv',length(chgEst()[['chgOut']])!=0)})
  # Name output file based on type of analysis selected and write to comma-delimited file
  output$chgcsv <- downloadHandler(
    filename = function() {
      if(input$chgCatCont == 'chgCat'){
        paste("Change_Categ_Est_Output_",Sys.Date(), ".csv", sep = "")
      }else{
        if(input$testType == 'mean'){
          paste("Change_Contin_Mean_Est_Output_",Sys.Date(), ".csv", sep = "")
        }else{
          paste("Change_Contin_Median_Est_Output_",Sys.Date(), ".csv", sep = "")
        }
      }
    },
    content = function(file) {
      write.csv(chgEst()[['chgOut']], file, row.names = FALSE)
    }
  )
  
  
  session$onSessionEnded(function() {
    rm(warn.df,envir=.GlobalEnv)
    stopApp()
  })  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

