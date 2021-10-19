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
ui <- fluidPage(theme="style.css",
   shinyjs::useShinyjs(),
   # Application title 
   navbarPage(title="NARS Population Estimate Calculation Tool (v. 2.0)",
              selected='instructions',position='static-top',
      # Panel with instructions for using this tool
      tabPanel(title='Instructions for Use',value='instructions',
               h2("Overview"),
               p('This Shiny app allows for calculation of population estimates as performed for the National 
                 Aquatic Resource Surveys (NARS). Estimates based on categorical and continuous variables are possible. 
                 This app does not include all possible options but does allow typical settings used by NARS for creating 
                 population estimates.'),
               h3("Instructions for Use"),
               h4("Prepare Data for Analysis"),
               tags$ol(
                 tags$li("Select data file and upload. If the data are to be loaded from a URL, check the
                         box to do so and paste or enter the URL for the file."),
                 tags$li("The variables in that file will populate dropdown lists on that tab."),
                 tags$li("Select variables to serve as site IDs, weights, response variables, and subpopulations
                         (if desired). If only overall or 'national' estimates are desired, check the box for overall analysis."),
                 tags$li("If data are to be used for change analysis, select year variable 
                         (or other variable to distinguish design cycles)."),
                tags$li(p("Select the type of variance you want to use. ", strong("Local neighborhood variance"), 
                          " uses a site's nearest neighbors to estimate variance, tending to result in smaller 
                          variance values. This approach is ", em("recommended"),"and is the approach used in 
                          NARS estimates. It requires site coordinates to be provided."),
                        tags$ul(
                        tags$li("For local neighborhood variance, select coordinate variables 
                                (Albers projection, or some other projection)."),
                        tags$li("For simple random sample (SRS) variance, selecting a stratum variable 
                                to better estimate variance is advised but not required. Coordinates are 
                                not used with type of variance."))),
                br(),
                tags$li("You may subset the data for analysis by up to one categorical variable. To do 
                        this, select the check box to subset, then select the variable to subset by.
                        Finally, select one or more categories by which to subset data."),
                tags$li("Click on the left hand button to view the full dataset if necessary."),
               tags$li("Click on the right hand button above the data to subset the data before 
                       proceeding to the Run Population Estimates tab.")
               ),
               br(),
               h4("Run Population Estimates"),
               tags$ol(
                 tags$li("Select the type of analysis (categorical or continuous)."),
                 tags$li("If year or design cycle variable was selected on the Prepare Data for Analysis tab, 
                         select year or cycle of interest."),
                 tags$li("For continuous analysis, select either CDFs (cumulative distribution 
                         functions) or Percentiles."),
                 tags$li("Click on the Run/Refresh Estimates button. Depending on the number of
                         responses, subpopulations, and type of analysis, it may take a few seconds 
                         to several minutes."),
                 tags$li("If desired, download results to a comma-delimited file by clicking
                         the Save Results button.")
               ),
               br(),
               h4("Run Change Analysis"),
               tags$ol(
                 tags$li("First select the two years (or sets of years) to compare."),
                 tags$li("Select type of data to analyze (categorical or continuous)."),
                 tags$li("If continuous data are selected, select parameter on which to test 
                         for differences (mean or median)."),
                 tags$li("If repeated visits to sites are included in dataset across years or 
                         cycles, check box. If selected, note that site ID variable selected 
                         must contain the same value for both years or cycles of data."),
                 tags$li("Click on the Run/Refresh Estimates button. Depending on the number of 
                 responses, subpopulations, and type of analysis, it may take a few 
                         seconds to several minutes."),
                 tags$li("If any data are changed in the Prepare Data for Analysis tab, years 
                         must be re-selected before running analysis.")
               ),
               
               br(),
               h4("Minimum requirements:"),
               tags$ul(
                         tags$li("All variables must be contained in one file and include site IDs, 
                         weights, response variables, subpopulations (if any), and optionally, 
                         coordinates and/or design stratum (depending on type of variance desired)." ),
                         tags$li("All sites included in the dataset should have weight > 0. Any 
                                 records with a missing weight or a weight of 0 will be dropped 
                                 before analysis."),
                         tags$li("Input data should include", 
                                 strong("only one visit per site and year/survey cycle"), 
                                 "(based on the variables for site ID and year/survey cycle selected)."),
                         tags$li("Only delimited files, such as comma- and tab-delimited, are accepted for upload."),
                         tags$li("If local neighborhood variance is desired, coordinates must be provided in some type of projection, such as Albers."),
                         tags$li("If variance based on a simple random sample is desired (or if coordinates are not available), the design stratum should be provided to better estimate variance."),
                         tags$li("If change analysis is intended, all desired years of data must be contained in one file.")
                       ),
               br(),
               p('Contact Karen Blocksom at blocksom.karen@epa.gov with questions or feedback.'),
               p('Last updated on October 19, 2021.'),
               h3('Disclaimer'),
               p('The United States Environmental Protection Agency (EPA) GitHub project code is provided 
                 on an "as is" basis and the user assumes responsibility for its use.  EPA has relinquished 
                 control of the information and no longer has responsibility to protect the integrity, 
                 confidentiality, or availability of the information.  Any reference to specific commercial 
                 products, processes, or services by service mark, trademark, manufacturer, or otherwise, 
                 does not constitute or imply their endorsement, recommendation or favoring by EPA.  The EPA 
                 seal and logo shall not be used in any manner to imply endorsement of any commercial product
                 or activity by EPA or the United States Government.')),

      # Panel to import and select data to analyze
      tabPanel(title='Prepare Data for Analysis',value="prepdata",
       
          fluidRow(
            # Input: Select a file ---
            column(3,
                add_busy_spinner(spin='fading-circle', position='full-page'),
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
                                 selectizeInput("subcat","Select one or more categories by which to subset data", choices=NULL,
                                                multiple=TRUE))),
            
            # Provide dropdown menus to allow user to select site, weight, and response variables from those in the imported dataset
            column(4,
              selectizeInput("siteVar","Select site variable", choices=NULL, multiple=FALSE),
              selectizeInput("weightVar","Select weight variable", choices=NULL, multiple=FALSE),
              selectizeInput("respVar","Select up to 10 response variables - must all be either categorical or numeric", 
                             choices=NULL, multiple=TRUE),
              checkboxInput('chboxYear', 'Check box if performing change analysis or need to subset data by year 
                            or cycle for population estimates'),
              conditionalPanel(condition="input.chboxYear == true",
                               selectizeInput("yearVar","Select year variable",
                             choices=NULL, multiple=FALSE)),
              
              checkboxInput('natpop','Calculate overall (all sites) estimates?', TRUE),
              checkboxInput('subpop', 'Calculate estimates for subpopulations?', FALSE),
              # If national estimates box is NOT checked, show subpopulation dropdown list              
              conditionalPanel(condition = 'input.subpop == true',
                               selectizeInput("subpopVar","Select up to 10 subpopulation variables", 
                                              choices=NULL, multiple=TRUE))
              
            ),
            # Set up type of variance to use in estimates: local or SRS
            column(4,
                   radioButtons('locvar',"Type of variance estimate to use (select one)",
                                choices = c('Local neighborhood variance (recommended, used for NARS, 
                                            requires site coordinates)' = 'local',
                                            'Simple Random Sample (requires stratum but not site 
                                            coordinates)' = 'SRS'),
                                select = 'local'),
                   # If local, user must select x and y coordinates and convert to Albers if in lat/long
                   conditionalPanel(condition = "input.locvar == 'local'",
                                    selectizeInput("coordxVar","Select the X coordinate variable (or longitude) 
                                                   (required only for local neighborhood variance)", 
                                                   choices=NULL, multiple=FALSE),
                                    selectizeInput("coordyVar","Select the Y coordinate variable (or latitude) 
                                                   (required only for local neighborhood variance)", 
                                                   choices=NULL, multiple=FALSE)
                                    
                                                      ),
                   
                   # Select stratum if Simple Random Sample                                 
                   selectizeInput("stratumVar","Select a categorical stratum variable if desired", 
                                  choices=NULL, multiple=TRUE)
            )  
         ),
          # Press button to subset data for analysis - must click here first
         column(4, actionButton("resetBtn", "Click to view full dataset")),
         column(4, actionButton("subsetBtn", "Click HERE to prepare data for analysis"), offset=2),
         hr(),
         br(),
       
          # Show a table of the data
          h4("Data for Analysis"),
          tableOutput("contents")
        
        
      ),
      # Tab to run population estimates
      tabPanel(title="Run Population Estimates",value="runest",
          fluidRow(     
             column(3, 
                    # add_busy_bar(color="red", centered=TRUE),
               # User must select categorical or continuous variable analysis, depending on response variables selected
               radioButtons("atype","Type of Analysis (pick one)",
                            choices = c('Categorical (for character variables)' = 'categ', 
                                        'Continuous (for numeric variables)' = 'contin'),
                            selected='categ'),
             # If continuous analysis selected, select whether CDFs or percentiles are desired.  
             conditionalPanel(condition = "input.atype == 'contin'",
                              radioButtons("cdf_pct", "Show CDF or percentile results",
                                           choices = c(CDF = 'cdf', Percentiles = 'pct', Mean = 'mean'),
                                           selected = 'pct')),
             conditionalPanel(condition="input.chboxYear==true",
                              selectizeInput('selYear', 'Select the year for analysis', 
                                             choices=NULL, multiple=FALSE)),
            
             p("If the", strong("Run/Refresh Estimates"), "button is grayed out, return to the", 
               strong("Prepare Data for Analysis"), "tab and click the button that says", 
               strong("Click HERE to prepare data for analysis")),
             p("Note that if all values are very small, the results may appear as zeroes. Save 
               and view output file to see the results will full digits."),
             # Once data are prepared, user needs to click to run estimates, or rerun estimates on refreshed data
             shinyjs::disabled(actionButton('runBtn', "Run/Refresh estimates")),
             hr(),
             # Click to download results into a comma-delimited file
             shinyjs::disabled(downloadButton("dwnldcsv","Save Results as .csv file"))),
             # Show results here
             column(8,
                    # conditionalPanel(condition = "input.chboxSize == true",
                    #                  h4(p(strong("*Note that estimates use size weights")), 
                    #                     style='color:blue')),
                    h4("Warnings"),
                    tableOutput("warnest"),
                    h4("Analysis Output"),
                    tableOutput("popest"))
          )

      ),
      tabPanel(title="Run Change Analysis", value='change',
               fluidRow(
                 h5(p("If a different set of response variables from those 
                 used in the population estimates is desired, 
                    return to the", strong("Prepare Data for Analysis"), "tab to re-select variables."), 
                    style="color:blue"),
                 column(3, 
                        # add_busy_spinner(spin='fading-circle', position='full-page'),
                        # User must select years to compare
                        
                        selectizeInput('chgYear1',"Select two years of data to compare in desired order", 
                                       choices=NULL, multiple=TRUE),
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
                 p("If the", strong("Run/Refresh Estimates"), "button is grayed out, return to the", 
                   strong("Prepare Data for Analysis"), "tab and click the button that says", 
                   strong("Click HERE to prepare data for analysis")),
                 shinyjs::disabled(actionButton('chgBtn', "Run/Refresh estimates")),
                 hr(),
                 # Click to download results into a comma-delimited file
                 shinyjs::disabled(downloadButton("chgcsv","Save Change Results as .csv file"))),
                 column(8,
                        # conditionalPanel(condition = "input.chboxSize == true",
                        #                  h4(p(strong("*Note that estimates use size weights")), 
                        #                     style='color:blue')),
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

    df
    
  })

  # Need code to show initial version of dataIn() before subsetting or pressing the button to reset
  
  observeEvent(input$subsetBtn, {
    shinyjs::disable('dwnldcsv')
    shinyjs::enable('chgBtn')
    shinyjs::enable('runBtn')
    shinyjs::disable('chgcsv')
    if(input$disp == 'head'){
      output$contents <- renderTable({head(dataOut())}, digits=5)
    }else{
      output$contents <- renderTable({dataOut()}, digits=5)
    }
  })
  
  observeEvent(input$resetBtn, {
    if(input$disp == 'head'){
      output$contents <- renderTable({head(dataIn())}, digits=5)
    }else{
      output$contents <- renderTable({dataIn()}, digits=5)
    }
  })
  
  
  # Use current dataset to refresh dropdown list of variables.
  observe({
    vars <- colnames(dataIn())
    updateSelectizeInput(session, "weightVar", "Select weight variable", choices=vars)
    
    updateSelectizeInput(session, 'respVar', 'Select up to 10 response variables - All must be either categorical or numeric', 
                         choices=vars, selected = NULL, 
                         options = list(maxItems=10))
    updateSelectizeInput(session, 'coordxVar', "Select the X coordinate variable (or longitude) \n(required only 
                         for local neighborhood variance)", 
                         choices=vars, selected = NULL)
    updateSelectizeInput(session, 'coordyVar', "Select the Y coordinate variable (or latitude) \n(required only 
                         for local neighborhood variance)", 
                         choices=vars, selected = NULL)
    updateSelectizeInput(session, 'siteVar', 'Select site variable', choices=vars)
    updateSelectizeInput(session, 'subpopVar', 'Select up to 10 subpopulation variables \n(required if not 
                         national estimates only)', choices=vars, selected=NULL,
                         options = list(maxItems=10))
    updateSelectizeInput(session, "stratumVar", "Select the stratum variable in order to calculate variance based 
                         on a simple random sample",
                         choices=c('None', vars), selected='None')
    updateSelectizeInput(session, "yearVar","Select year variable",
                         choices=c('', vars))
    
    updateSelectizeInput(session, "subvar", choices=vars)
    
    updateSelectizeInput(session, 'szwtVar', choices=vars)
  })
  
  observeEvent(input$subvar,{
    
      catchoices <- as.character(sort(unique(dataIn()[[input$subvar]])))
      
      updateSelectizeInput(session, 'subcat', choices = catchoices, selected=NULL)

  })
  

  # Once subset button is clicked, validate selections to make sure any variable only occurs in set of selections
  dataOut <- eventReactive(input$subsetBtn,{
    if(input$subsetBtn > 0){ 
      if(input$chboxYear==TRUE){
        yearVName <- input$yearVar
      }else{
        yearVName <- NULL
      }
      if(input$subcheck==TRUE){
        subVName <- input$subvar
      }else{
        subVName <- NULL
      }
      
      print(input$stratumVar)
      if(input$subpop == TRUE){
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

              
          # If local variance not used, need stratum variable but not coordinates 
          }else{
            # validate variable for stratum to make sure it does not overlap with other variables selected
            validate(
              need(input$stratumVar %nin% c(input$siteVar,input$respVar,input$subpopVar,
                                            input$weightVar,yearVName, subVName),
                   "Stratum variable cannot overlap with other variable selections.")
              )
            if(input$chboxYear==TRUE){
              validate(
                need(yearVName %nin% c(input$stratumVar),
                     "Year variable cannot overlap with other variable selections")
              ) 
            }
          }
        
        
        
          df1 <- dataIn()
 
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


        # if stratum selected, make sure stratum does not overlap with other variable selections
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

        }

      }
      df1 <- dataIn()
      # Drop any rows with weights missing or 0
      df1 <- subset(df1, !is.na(eval(as.name(input$weightVar))) & eval(as.name(input$weightVar))>0)
      if(input$subcheck==TRUE){
        df1 <- subset(df1, eval(as.name(input$subvar)) %in% input$subcat)
      }
      # Look for missing values among coordinates and weights only - response and subpopulation variables can have missing values
      # If local neighborhood variance not used, these values are null (not in df1) and thus return TRUE.
      validate(
        need(!any(is.na(df1[,input$coordxVar])), "Non-numeric or missing values for x coordinates."),
        need(!any(is.na(df1[,input$coordyVar])), "Non-numeric or missing values for y coordinates.")
      )
      df1
      
    # This is what shows up before the subset button is clicked  
    }else{
      df1 <- dataIn()
    }
  }

  )
 
  
  # Allow user to select specific year of interest for analysis
  observe({ 
                 ychoices <- as.character(sort(unique(dataOut()[[input$yearVar]])))
  
                 updateSelectizeInput(session, 'selYear', 'Select the year for analysis', 
                                      choices = ychoices, selected=NULL)
               
                
                 updateSelectizeInput(session, 'chgYear1', "Select two years or design cycles of data to compare", 
                                      choices = ychoices, selected=NULL, options = list(maxItems=2)) 
                 
                
                 }
               )
  
  
  # Change estimate code
  chgEst <- eventReactive(input$chgBtn,{
    
    if(exists("warn_df") && is.data.frame(get("warn_df"))){
      rm("warn_df", envir=.GlobalEnv)
      print('exists')
    }else{
      print("does not")
    }
    
      chgIn <- dataOut()
      
      chgIn <- subset(chgIn, eval(as.name(input$yearVar)) %in% input$chgYear1)
      
      # Check for duplicate rows for siteID
      freqSiteChg <- as.data.frame(table(siteID = chgIn[,input$siteVar],Year = chgIn[,input$yearVar]))

      validate(
        need(nrow(subset(freqSiteChg, Freq>1))==0, 
             paste("There are", nrow(subset(freqSiteChg, Freq>1)),
                   "duplicated sites in this dataset within years or cycles. 
                   Only one row per site-design cycle combination is permitted in the input data."))
      )
      
      # If categorical data, automatically reorder any response variables that are Good/Fair/Poor or 
      # Low/Moderate/High (allow for all caps versions)
      if(input$atype=='categ'){
        for(i in 1:length(input$respVar)){
          if(all(unique(chgIn[,input$respVar[[i]]]) %in% c('Good','GOOD','Low','LOW',
                                                          'Fair','FAIR','MODERATE','Moderate',
                                                          'Poor','POOR','High','HIGH',
                                                          'Very High','VERY HIGH','Not Assessed'))){
            chgIn[,input$respVar[[i]]] <- factor(chgIn[,input$respVar[[i]]], 
                                                levels=c('Good','GOOD','Low','LOW',
                                                         'Fair','FAIR','MODERATE','Moderate',
                                                         'Poor','POOR','High','HIGH',
                                                         'Very High','VERY HIGH','Not Assessed'), 
                                                ordered=TRUE)
          }
        }
      }
      
      # Need to order by siteID, yearVar
      chgIn <- chgIn[order(chgIn[,input$yearVar],chgIn[,input$siteVar]),]
      
      print(input$chgYear1[[1]])
      print(input$chgYear1[[2]])
      surveyID <- input$yearVar
      survey_names <- c(input$chgYear1[[1]], input$chgYear1[[2]])
      validate(
        need(length(survey_names)==2, 
             paste("Select years or design cycles to compare."))
      )

      if(input$natpop==TRUE & input$subpop==TRUE){
        all_sites <- TRUE
        # chgIn$All_Sites <- factor('All Sites')
      }else{
        all_sites <- FALSE
      }
      
      if(input$chgCatCont == 'chgCat'){
        vars_cat <- input$respVar
      }else{
        vars_cont <- input$respVar
      }
      
      if(input$locvar == 'local'){
        vartype <- 'Local'
      }else{
        vartype <- 'SRS'
      }
      
      if(input$subpop == FALSE){
        subpops.in <- NULL
      }else{
        subpops.in <- input$subpopVar
      }
      
      if(input$stratumVar=='None'){
        stratum.in <- NULL
      }else{
        stratum.in <- input$stratumVar
      }
      
      if(is.null(input$coordxVar)){
        xcoord.in <- NULL
      }else{
        xcoord.in <- input$coordxVar
      }
      
      if(is.null(input$coordyVar)){
        ycoord.in <- NULL
      }else{
        ycoord.in <- input$coordyVar
      }
      
      if(input$chgCatCont == 'chgCont'){
        if(input$testType == 'mean'){
          ttype <- 'mean'
        }else{
          ttype <- 'median'
        }
      }
      
       revisitWgt <- FALSE # NOT SURE WHAT THIS SHOULD BE SO ASSUME DEFAULT
       show_modal_spinner(spin = 'flower', text = 'This might take a while...please wait')
      
        # if(input$repeatBox==TRUE){
          if(input$chgCatCont == 'chgCat'){
            chgOut <- change_analysis(dframe = chgIn, vars_cat = input$respVar, 
                                      subpops=subpops.in, surveyID = surveyID,  
                                      survey_names = survey_names, revisitwgt = revisitWgt,
                                      siteID = input$siteVar, weight = input$weightVar, 
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      stratumID = stratum.in,
                                      vartype = vartype, All_Sites = all_sites)
          }else{
            chgOut <- change_analysis(dframe = chgIn, vars_cont = input$respVar, test = ttype, 
                                      subpops=subpops.in, surveyID = surveyID, 
                                      survey_names = survey_names, revisitwgt = revisitWgt,
                                      siteID = input$siteVar, weight = input$weightVar, 
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      stratumID = stratum.in,
                                      vartype = vartype, All_Sites = all_sites)
          }
 
      remove_modal_spinner()
      
    if(input$chgCatCont == 'chgCat'){
      chgOut.1 <- chgOut$catsum
    }else{
      if(input$testType == 'mean'){
        chgOut.1 <- chgOut$contsum_mean
      }else{
        chgOut.1 <- chgOut$contsum_median
      }
    }
    
    if(exists('warn_df') && ncol(warn_df)>1){
      outdf <- list(chgOut=chgOut.1, warndf=warn_df)
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
    
    if(exists("warn_df") && is.data.frame(get("warn_df"))){
      rm("warn_df", envir=.GlobalEnv)
      print('exists')
    }else{
      print("does not")
    }
    
    dfIn <- dataOut()
    
    if(input$natpop==TRUE & input$subpop==TRUE){
      all_sites <- TRUE
    }else{
      all_sites <- FALSE
    }
    
    if(input$chboxYear==TRUE){
      dfIn <- subset(dfIn, eval(as.name(input$yearVar)) == as.character(input$selYear))
    }
    
    # Check for duplicate rows for siteID
    freqSite <- as.data.frame(table(siteID=dfIn[,input$siteVar]))
    # print(nrow(subset(freqSite, Freq>1)>0))
    
    validate(
      need(nrow(subset(freqSite, Freq>1))==0, 
           paste("There are", nrow(subset(freqSite, Freq>1)),"duplicated sites in this dataset. 
                 Only one row per site permitted in the input data."))
    )

    show_modal_spinner(spin = 'flower', text = 'This might take a while...please wait')
    
    # If categorical data, automatically reorder any response variables that are Good/Fair/Poor or 
    # Low/Moderate/High (allow for all caps versions)
    if(input$atype=='categ'){
      for(i in 1:length(input$respVar)){
        if(all(unique(dfIn[,input$respVar[[i]]]) %in% c('Good','GOOD','Low','LOW',
                                                        'Fair','FAIR','MODERATE','Moderate',
                                                        'Poor','POOR','High','HIGH',
                                                        'Very High','VERY HIGH','Not Assessed'))){
          dfIn[,input$respVar[[i]]] <- factor(dfIn[,input$respVar[[i]]], 
                                        levels=c('Good','GOOD','Low','LOW',
                                                 'Fair','FAIR','MODERATE','Moderate',
                                                 'Poor','POOR','High','HIGH',
                                                 'Very High','VERY HIGH','Not Assessed'), 
                                              ordered=TRUE)
        }
      }
    }
    
      
      # Create varype variable depending on option selected
      if(input$locvar == 'local'){
        vartype <- 'Local'
      }else{
        vartype <- input$locvar
      }
      
      if(input$subpop == FALSE){
        subpops.in <- NULL
      }else{
        subpops.in <- input$subpopVar
      }
      
      if(input$stratumVar=='None'){
        stratum.in <- NULL
      }else{
        stratum.in <- input$stratumVar
      }
      
      if(is.null(input$coordxVar)){
        xcoord.in <- NULL
      }else{
        xcoord.in <- input$coordxVar
      }
      
      if(is.null(input$coordyVar)){
        ycoord.in <- NULL
      }else{
        ycoord.in <- input$coordyVar
      }
      
       # User selected categorical analysis, set up cat.analysis function depending on previous selections
      if(input$atype=='categ'){
            estOut <- cat_analysis(dframe = dfIn, siteID=input$siteVar, subpops=subpops.in, 
                                   vars=input$respVar, weight = input$weightVar, 
                                   xcoord = xcoord.in, ycoord = ycoord.in,
                                   # sizeweight = sizeweight.in, sweight = sweight.in,
                                   stratumID = stratum.in, vartype=vartype, 
                                   All_Sites = all_sites)
            
      }else{
            
            if(input$cdf_pct=='cdf'){ # Produce CDFs
              estOut <- cont_analysis(dframe = dfIn, siteID=input$siteVar, subpops=subpops.in, 
                                      vars=input$respVar, weight = input$weightVar, 
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      # sizeweight = sizeweight.in, sweight = sweight.in,
                                      stratumID = stratum.in, vartype=vartype, 
                                      All_Sites = all_sites, 
                                      statistics = 'CDF')$CDF
                            
            }else if(input$cdf_pct=='pct'){ # Just produce percentiles
              estOut <- cont_analysis(dframe = dfIn, siteID=input$siteVar, subpops=subpops.in, 
                                      vars=input$respVar, weight = input$weightVar, 
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      # sizeweight = sizeweight.in, sweight = sweight.in,
                                      stratumID = stratum.in, vartype=vartype, 
                                      All_Sites = all_sites,  
                                      statistics = c('Pct'))$Pct
            }else{
              estOut <- cont_analysis(dframe = dfIn, siteID=input$siteVar, subpops=subpops.in, 
                                      vars=input$respVar, weight = input$weightVar, 
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      # sizeweight = sizeweight.in, sweight = sweight.in,
                                      stratumID = stratum.in, vartype=vartype, 
                                      All_Sites = all_sites,  
                                      statistics = c('Mean'))$Mean
            }
          # }
      }
      
      remove_modal_spinner()

    if(exists('warn_df') && ncol(warn_df)>1){
      outdf <- list(estOut=estOut, warndf=warn_df)
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
    rm(warn_df,envir=.GlobalEnv)
    stopApp()
  })  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

