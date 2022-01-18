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
               p('This Shiny app allows for calculation of population estimates as performed for 
               the National Aquatic Resource Surveys (NARS) and the plotting of results. 
               Estimates based on categorical and continuous variables are possible. 
                 This app does not include all possible options but does allow typical settings 
                 used by NARS for creating population estimates.'),
               h3("Instructions for Use"),
               bsCollapsePanel(title = h4(strong("Prepare Data for Analysis")),
               tags$ol(
                 tags$li("Select data file and upload. If the data are to be loaded from a URL, check the
                         box to do so and paste or enter the URL for the file."),
                 tags$li("The variables in that file will populate dropdown lists on that tab."),
                 tags$li("Select variables to serve as site IDs, weights, response variables, and subpopulations
                         (if desired). If only overall or 'national' estimates are desired, check the box for overall analysis."),
                 tags$li("If data are to be used for change analysis, select year variable 
                         (or other variable to distinguish design cycles)."),
                tags$li(p("Select the type of variance you want to use. ", 
                          strong("Local neighborhood variance"), 
                          " uses a site's nearest neighbors to estimate variance, tending to 
                          result in smaller variance values. This approach is ", 
                          em("recommended"),"and is the approach used in 
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
               )),
               bsCollapsePanel(title = h4(strong("Run Population Estimates")), 
               tags$ol(
                 tags$li("Select the type of analysis (categorical or continuous)."),
                 tags$li("If year or design cycle variable was selected on the Prepare Data for 
                 Analysis tab, select year or cycle of interest."),
                 tags$li("For continuous analysis, select either CDFs (cumulative distribution 
                         functions) or Percentiles."),
                 tags$li("Click on the Run/Refresh Estimates button. Depending on the number of
                         responses, subpopulations, and type of analysis, it may take a few seconds 
                         to several minutes."),
                 tags$li("If desired, download results to a comma-delimited file by clicking
                         the Save Results button.")
               )),
               bsCollapsePanel(title = h4(strong("Run Change Analysis")),
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
               )),
               bsCollapsePanel(title = h4(strong("Minimum requirements for Analysis")),
               tags$ul(
                        tags$li("The R package", strong("spsurvey, v.5.0 or later"), 
                        "is required. 
                                Be sure to update this package if an older version is 
                                already installed." ),
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
                         tags$li("If local neighborhood variance is desired, coordinates must be 
                                 provided in some type of projection, such as Albers."),
                         tags$li("If variance based on a simple random sample is desired (or if 
                                 coordinates are not available), the design stratum should be provided 
                                 to better estimate variance."),
                         tags$li("If change analysis is intended, all desired years of data must be 
                                 contained in one file.")
                       )),
               bsCollapsePanel(title = h4(strong("Plot Categorical Estimates")),
                  tags$ol(
                    tags$li("Either run population estimates on categorical data either within the 
                            app or import results into the app."),
                    tags$li("Variables in dataset must match those expected as output from 
                            spsurvey::cat_analysis function:", strong("Type, Subpopulation, Indicator, 
                            Category, Estimate.P, StdError.P, LCB95Pct.P, UCB95Pct.P, Estimate.U,	
                            StdError.U,	LCB95Pct.U,	UCB95Pct.U")),
                    tags$li("Select either proportion or unit estimates to plot from Estimate Type."),
                    tags$li("Select Category values that represent Good, Fair, Poor, Not Assessed, 
                            and Other condition classes. More than one value per condition class 
                            in the dataset may be selected. For example, if one response uses 
                            Good/Fair/Poor and another used At or Below Benchmark/Above Benchmark, 
                            both Good and At or Below Benchmark can be selected."),
                    tags$li(strong("Optional:"), "add plot title and define resource type/unit 
                            (i.e., stream length, number of lakes, coastal or wetland area)"),
                    tags$li("Click the Plot/Refresh Button to create plots."),
                    tags$li("From the menus on the right-hand side of the page, select the 
                            Indicator of interest, and then the Subpopulation Group. Then 
                            select the Subpopulation of interest. The upper plot shows the 
                            individual subpopulation and the lower plot show a particular 
                            condition class across all subpopulations."),
                    tags$li("To show confidence bound values, click the box above the main 
                            and/or subpopulation plots."),
                    tags$li("The default order of the subpopulations in the lower plot is 
                            alphabetical, but to sort by the estimate of the", em("Good"), 
                            "class, 
                            click the box for", em("Sort Subpopulations by Good Condition"),"."),
                    tags$li("Select", em("Download Estimate Plot"), "or", 
                    em("Download Subpopulation Plot"), "to 
                            save a .png file of the output. ")
                    )),
               bsCollapsePanel(title = h4(strong("Plot Continuous Estimates")),
                   tags$ol(
                     tags$li("Either run population estimates on continuous data to obtain 
                             CDF estimates within the app, or import results into the app."),
                     tags$li("Variables in dataset must match those expected as output from 
                spsurvey::cont_analysis function:", strong("Type, Subpopulation, Indicator, 
                Value, Estimate.P, StdError.P, LCB95Pct.P, UCB95Pct.P, Estimate.U,	
                StdError.U,	LCB95Pct.U,	UCB95Pct.U")),
                     tags$li("Select either proportion or unit estimates to plot 
                             from Estimate Type."),
                     tags$li(strong("Optional:"), "Add plot title, indicator units, 
                             and define resource type/unit."),
                     tags$li("Click the Plot Continuous Estimates button."),
                     tags$li("Select indicator from dropdown, then select a subpopulation group. Add 
                             or remove subpopulations to the plot from the",
                             em("Add/Remove Subpopulations"),  
                             "dropdown."),
                     tags$li(strong("Optional:"), "Add an Indicator Threshold, add confidence bands, and/or 
                             change the x-axis to log10 scale. Be aware that if you have values 
                             that are below or equal to zero, points will be excluded from the 
                             plot if the", em("Log Scale X-Axis"), "option is selected.")
                     )),
               br(),
               p('Contact Karen Blocksom at blocksom.karen@epa.gov with questions or feedback.'),
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
              shiny::selectizeInput("siteVar","Select site variable", choices=NULL, multiple=FALSE) %>% 
                helper(type = "inline", icon = 'exclamation', colour='red',
                       title = "Site variable selection",
                       content = paste("Select a site variable. If multiple years and resampled 
                                       sites are included in the dataset, be sure the site 
                                       variable has the same value across years."),
                       size = "s", easyClose = TRUE, fade = TRUE),
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
                                  choices=NULL, multiple=FALSE)
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
               and view output file to see the results with full digits."),
             # Once data are prepared, user needs to click to run estimates, or rerun estimates on refreshed data
             shinyjs::disabled(actionButton('runBtn', "Run/Refresh estimates")),
             hr(),
             # Click to download results into a comma-delimited file
             shinyjs::disabled(downloadButton("dwnldcsv","Save Results as .csv file"))),
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
               
          ),
      ####Categorical Plot UI####
      tabPanel(title="Plot Categorical Estimates",
               fluidRow(
                 sidebarPanel(
                   radioButtons(inputId="catinput", 
                                label=strong("Choose Categorical Estimate Dataset to Use:"), 
                                choices=c("Upload Estimate Data File", "Current Estimate Data"), 
                                selected = "Upload Estimate Data File",
                                inline=FALSE),
                   uiOutput("catui"),
                   # div(id = "userinput1",
                   #     fileInput(
                   #       inputId = "userinput",
                   #       label = strong("Choose Categorical Estimate File"),
                   #       placeholder = "Must be a .csv file",
                   #       accept = c(".csv"))) %>%
                   #   #File input helper
                   #   helper(type = "inline",
                   #          title = "Categorical Estimate File",
                   #          content = paste("Choose a file with the same output which the 
                   #                   spsurvey package", strong("cat_analysis()"), 
                   #                          "function renders. If the 
                   #                               dataset is missing required variables, no 
                   #                               selections will show up in the dropdown menu. 
                   #                               The expected and required variables are:", strong("Type, 
                   #                               Indicator, Subpopulation, Category, NResp, Estimate.P,
                   #                               StdError.P, LCB95Pct.P, UCB95Pct.P, Estimate.U,
                   #                               StdError.U, LCB95Pct.U, UCB95Pct.U")),
                   #          size = "s", easyClose = TRUE, fade = TRUE),
                   selectInput(inputId = "Estimate",
                               label = HTML("<b>Select Estimate Type</b>"),
                               choices = c("Proportion Estimates" = "P Estimates", "Unit Estimates" = "U Estimates"),
                               multiple = FALSE, 
                               width = "300px")  %>%
                     #Estimate Type helper
                     helper(type = "inline",
                            title = "Estimate Type",
                            content = c("<b>Proportion Estimates:</b> Proportion of observations that 
                                        belong to each level of the categorical variable.",
                                        "<b>Unit Estimates:</b> Total units (i.e. extent) that belong 
                                        to each level of the categorical variable (total number 
                                        (point resources), total line length (linear network), or 
                                        total area (areal network))."),
                            size = "s", easyClose = TRUE, fade = TRUE),
                   tags$style(HTML(".shiny-output-error-validation {color: #ff0000; font-weight: bold;}")),
                   selectInput(inputId = "Good",
                               label = HTML("<b>Select <span style='color: #5796d1'>'Good'</span> Condition Classes</b>"),
                               choices = "",
                               selected = NULL,
                               multiple = TRUE, 
                               width = "300px") %>%
                     #Condition Category helper
                     helper(type = "inline",
                            title = "Condition Category Color",
                            content = paste("The following inputs ask you to define the condition classes 
                                        which will be used in the plots. You may use the same 
                                        category for multiple conditions.", strong(em("For example, if 
                                        your dataset
                                        contains some responses for which Good is the best condition 
                                        and some for which Low is the best, you can select both for
                                        plotting. Only those applicable to a given response will be 
                                        shown in the plot."))),
                            size = "s", easyClose = TRUE, fade = TRUE),
                   selectInput(inputId = "Fair",
                               label = HTML("<b>Select <span style='color: #EE9A00'>'Fair'</span> Condition Classes</b>"),
                               choices = "",
                               selected = NULL,
                               multiple = TRUE, 
                               width = "300px"),
                   selectInput(inputId = "Poor",
                               label = HTML("<b>Select <span style='color: #f55b5b'>'Poor'</span> Condition Classes</b>"),
                               choices = "",
                               selected = NULL,
                               multiple = TRUE, 
                               width = "300px"),
                   selectInput(inputId = "Not_Assessed",
                               label = HTML("<b>Select <span style='color: #c77505'>'Not Assessed'</span> Condition Classes</b>"),
                               choices = "",
                               selected = NULL,
                               multiple = TRUE, 
                               width = "300px"),
                   selectInput(inputId = "Other",
                               label = HTML("<b>Select <span style='color: #d15fee'>'Other'</span> Condition Classes</b>"),
                               choices = "",
                               selected = NULL,
                               multiple = TRUE, 
                               width = "300px"),
                   textInput("title", "Add a Plot Title", value = "", width = "300px", placeholder = "Optional"),
                   textInput("resource", "Define Resource Type/Unit", value = "", width = "300px", placeholder = "Resource") %>%
                     #Resource Type helper
                     helper(type = "inline",
                            title = "Resource Type",
                            content = c("This input defines the plot axis label. Resource Type is 
                                        the resource evaluated in your design (e.g., Stream Miles, 
                                        Wetland Area, Coastal Area, Lakes)."),
                            size = "s", easyClose = TRUE, fade = TRUE),
                   actionButton("plotbtn", strong("Plot/Refresh Estimates"), icon=icon("chart-bar")),
                   br(),br(),
                   width = 3),
                 mainPanel(
                   conditionalPanel(condition="input.plotbtn",
                                    column(3,
                                           tags$head(tags$style(HTML("#Ind_Plot ~ .selectize-control.single .selectize-input {background-color: #FFD133;}"))),
                                           selectInput(inputId = "Ind_Plot",
                                                       label = HTML("<b>Select Indicator</b>"),
                                                       choices = "",
                                                       multiple = FALSE, 
                                                       width = "200px") %>%
                                             #Indicator input helper
                                             helper(type = "inline",
                                                    title = "Indicator",
                                                    content = c("Select the indicator to display 
                                                                categorical estimates by population."),
                                                    size = "s", easyClose = TRUE, fade = TRUE),
                                           
                                           #column(3, 
                                           tags$head(tags$style(HTML("#Type_Plot ~ .selectize-control.single .selectize-input {background-color: #FFD133;}"))),
                                           selectInput(inputId = "Type_Plot",
                                                       label = HTML("<b>Select Subpopulation Group</b>"),
                                                       choices = NULL,
                                                       multiple = FALSE,
                                                       width = "200px") %>%
                                             #Subpop Group input helper
                                             helper(type = "inline",
                                                    title = "Subpopulation Group",
                                                    content = c("Select the Subpopulation group 
                                                                to generate individual subpopulation 
                                                                estimates and to use in the subpopulation 
                                                                comparison plot."),
                                                    size = "s", easyClose = TRUE, fade = TRUE))),
                   column(8, offset = 2,
                          h3(strong(HTML("<center>Categorical Estimates by Population<center/>") %>%
                                      #Condition estimate helper
                                      helper(type = "inline",
                                             title = "Categorical Estimates",
                                             content = c("Cycle through populations to display 
                                                         categorical estimates and download the plot, 
                                                         if desired."),
                                             size = "s", easyClose = TRUE, fade = TRUE)))),
                   fluidRow(
                     column(3, offset=1,
                            conditionalPanel(condition="input.plotbtn",
                                             selectInput(inputId = "Tot_Pop",
                                                         label = HTML("<b>Select Subpopulation</b>"),
                                                         choices = NULL,
                                                         selected = NULL,
                                                         multiple = FALSE))),
                     column(3, offset=1,  
                            conditionalPanel(condition="input.plotbtn",
                                             checkboxInput(inputId = "indconlim", 
                                                           label= "Add Confidence Limit Values", 
                                                           value = FALSE, 
                                                           width = NULL)))),
                   fluidRow(
                     column(3, offset = 1,
                            conditionalPanel(condition="input.plotbtn",
                                             downloadButton("downloadPlot1", "Download Estimate Plot")))),
                   
                   plotOutput("plot", width = "75%", height = "300px"),
                   br(), br(), br(),
                   column(8, offset = 2,
                          h3(strong(HTML("<center>Subpopulation Comparison<center/>") %>%
                                      #Subpopulation helper
                                      helper(type = "inline",
                                             title = "Subpopulation Comparison",
                                             content = c("Cycle through population groups to compare categorical estimates by condition and download plot, if desired."),
                                             size = "s", easyClose = TRUE, fade = TRUE)))),
                   fluidRow(  
                     column(3, offset=1,
                            conditionalPanel(condition="input.plotbtn",
                                             # tags$head(tags$style(HTML("#Con_Plot ~ .selectize-control.single .selectize-input {border: 1px solid #33ACFF;}"))),
                                             selectInput(inputId = "Con_Plot",
                                                         label = HTML("<b>Select Condition</b>"),
                                                         choices = "",
                                                         multiple = FALSE))),
                     column(3, offset=1,
                            conditionalPanel(condition="input.plotbtn",
                                             checkboxInput(inputId = "goodsort", 
                                                           label= HTML("Sort Subpopulations by <span style='color: #5796d1'>'Good'</span> Condition"), 
                                                           value = FALSE, 
                                                           width = NULL),
                                             checkboxInput(inputId = "subconlim", 
                                                           label= "Add Confidence Limit Values", 
                                                           value = FALSE, 
                                                           width = NULL)))),
                   fluidRow(  
                     column(3, offset=1,
                            conditionalPanel(condition="input.plotbtn",
                                             downloadButton('downloadPlot2', "Download Subpopulation Plot")))),
                   plotOutput("plot2", width = "75%"))
               )),
      ####Continuous Plot UI####
      tabPanel(title="Plot Continuous Estimates",
               sidebarPanel(
                 radioButtons(inputId="coninput", 
                              label=strong("Choose Continuous Estimate Dataset to Use:"), 
                              choices=c("Upload Estimate Data File", "Current Estimate Data"), 
                              selected = "Upload Estimate Data File",
                              inline=FALSE),
                 uiOutput("conui"),
                 # div(id = "Coninput",
                 #                fileInput(
                 #                  inputId = "ConCDFinput",
                 #                  label = strong("Choose CDF Analysis file"),
                 #                  placeholder = "Must be a .csv file",
                 #                  accept = c(".csv"))) %>%
                 #              #File input helper
                 #              helper(type = "inline",
                 #                     title = "CDF Estimate File",
                 #                     content = paste("Choose a file with the same output which the 
                 #                     spsurvey package", strong("cont_analysis()"), 
                 #                                     "function renders. If the 
                 #                                 dataset is missing required variables, no 
                 #                                 selections will show up in the dropdown menu. 
                 #                     The expected and required variables are:", strong("Type,
                 #                     Subpopulation, Indicator, Value, Estimate.P, Estimate.U,
                 #                     StdError.P, StdError.U, LCB95Pct.P, UCB95Pct.P, LCB95Pct.U,
                 #                     UCB95Pct.U")),
                 #                     size = "s", easyClose = TRUE, fade = TRUE),
                            selectInput(inputId = "Estimate_CDF",
                                        label = HTML("<b>Select Estimate Type</b>"),
                                        choices = c("Proportion Estimates" = "P Estimates_CDF", "Unit Estimates" = "U Estimates_CDF"),
                                        multiple = FALSE, 
                                        width = "300px")  %>%
                              #Estimate Type helper
                              helper(type = "inline",
                                     title = "Estimate Type",
                                     content = c("<b>Proportion Estimates:</b> Proportion of 
                                                 observations that belong to each level of the 
                                                 categorical variable.",
                                                 "<b>Unit Estimates:</b> Total units (i.e. extent) 
                                                 that belong to each level of the categorical 
                                                 variable (total number (point resources), total 
                                                 line length (linear network), or total area 
                                                 (areal network))."),
                                     size = "s", easyClose = TRUE, fade = TRUE),
                            textInput("title2", "Add a Plot Title", value = "", width = "300px", placeholder = "Optional"),
                            textInput("units", "Add Indicator Units", value = "", width = "300px", placeholder = "Optional"),
                            textInput("resource2", "Define Resource Type/Unit", value = "", width = "300px", placeholder = "Resource") %>%
                              #Resource Type helper
                              helper(type = "inline",
                                     title = "Resource Type",
                                     content = c("This input defines the plot axis label. Resource
                                                 Type is the resource evaluated in your design 
                                                 (e.g., Stream Miles, Wetland Area, Coastline)."),
                                     size = "s", easyClose = TRUE, fade = TRUE), 
                            actionButton("plotbtncon", strong("Plot Continuous Estimates"), icon=icon("chart-bar"))
                            ,width=3),#sidebarPanel
               mainPanel(
                 column(3, offset=1,
                        conditionalPanel(condition="input.plotbtncon",
                                         selectInput(inputId = "Ind_Con",
                                                     label = HTML("<b>Select Indicator</b>"),
                                                     choices = "",
                                                     multiple = FALSE)),
                        conditionalPanel(condition="input.plotbtncon",
                                         selectInput(inputId = "Pop_Con",
                                                     label = HTML("<b>Select Population</b>"),
                                                     choices = "",
                                                     multiple = FALSE)),
                        conditionalPanel(condition="input.plotbtncon",
                                         selectInput(inputId = "SubPop_Con",
                                                     label = HTML("<b>Add/Remove Subpopulations</b>"),
                                                     choices = "",
                                                     multiple = TRUE))),
                 fluidRow(
                   column(3, offset=1,
                          conditionalPanel(condition="input.plotbtncon",
                                           numericInput("Thresh", "Indicator Threshold (optional)",
                                                        value = NULL),
                                           checkboxInput(inputId = "conflim", 
                                                         label="Add Confidence Limits", 
                                                         value = FALSE, 
                                                         width = NULL),
                                           checkboxInput(inputId = "log", 
                                                         label="Log Scale X-Axis", 
                                                         value = FALSE, 
                                                         width = NULL)))),
                 
                 column(8, offset = 2,
                        h3(strong(HTML("<center>CDF Estimates<center/>") %>%
                                    #CDF helper
                                    helper(type = "inline",
                                           title = "Cumulative Distribution Function (CDF)",
                                           content = c("A Cumulative Distribution Function calculates 
                                                       the cumulative probability for a given value and 
                                                       can be used to determine the probability that a 
                                                       random observation that is taken from the 
                                                       population will be less than or equal to a certain 
                                                       value."),
                                           size = "s", easyClose = TRUE, fade = TRUE))),
                 hr(),
                 h4("NOTE: Plotting and downloading may take a while if there are multiple
                          subpopulations. PLEASE BE PATIENT.")),
                 fluidRow(
                   column(3,
                          conditionalPanel(condition="input.plotbtncon",
                                           downloadButton("download1", "Download CDF Plot")))),
                 plotOutput("CDFsubplot", width = "75%"),
                 br(), br(), br(),
                 column(8, offset = 2,
                        h3(strong(HTML("<center>Distribution of Estimates by Population<center/>") %>%
                                    #CDF helper
                                    helper(type = "inline",
                                           title = "Ridgeline Distribution Plot",
                                           content = c("A <b>Ridgeline Plot</b>, also known as a Joy Plot, 
                                                       is used to visualize distributions of several groups.
                                                       Each group produces a density curve which overlaps 
                                                       with each other to help visualize differences.",
                                                       "",
                                                       "Small vertical lines represent values.",
                                                       "Large vertical lines represent quartiles."),
                                           size = "s", easyClose = TRUE, fade = TRUE)))),
                 fluidRow(
                   column(3,
                          conditionalPanel(condition="input.plotbtncon",
                                           downloadButton("download2", "Download Distribution Plot")))),
                 br(),
                 plotOutput("Distplot", width = "75%")
               ))
   )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observe_helpers()
   
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
          validate(
            need(input$stratumVar %nin% c(input$siteVar,input$respVar,input$weightVar),
                 "Stratum variable cannot overlap with other variable selections. If no stratum variable, select 'None'")
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
    
      # Create vartype variable depending on option selected
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
  
  ###################################### Categorical Plots Server ######################
  userEst <- reactive({
      estOut <- read.csv(req(input$userinput$datapath))
  })
  
  observeEvent(input$runBtn, {
    updateRadioButtons(session, "catinput", selected = "Current Estimate Data")
  })
  
  output$catui <- renderUI({
    req(input$catinput != "Current Estimate Data")
    div(id = "userinput1",
        fileInput(
          inputId = "userinput",
          label = strong("Choose Categorical Estimate File"),
          placeholder = "Must be a .csv file",
          accept = c(".csv"))) %>%
      #File input helper
      helper(type = "inline",
             title = "Categorical Estimate File",
             content = paste("Choose a file with the same output which the 
                                     spsurvey package", strong("cat_analysis()"), 
                             "function renders. If the 
                                                 dataset is missing required variables, no 
                                                 selections will show up in the dropdown menu. 
                                                 The expected and required variables are:", strong("Type, 
                                                 Indicator, Subpopulation, Category, NResp, Estimate.P,
                                                 StdError.P, LCB95Pct.P, UCB95Pct.P, Estimate.U,
                                                 StdError.U, LCB95Pct.U, UCB95Pct.U")),
             size = "s", easyClose = TRUE, fade = TRUE)
  })

  plotDataset <- eventReactive(c(input$userinput, input$runBtn, input$catinput), {
    if(input$catinput == "Current Estimate Data" && input$atype == 'categ') {
      dataEst()[['estOut']]
    } else {
      necVars <- c('Type', 'Indicator', 'Subpopulation', 'Category', 'NResp', 'Estimate.P',
                   'StdError.P', 'LCB95Pct.P', 'UCB95Pct.P', 'Estimate.U',
                   'StdError.U', 'LCB95Pct.U', 'UCB95Pct.U')
      
      validate(need(all(necVars %in% colnames(userEst())),
                    message = "Dataset does not include all variables in standardized output from spsurvey."))
      
      userEst <- userEst()
      print(colnames(userEst))
      userEst
    }
    
    
  })
  
  con_choices <- eventReactive(c(input$userinput, input$runBtn, input$catinput),{
    req(plotDataset())
    plotData <- plotDataset()
    
    plotData <- unique(subset(plotData, !(Category=='Total'), select='Category'))
    
  })
  
  # Good Condition Input
  observe({
    cc <- con_choices() 
    
    if(is.null(cc)){
      cc<-""
    } else{
      cc <- subset(cc, !(Category %in% c(input$Fair, input$Poor, input$Not_Assessed, input$Other)))
      dd <- unique(cc$Category)
    }
    updateSelectInput(session, "Good", choices = dd, 
                      selected = isolate(input$Good))
  })
  
  # Fair Condition Input
  observe({
    cc <- con_choices() 
    
    if(is.null(cc)){
      cc<-""
    } else{
      cc <- subset(cc, !(Category %in% c(input$Good, input$Poor, input$Not_Assessed, input$Other)))
      dd <- unique(cc$Category)
    }
    
    updateSelectInput(session, "Fair", choices = dd,  
                      selected = isolate(input$Fair))
  })
  
  # Poor Condition Input
  observe({
    cc <- con_choices() 
    
    if(is.null(cc)){
      cc<-""
    } else{
      cc <- subset(cc, !(Category %in% c(input$Good, input$Fair, input$Not_Assessed, input$Other)))
      dd <- unique(cc$Category)
    }
    
    updateSelectInput(session, "Poor", choices = dd,  
                      selected = isolate(input$Poor)) 
  })
  
  # Not Assessed Condition Input
  observe({
    cc <- con_choices() 
    
    if(is.null(cc)){
      cc<-""
    } else{
      cc <- subset(cc, !(Category %in% c(input$Good, input$Poor, input$Fair, input$Other)))
      dd <- unique(cc$Category)
    }
    
    updateSelectInput(session, "Not_Assessed", choices = dd,  
                      selected = isolate(input$Not_Assessed))
  })
  
  # Other Condition Input
  observe({
    cc <- con_choices() 
    
    if(is.null(cc)){
      cc<-""
    } else{
      cc <- subset(cc, !(Category %in% c(input$Good, input$Poor, input$Fair, input$Not_Assessed)))
      dd <- unique(cc$Category)
    }
    
    updateSelectInput(session, "Other", choices = dd, 
                      selected = isolate(input$Other))
  })
  
  observeEvent(input$plotbtn,{
    
    ind_plot <- plotDataset() 
    
    ind_plot <- unique(ind_plot$Indicator) 
    
    updateSelectInput(session, "Ind_Plot", choices = c(ind_plot))
  })
  
  observeEvent(input$plotbtn,{
    type_plot <- plotDataset()
    
    type_plot <- subset(type_plot, !(Type == input$Tot_Pop))
    
    type_plot.choice <- unique(type_plot$Type)
    
    updateSelectInput(session, "Type_Plot", choices = c(type_plot.choice))
    
  })
  
  #Total Population Label Input
  observeEvent(input$Type_Plot,{
    
    tot_pop <- plotDataset()
    tot_pop <- subset(tot_pop, Type == input$Type_Plot)
    tot_pop <- unique(tot_pop$Subpopulation)
    
    updateSelectInput(session, "Tot_Pop", choices = tot_pop)
  })
  
  
  observeEvent(input$Ind_Plot,{
    cc_plot <- plotDataset()
    
    cc_plot <- subset(cc_plot, Indicator == input$Ind_Plot & Category %in% c(input$Good, 
                                                input$Fair, input$Poor, input$Not_Assessed, input$Other))
    
    cc_plot.choice <- unique(cc_plot$Category)
    
    updateSelectInput(session, "Con_Plot", choices = c(cc_plot.choice))
  })
  
  
  Est_plot <- eventReactive(c(input$Tot_Pop, input$Ind_Plot, input$plotbtn, input$indconlim),{
    req(input$Ind_Plot, input$Type_Plot, input$Tot_Pop)
    
    #Set colors to users Condition classes
    col1 <- rep("#5796d1", length(input$Good))
    col2 <- rep("#EE9A00", length(input$Fair))
    col3 <- rep("#f55b5b", length(input$Poor))
    col4 <- rep("#c47a54", length(input$Not_Assessed))
    col5 <- rep("#d15fee", length(input$Other))
    names(col1) <- input$Good
    names(col2) <- input$Fair
    names(col3) <- input$Poor
    names(col4) <- input$Not_Assessed
    names(col5) <- input$Other
    colors <- c(col1, col2, col3, col4, col5)

    
    if (input$Estimate == "P Estimates") {
      Dataset <- plotDataset()
      
      Dataset <- subset(Dataset, select = c('Type', 'Subpopulation', 'Indicator', 'Category',
                                            'Estimate.P', 'StdError.P', 'LCB95Pct.P',
                                            'UCB95Pct.P'))
      
      Dataset$LCB95Pct.P <- with(Dataset, ifelse(LCB95Pct.P < 0, 0, LCB95Pct.P))
      Dataset$UCB95Pct.P <- with(Dataset, ifelse(UCB95Pct.P > 100, 100, UCB95Pct.P))
      Dataset$Estimate.P <- with(Dataset, round(Estimate.P, 0))
      Dataset$UCB95Pct.P <- with(Dataset, round(UCB95Pct.P, 0))
      Dataset$LCB95Pct.P <- with(Dataset, round(LCB95Pct.P, 0))
      
      
      names(Dataset)[names(Dataset) == "Estimate.P"] <- "Estimate"
      names(Dataset)[names(Dataset) == "StdError.P"] <- "StdError"
      names(Dataset)[names(Dataset) == "LCB95Pct.P"] <- "LCB95"
      names(Dataset)[names(Dataset) == "UCB95Pct.P"] <- "UCB95"
      
    } else {
      Dataset <- plotDataset()
      
      Dataset <- subset(Dataset, select = c('Type', 'Subpopulation', 'Indicator', 'Category',
                                            'Estimate.U', 'StdError.U', 'LCB95Pct.U',
                                            'UCB95Pct.U'))
      names(Dataset)[names(Dataset) == "Estimate.U"] <- "Estimate"
      names(Dataset)[names(Dataset) == "StdError.U"] <- "StdError"
      names(Dataset)[names(Dataset) == "LCB95Pct.U"] <- "LCB95"
      names(Dataset)[names(Dataset) == "UCB95Pct.U"] <- "UCB95"
      
     }
    
    # Fill in for all combinations of Indicator and Category for consistency across 
    # subpopulations
    popest <- Dataset
    
    cats.ind <- unique(Dataset[,c('Indicator', 'Category')])
    cats.sub <- unique(Dataset[,c('Type', 'Subpopulation')])
    
    cats.comb <- merge(cats.sub, cats.ind)
    popest <- merge(popest, cats.comb, by = c('Type','Subpopulation','Indicator','Category'), all=TRUE)
    popest[is.na(popest)] <- 0
    
    popest <- subset(Dataset, !(Category == "Total"))
    
    popest$Category <- with(popest, factor(Category, levels=c(input$Not_Assessed, input$Other, 
                                                              input$Poor, input$Fair, input$Good)))
    
    popest <- subset(popest, Indicator == input$Ind_Plot & Type == input$Type_Plot & 
                       Subpopulation == input$Tot_Pop)
    
    names.popest <- popest$Type
    req(input$Type_Plot %in% names.popest)  
    
    #Create Plots
    P1 <- ggplot(data = popest, aes(x = Category, y = Estimate)) +
      geom_bar(aes(fill = Category, color = Category), alpha = 0.5, 
               stat="identity", position = position_dodge()) +
      geom_errorbar(aes(ymin = LCB95, ymax = UCB95, color = Category), 
                    size=2, width=0) +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      theme_bw() +
      labs(
        title = input$title,
        subtitle= input$Ind_Plot,
        x = NULL)+
      theme(
        plot.title = element_text(size = 16, face = "bold", family="sans", hjust=0.5),
        plot.subtitle = element_text(size = 14, face = "bold", family="sans"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        legend.position="none",
        axis.text.x=element_text(face = "bold", size=14),
        axis.text.y=element_text(face = "bold", size=13),
        axis.title.x = element_text(face = "bold", size=14))
    
    
    if (input$Estimate == "P Estimates") {
      P1 <- P1 + geom_text(aes(label=paste(format(Estimate),"%",
                                           sep=""), y=Estimate), hjust = -.05, size = 4, 
                           fontface = "bold", color = "#4D4D4D", family="sans", 
                           position = position_nudge(x = -0.2)) +
        scale_y_continuous(labels = scales::percent_format(scale = 1),
                           breaks=c(0,25,50,75,100)) +
        coord_flip(ylim=c(0, 110)) +
        labs(y = paste0("Percentage of ", input$resource)) 
    } else {
      P1 <- P1 + geom_text(aes(label = format(round(Estimate), big.mark = ","), y=Estimate), 
                           hjust = -.05, size = 4, fontface = "bold", color = "#4D4D4D", 
                           family="sans", position = position_nudge(x = -0.25)) +
        scale_y_continuous(labels = scales::comma) +
        coord_flip() +
        labs(y = input$resource)
    }
    
    if (input$indconlim == TRUE && input$Estimate == "P Estimates") {
      P1 <- P1 + geom_text(aes(label=paste(format(LCB95),"%",
                                           sep=""), y=LCB95), hjust = 1.1, size = 3.5, fontface = "bold", 
                           color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15)) +
        geom_text(aes(label=paste(format(UCB95),"%",
                                  sep=""), y=UCB95), hjust = -.15,size = 3.5, fontface = "bold", 
                  color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15))
    }

    if (input$indconlim == TRUE && input$Estimate == "U Estimates") {
      ylim <- max(popest$UCB95) * 1.1 
      
      P1 <- P1 + geom_text(aes(label = format(round(LCB95), big.mark = ","), y=LCB95), 
                           hjust = 1.1, size = 3.5, fontface = "bold", color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15)) +
        geom_text(aes(label = format(round(UCB95), big.mark = ","), y=UCB95), 
                  hjust = .2, size = 3.5, fontface = "bold", color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15)) +
        ylim(0, ylim)
    }
    
    print(P1)
  
    
  })
  
  SubEst_plot <- eventReactive(c(input$plotbtn, input$goodsort, input$subconlim, 
                                 input$Type_Plot, input$Ind_Plot, input$Con_Plot),{
    req(input$Type_Plot %nin% c("ALL SITES", "All Sites", "all sites", "All_Sites", "All.Sites", "all_sites", "all.sites", "National", "national", "NATIONAL", "Statewide", "statewide", "STATEWIDE"))
    
                                    
    #Set colors to users Condition classes
    col1 <- rep("#5796d1", length(input$Good))
    col2 <- rep("#EE9A00", length(input$Fair))
    col3 <- rep("#f55b5b", length(input$Poor))
    col4 <- rep("#c47a54", length(input$Not_Assessed))
    col5 <- rep("#d15fee", length(input$Other))
    names(col1) <- input$Good
    names(col2) <- input$Fair
    names(col3) <- input$Poor
    names(col4) <- input$Not_Assessed
    names(col5) <- input$Other
    colors <- c(col1, col2, col3, col4, col5)
    
    
    if (input$Estimate == "P Estimates") {
      Dataset <- plotDataset()
      
      Dataset <- subset(Dataset, select = c('Type', 'Subpopulation', 'Indicator', 'Category',
                                            'Estimate.P', 'StdError.P', 'LCB95Pct.P',
                                            'UCB95Pct.P'))
      Dataset$LCB95Pct.P <- with(Dataset, ifelse(LCB95Pct.P < 0, 0, LCB95Pct.P))
      Dataset$UCB95Pct.P <- with(Dataset, ifelse(UCB95Pct.P > 100, 100, UCB95Pct.P))
      Dataset$Estimate.P <- with(Dataset, round(Estimate.P, 0))
      Dataset$UCB95Pct.P <- with(Dataset, round(UCB95Pct.P, 0))
      Dataset$LCB95Pct.P <- with(Dataset, round(LCB95Pct.P, 0))
      
      
      names(Dataset)[names(Dataset) == "Estimate.P"] <- "Estimate"
      names(Dataset)[names(Dataset) == "StdError.P"] <- "StdError"
      names(Dataset)[names(Dataset) == "LCB95Pct.P"] <- "LCB95"
      names(Dataset)[names(Dataset) == "UCB95Pct.P"] <- "UCB95"
      
    } else {
      Dataset <- plotDataset()
      
      Dataset <- subset(Dataset, select = c('Type', 'Subpopulation', 'Indicator', 'Category',
                                            'Estimate.U', 'StdError.U', 'LCB95Pct.U',
                                            'UCB95Pct.U'))
      names(Dataset)[names(Dataset) == "Estimate.U"] <- "Estimate"
      names(Dataset)[names(Dataset) == "StdError.U"] <- "StdError"
      names(Dataset)[names(Dataset) == "LCB95Pct.U"] <- "LCB95"
      names(Dataset)[names(Dataset) == "UCB95Pct.U"] <- "UCB95"
      
    }
    
    popest2 <- Dataset
   
    popest2 <- subset(popest2, Type == input$Type_Plot)
  
    
    cats.ind.1 <- unique(popest2[,c('Indicator', 'Category')])
    cats.sub.1 <- unique(popest2[,c('Type', 'Subpopulation')])

    cats.comb.1 <- merge(cats.sub.1, cats.ind.1)
    popest2 <- merge(popest2, cats.comb.1, by = c('Type','Subpopulation','Indicator','Category'),
                     all=TRUE)
    # popest2 <- tidyr::complete(popest2, Type, Subpopulation, nesting(Indicator, Category))
    
    popest2[is.na(popest2)] <- 0
    
    popest2 <- subset(popest2, Indicator == input$Ind_Plot & !(Category == 'Total'))
    # popest2$Subpopulation <-factor(popest2$Subpopulation, levels = rev(unique(popest2$Subpopulation)))
    
    # Creates vector of category variables
    firstcon <- popest2
    # NOT SURE WHAT THIS IS TRYING TO DO AND WHY SO COMPLEX
    firstcon$Category <- factor(firstcon$Category, levels = c(input$Good, input$Fair,
                                                              input$Poor, input$Other,
                                                              input$Not_Assessed))

    firstcon <- firstcon[order(firstcon$Type, firstcon$Subpopulation, firstcon$Category),]
    
    # firstcon <- dplyr::arrange(firstcon, Type, Subpopulation,
    #                            factor(Category, levels=c(.env$input$Good, .env$input$Fair, .env$input$Poor, .env$input$Other, .env$input$Not_Assessed)))
    # firstcon$Type <- with(firstcon, factor(Category, levels=c(input$Good, input$Fair, input$Poor, input$Other, input$Not_Assessed)))
    firstcon <- subset(firstcon, Category %in% c(input$Good, input$Fair, input$Poor, input$Other, input$Not_Assessed))
    firstcon <- unique(firstcon$Category)
    
    # Creates vector of subpopulations by order of users 'good' condition  
    suborder <- popest2 
    suborder <- subset(suborder, Category == firstcon[1])
    suborder <- suborder[order(suborder$Estimate),]
    
    #suborder$Subpopulation <- forcats::fct_reorder(suborder$Subpopulation, suborder$Estimate)
    suborder <- unique(suborder$Subpopulation)
    
    if(input$goodsort == TRUE){
      # Filters and Arranges user data based on good condition
      popest2 <- subset(popest2, Category == input$Con_Plot)
      popest2$Subpopulation <- factor(popest2$Subpopulation, levels = suborder) 
    } else {
      popest2 <- subset(popest2, Category == input$Con_Plot)
      popest2$Subpopulation <- factor(popest2$Subpopulation, 
                                      levels = rev(unique(popest2$Subpopulation)))
    }
    
    names <- popest2$Type
    # pluck(popest2, "Type") 
    req(input$Type_Plot %in% names)                               
    
    P2 <- ggplot(data = popest2, aes(x = Subpopulation, y = Estimate)) +
      geom_bar(aes(fill = Category, color = Category), alpha = 0.5, stat="identity", 
               position = position_dodge()) +
      geom_errorbar(aes(ymin = LCB95, ymax = UCB95, color = Category), size=2, width=0) +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
      theme_bw() +
      labs(
        title = input$title,
        subtitle= input$Ind_Plot,
        x = NULL)+
      theme(
        plot.title = element_text(size = 16, face = "bold", family="sans", hjust=0.5),
        plot.subtitle = element_text(size = 15, face = "bold", family="sans"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        legend.position="none",
        axis.text.x=element_text(face = "bold", size=14),
        axis.text.y=element_text(face = "bold", size=13),
        axis.title.x = element_text(face = "bold", size=14))
    
    if (input$Estimate == "P Estimates") {
      P2 <- P2 + geom_text(aes(label=paste(format(Estimate),"%",
                                           sep=""), y=Estimate), hjust = -.05, size = 4, 
                           fontface = "bold", color = "#4D4D4D", family="sans", 
                           position = position_nudge(x = -0.2)) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), breaks=c(0,25,50,75,100)) +
        coord_flip(ylim=c(0, 110)) +
        labs(y = paste0("Percentage of ", input$resource)) 
    } else {
      P2 <- P2 + geom_text(aes(label = format(round(Estimate), big.mark = ","), y=Estimate), 
                           hjust = -.05, size = 4, fontface = "bold", color = "#4D4D4D", 
                           family="sans", position = position_nudge(x = -0.2)) +
        scale_y_continuous(labels = scales::comma) +
        coord_flip() +
        labs(y = paste0("Amount of ", input$resource))
    }
    
    if (input$subconlim == TRUE && input$Estimate == "P Estimates") {
      P2 <- P2 + geom_text(aes(label=paste(format(LCB95),"%",
                                           sep=""), y=LCB95), hjust = 1.1, size = 3.5, 
                           fontface = "bold", 
                           color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15)) +
        geom_text(aes(label=paste(format(UCB95),"%",
                                  sep=""), y=UCB95), hjust = -.15, size = 3.5, fontface = "bold", 
                  color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15))
    }
   
    if (input$subconlim == TRUE && input$Estimate == "U Estimates") {
      ylim <- max(popest2$UCB95) * 1.1 
      
      P2 <- P2 + geom_text(aes(label = format(round(LCB95), big.mark = ","), y=LCB95), 
                           hjust = 1.1, size = 3.5, fontface = "bold", color = "#4D4D4D", 
                           family="sans", position = position_nudge(x = 0.15)) +
        geom_text(aes(label = format(round(UCB95), big.mark = ","), y=UCB95), 
                  hjust = .2, size = 3.5, fontface = "bold", color = "#4D4D4D", family="sans", 
                  position = position_nudge(x = 0.15)) +
        ylim(0, ylim)
    }
    
    print(P2)
  })
  
  output$plot <- renderPlot({
    Est_plot()
    
  })

  calcheight <- reactive({
    popest <- plotDataset()
    # Prevents plotting error
    names.popest <- popest$Type
    
    req(input$Type_Plot %in% names.popest)
    
    # popest <- plotDataset()
    popest <- unique(subset(popest, Type == input$Type_Plot, select = 'Subpopulation'))
    
    calcheight <- 70 * length(popest$Subpopulation)})
  
  
  output$plot2 <- renderPlot({
    req(calcheight())
    calcheight <- calcheight()
    SubEst_plot()
  }, height = calcheight)
  
  
  output$downloadPlot1 <- downloadHandler(
    filename = function() { paste("Indicator_Estimates-", Sys.Date(), '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = Est_plot(), width=8, height=4)
    }
  )
  
  output$downloadPlot2 <- downloadHandler(
    filename = function() { paste("SubPop_Estimates-", Sys.Date(), '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = SubEst_plot())
    }
  )
  ####################### Continuous Server ##############################
  userCDFEst <- reactive({
    ConEstOut <- read.csv(req(input$ConCDFinput$datapath))
  })
  
  
  observeEvent(input$runBtn, {
    updateRadioButtons(session, "coninput", selected = "Current Estimate Data")
  })
  
  output$conui <- renderUI({
    req(input$coninput != "Current Estimate Data")
    fileInput(
      inputId = "ConCDFinput",
      label = strong("Choose CDF Analysis file"),
      placeholder = "Must be a .csv file",
      accept = c(".csv")) %>%
      #File input helper
      helper(type = "inline",
             title = "CDF Estimate File",
             content = paste("Choose a file with the same output which the 
                                     spsurvey package", strong("cont_analysis()"), 
                             "function renders. If the 
                                                 dataset is missing required variables, no 
                                                 selections will show up in the dropdown menu. 
                                     The expected and required variables are:", strong("Type,
                                     Subpopulation, Indicator, Value, Estimate.P, Estimate.U,
                                     StdError.P, StdError.U, LCB95Pct.P, UCB95Pct.P, LCB95Pct.U,
                                     UCB95Pct.U")),
             size = "s", easyClose = TRUE, fade = TRUE)
  })
  
  CDFDataset <- eventReactive(c(input$ConCDFinput, input$runBtn, input$coninput), {
    if(input$coninput == "Current Estimate Data" && input$cdf_pct=='cdf') {
      dataEst()[['estOut']]
    } else {
      validate(need(all(c('Type', 'Subpopulation', 'Indicator', 'Value', 'Estimate.P', 'Estimate.U',
                          'StdError.P', 'StdError.U', 'LCB95Pct.P', 'UCB95Pct.P', 'LCB95Pct.U',
                          'UCB95Pct.U') %in% names(userCDFEst())),
                    "Standardized spsurvey variables not included in dataset."))
      
      CDFOut <- userCDFEst() 
    }})
  
  
  
  observe({
    indcon_choice <- CDFDataset()
    
    indcon_choice <- unique(indcon_choice$Indicator)
    
    updateSelectInput(session, "Ind_Con", choices = indcon_choice)
  })
  
  observe({
    popcon_choice <- CDFDataset()
    
    popcon_choice <- unique(popcon_choice$Type)
    
    updateSelectInput(session, "Pop_Con", choices = popcon_choice)
  })
  
  observe({
    req(input$Pop_Con)
    subpopcon_choice <- CDFDataset()
    
    subpopcon_choice <- subset(subpopcon_choice, Type==input$Pop_Con, select = Subpopulation)
    
    subpopcon_choice <- unique(subpopcon_choice$Subpopulation)
    
    allsites_sub <- CDFDataset()
    
    allsites_sub <- subset(allsites_sub, Type %in% c("ALL SITES", "All Sites", "all sites", "All_Sites", "All.Sites", "all_sites", "all.sites", "National", "national", "NATIONAL", "Statewide", "statewide", "STATEWIDE"), select = 'Subpopulation')
    
    allsites_sub.choice <- unique(allsites_sub$Subpopulation)
    
    updateSelectInput(session, "SubPop_Con", choices = c(allsites_sub.choice, subpopcon_choice), 
                      selected = allsites_sub.choice[1])
  })
  
  CDF_subplot <- reactive({
    req(input$plotbtncon, input$SubPop_Con)
    
    if (input$Estimate_CDF == "P Estimates_CDF") {
      CDFDataset <- CDFDataset()  
      names(CDFDataset)[names(CDFDataset) == "Estimate.P"] <- "Estimate"
      names(CDFDataset)[names(CDFDataset) == "StdError.P"] <- "StdError"
      names(CDFDataset)[names(CDFDataset) == "LCB95Pct.P"] <- "LCB95"
      names(CDFDataset)[names(CDFDataset) == "UCB95Pct.P"] <- "UCB95"
      CDFDataset <- subset(CDFDataset, select = c('Type', 'Subpopulation', 'Indicator', 'Value', 'Estimate', 'StdError', 'LCB95', 'UCB95'))
    } else {
      CDFDataset <- CDFDataset()
      names(CDFDataset)[names(CDFDataset) == "Estimate.U"] <- "Estimate"
      names(CDFDataset)[names(CDFDataset) == "StdError.U"] <- "StdError"
      names(CDFDataset)[names(CDFDataset) == "LCB95Pct.U"] <- "LCB95"
      names(CDFDataset)[names(CDFDataset) == "UCB95Pct.U"] <- "UCB95"
      CDFDataset <- subset(CDFDataset, select = c('Type', 'Subpopulation', 'Indicator', 'Value', 'Estimate', 'StdError', 'LCB95', 'UCB95'))
    }
    
    CDFDataset <- subset(CDFDataset, Indicator == input$Ind_Con & Subpopulation %in% input$SubPop_Con)
    CDFDataset$Subpopulation <-factor(CDFDataset$Subpopulation, levels = rev(unique(CDFDataset$Subpopulation)))
    
    g <- ggplot(CDFDataset, aes(y=Estimate, x=Value, color = Subpopulation, fill = Subpopulation)) +
      geom_step(size=1) +
      scale_colour_viridis_d("Population", guide = guide_legend(reverse = TRUE)) +
      scale_fill_viridis_d() +
      theme_bw() +
      labs(
        title = input$title2,
        subtitle= "",
        x = paste0(input$Ind_Con," ",input$units)
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold", family="sans", hjust=0.5),
        plot.subtitle = element_text(size = 14, face = "bold", family="sans"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        legend.title = element_text(face = "bold", size=14), 
        legend.text = element_text(face = "bold", size=12),
        axis.text.x=element_text(face = "bold", size=14),
        axis.text.y=element_text(face = "bold", size=13),
        axis.title.x = element_text(face = "bold", size=14),
        axis.title.y = element_text(face = "bold", size=14))
    
    
    if(input$Estimate_CDF == "P Estimates_CDF") {
      g <- g + scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        labs(y = paste0("Cumulative Probability ", input$resource2)) 
    } else {
      g <- g + scale_y_continuous(labels = scales::comma) +
        labs(y = paste0("Cumulative amount of ", input$resource2)) 
    }
    
    if (is.numeric(input$Thresh)) {
      g <- g + geom_vline(xintercept = input$Thresh, color = "red", size = 1, linetype = "longdash")
    }
    if (input$conflim == TRUE) {
      g <- g + geom_ribbon(aes(ymin = LCB95, ymax = UCB95, fill = Subpopulation), alpha = 0.2,
                           colour = "transparent", show.legend = FALSE)
    }
    if (input$log == TRUE) {
      g <-  g + scale_x_continuous(trans='log10') 
    }
    
    g
  })
  
  Dist_plot <- reactive({
    req(input$plotbtncon)
    
    if (input$Estimate_CDF == "P Estimates_CDF") {
      CDFDataset <- CDFDataset()  
      names(CDFDataset)[names(CDFDataset) == "Estimate.P"] <- "Estimate"
      names(CDFDataset)[names(CDFDataset) == "StdError.P"] <- "StdError"
      names(CDFDataset)[names(CDFDataset) == "LCB95Pct.P"] <- "LCB95"
      names(CDFDataset)[names(CDFDataset) == "UCB95Pct.P"] <- "UCB95"
      CDFDataset <- subset(CDFDataset, select = c('Type', 'Subpopulation', 'Indicator', 'Value', 'Estimate', 'StdError', 'LCB95', 'UCB95'))
    } else {
      CDFDataset <- CDFDataset()
      names(CDFDataset)[names(CDFDataset) == "Estimate.U"] <- "Estimate"
      names(CDFDataset)[names(CDFDataset) == "StdError.U"] <- "StdError"
      names(CDFDataset)[names(CDFDataset) == "LCB95Pct.U"] <- "LCB95"
      names(CDFDataset)[names(CDFDataset) == "UCB95Pct.U"] <- "UCB95"
      CDFDataset <- subset(CDFDataset, select = c('Type', 'Subpopulation', 'Indicator', 'Value', 'Estimate', 'StdError', 'LCB95', 'UCB95'))
    }
    
    CDFDataset <- subset(CDFDataset, Indicator == input$Ind_Con & Subpopulation %in% input$SubPop_Con)
    #CDFDataset <- with(CDFDataset, reorder(Subpopulation, Estimate, decreasing=FALSE))
    #CDFDataset$Subpopulation <-factor(CDFDataset$Subpopulation, levels = rev(unique(CDFDataset$Subpopulation)))
    #CDFDataset <- suborder[order(CDFDataset$Estimate),]
    CDFDataset$Subpopulation <- forcats::fct_reorder(CDFDataset$Subpopulation, CDFDataset$Estimate)
    
    g <- ggplot(CDFDataset, aes(x = Estimate, y = Subpopulation, fill=Subpopulation)) +
      scale_fill_viridis_d("Population") +
      labs(
        title = input$title2,
        subtitle= "",
        y = "Frequency") +
      theme(
        plot.title = element_text(size = 16, face = "bold", family="sans", hjust=0.5),
        plot.subtitle = element_text(size = 14, face = "bold", family="sans"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        legend.title = element_text(face = "bold", size=14), 
        legend.text = element_text(face = "bold", size=12),
        axis.text.x=element_text(face = "bold", size=14),
        axis.text.y=element_text(face = "bold", size=13),
        axis.title.x = element_text(face = "bold", size=14),
        axis.title.y = element_text(face = "bold", size=14),
        legend.position = "none")
    
    if(input$Estimate_CDF == "P Estimates_CDF") {
      g <- g + scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
        labs(x = paste0("Percentage of ", input$resource2)) 
      
    } else {
      g <- g + scale_x_continuous(labels = scales::comma) +
        labs(x = paste0("Amount of ", input$resource2)) 
    }
    
    if (length(input$SubPop_Con) == 1 && input$Estimate_CDF == "U Estimates_CDF") {
      g + geom_density_ridges(scale = 100000, size = 0.9, jittered_points = TRUE,
                              position = position_points_jitter(width = 0.5, height = 0),
                              point_shape = "|", point_size = 3,
                              quantile_lines = TRUE, alpha=0.8)
    } else if (length(input$SubPop_Con) == 1)  {
      g + geom_density_ridges(scale = 50, size = 0.5, jittered_points = TRUE,
                              position = position_points_jitter(width = 0.5, height = 0),
                              point_shape = "|", point_size = 3,
                              quantile_lines = TRUE, alpha=0.8)
    } else {
      g + geom_density_ridges(size = 1, rel_min_height = 0.03, jittered_points = TRUE,
                              position = position_points_jitter(width = 0.5, height = 0),
                              point_shape = "|", point_size = 3,
                              quantile_lines = TRUE, alpha=0.8) 
    }
  })
  
  output$CDFsubplot <- renderPlot({
    req(input$plotbtncon)
    CDF_subplot()
  })
  
  
  output$download1 <- downloadHandler(
    filename = function() {paste("CDF_Plot-", Sys.Date(), ".png", sep="")},
    content = function(file) {
      ggsave(file, CDF_subplot())
    })
  
  calcheight2 <- reactive({
    req(input$SubPop_Con)
    popcount <- CDFDataset()
    
    popcount <- unique(subset(popcount, Subpopulation %in% input$SubPop_Con, select = 'Subpopulation'))
    
    if (length(input$SubPop_Con) == 1) {
      calcheight <- 250
    } else {
      calcheight <- 100 * length(popcount$Subpopulation)}})
  
  
  output$Distplot <- renderPlot({
    req(calcheight2())
    calcheight <- calcheight2()
    Dist_plot()
  }, height = calcheight2)
  
  output$download2 <- downloadHandler(
    filename = function() {paste("CDF_Distribution-", Sys.Date(), ".png", sep="")},
    content = function(file) {
      ggsave(file, Dist_plot())
    })
  
  
  # End session if browser window is closed
  session$onSessionEnded(function() {
    rm(warn_df,envir=.GlobalEnv)
    stopApp()
  })  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

