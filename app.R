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
  tags$html(class = "no-js", lang="en"),
  tags$head(
    tags$title('NARS Pop Est Tool| US EPA'),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    includeHTML("www/header.html")),

   shinyjs::useShinyjs(),
   # Application title

# Instructions ------------------------------------------------------------
    navbarPage(title=span("NARS Population Estimate Calculation Tool (v. 2.3.0)",
                         style = "font-weight: bold; font-size: 24px"),         
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
                 tags$li("If data are to be used for change analysis, select the variable that distinguishes between design cycles (we usually assume this variable represents year)."),
                tags$li(p("Select the type of variance you want to use. ",
                          strong("Local neighborhood variance"),
                          " uses a site's nearest neighbors to estimate variance, tending to
                          result in smaller variance values than variance based on a simple random sample. This approach is ",
                          em("recommended"),"and is the approach used in
                          NARS estimates. It requires site coordinates to be provided."),
                        tags$ul(
                        tags$li("For local neighborhood variance, select coordinate variables
                                (Albers projection, or some other projection)."),
                        tags$li("For simple random sample (SRS) variance, selecting a stratum variable
                                to better estimate variance is advised but not required. Coordinates are
                                not used with this type of variance."))),
                br(),
                tags$li("You may subset the data for analysis by up to one categorical variable. To do
                        this, select the check box to subset, then select the variable to subset by.
                        Finally, select one or more categories by which to subset data."),
                tags$li("Click on the left hand button to view the full dataset if necessary."),
               tags$li("Click on the right hand button above the data to subset the data before
                       proceeding to the Run Population Estimates tab.")
               )),
               bsCollapsePanel(title = h4(strong("Minimum Requirements for Analysis")),
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
                                         strong("only one row per site and year/survey cycle"),
                                         "(based on the variables for site ID and year/survey cycle selected). No within year revisits should be included, and all variables used in analysis should be separate columns in the dataset (i.e., wide format)."),
                                 tags$li("Only delimited files, such as comma- and tab-delimited, are accepted for upload."),
                                 tags$li("If local neighborhood variance is desired, coordinates must be
                                 provided in some type of projection, such as Albers."),
                                 tags$li("If variance based on a simple random sample is desired (or if
                                 coordinates are not available), the design stratum should be provided
                                 to better estimate variance."),
                                 tags$li("If change analysis is intended, all desired years of data must be
                                 contained in one file, with a single variable that identifies
                                         the individual years or survey cycles included.")
                               )),
               bsCollapsePanel(title = h4(strong("Run Population Estimates")),
               tags$ol(
                 tags$li("Select the type of analysis (categorical or continuous)."),
                 tags$li("Select the confidence level for estimating confidence intervals (90% or 95%).
                         The default is 95%."),
                 tags$li("If year or design cycle variable was selected on the Prepare Data for
                 Analysis tab, select year or cycle of interest."),
                 tags$li("For continuous analysis, select either CDFs (cumulative distribution
                         functions), percentiles, means, or totals."),
                 tags$li("Note that if data are missing for continuous variables,
                         those sites are ignored in analysis."),
                 tags$li("Click on the Run/Refresh Estimates button. Depending on the number of
                         responses, subpopulations, and type of analysis, it may take a few seconds
                         to several minutes."),
                 tags$li("If desired, download results to a comma-delimited file by clicking
                         the Save Results button.")
               ),
               tags$ol(strong("Outputs for categorical analysis:")),
                  tags$li("Type = Subpopulation group"),
                  tags$li("Subpopulation = Subpopulation name"),
                  tags$li("Indicator = Name of indicator"),
                  tags$li("Category = Category of indicator or Total"),
                  tags$li("nResp = Number of responses in category"),
                  tags$li("Estimate.P = Estimated percent of resource in category"),
                  tags$li("StdError.P = Estimated standard error of percent estimate"),
                  tags$li("MarginofError.P = Margin of error of percent estimate, representing difference between estimate and confidence bounds"),
                  tags$li("LCBXXPct.P = Lower confidence bound for percent, where XX represents the confidence level"),
                  tags$li("UCBXXPct.P = Upper confidence bound for percent, where XX represents the confidence level"),
                  tags$li("Estimate.U = Estimated amount of resource in category in same units as weights used"),
                  tags$li("StdError.U = Estimated standard error of amount of resource estimate"),
                  tags$li("MarginofError.U = Margin of error of amount of resource estimate, representing 
                          difference between estimate and confidence bounds"),
                  tags$li("LCBXXPct.U = Lower confidence bound for amount of resource, where XX represents the 
                          confidence level"),
                  tags$li("UCBXXPct.U = Upper confidence bound for amount of resource, where XX represents the 
                          confidence level"),
               br(),
               tags$ol(strong("Outputs for continuous analysis:")),
                   tags$li("Type = Subpopulation group"),
                   tags$li("Subpopulation = Subpopulation name"),
                   tags$li("Indicator = Name of indicator"),
                   tags$li("Value = Value of indicator (CDF only)"),
                   tags$li("Statistic = Value of indicator (Percentiles only)"),
                   tags$li("nResp = Number of responses in category"),
                   tags$li("Estimate.P = Estimated percent of resource at or below value (CDF only)"),
                   tags$li("Estimate = Estimated value for given percentile (Percentiles only)"),
                   tags$li("StdError.P = Estimated standard error of percent estimate (CDF only)"),
                   tags$li("StdError = Estimated standard error of mean, variance, or standard deviation 
                           estimate (Percentiles only)"),
                   tags$li("LCBXXPct.P = Lower confidence bound for percent, where XX represents the confidence level (CDF only)"),
                   tags$li("UCBXXPct.P = Upper confidence bound for percent, where XX represents the confidence level (CDF only"),
                   tags$li("LCBXXPct = Lower confidence bound for percentile estimate, where XX represents the 
                           confidence level (Percentiles only)"),
                   tags$li("UCBXXPct = Upper confidence bound for percentile estimate, where XX represents the 
                           confidence level (Percentiles only)"),
                   tags$li("Estimate.U = Estimated amount of resource at or below value in same units as weights 
                           used (CDF only)"),
                   tags$li("StdError.U = Estimated standard error of amount of resource at or below estimate (CDF only)"),
                   
                   tags$li("LCBXXPct.U = Lower confidence bound for amount of resource, where XX represents the 
                           confidence level (CDF only)"),
                   tags$li("UCBXXPct.U = Upper confidence bound for amount of resource, where XX represents the 
                           confidence level (CDF only)")
               ),
               bsCollapsePanel(title = h4(strong("Run Change Analysis")),
               tags$ol(
                 tags$li("First select the two years (or sets of years) to compare."),
                 tags$li("Select type of data to analyze (categorical or continuous)."),
                 tags$li("Select the confidence level for estimating confidence intervals (90% or 95%).
                         The default is 95%."),
                 tags$li("If continuous data are selected, select parameter on which to test
                         for differences (mean or median)."),
                 tags$li("Click on the Run/Refresh Estimates button. Depending on the number of
                 responses, subpopulations, and type of analysis, it may take a few
                         seconds to several minutes."),
                 tags$li("If any data are changed in the Prepare Data for Analysis tab, years
                         must be re-selected before running analysis.")
               ),
               tags$ul(strong("Outputs for Categorical Analysis:"),
                       tags$li("Survey_1 = Year or design cycle of first survey"),
                       tags$li("Survey_2 = Year or design cycle of second survey"),
                       tags$li("Type = Subpopulation group"),
                       tags$li("Subpopulation = Subpopulation name"),
                       tags$li("Indicator = Name of indicator"),
                       tags$li("Category = Category of indicator or Total"),
                       tags$li("DiffEst.P = Estimate of difference in percentage (Survey_2 - Survey_1)"),
                       tags$li("StdError.P = Estimated standard error of change percent estimate"),
                       tags$li("MarginofError.P = Margin of error of change percent estimate, representing 
                               difference between estimate and confidence bounds"),
                       tags$li("LCBXXPct.P = Lower confidence bound for change percent, where XX represents 
                               the confidence level"),
                       tags$li("UCBXXPct.P = Upper confidence bound for change percent, where XX represents 
                               the confidence level"),
                       tags$li("DiffEst.U = Estimated amount of change in resource in category in same units 
                               as weights used"),
                       tags$li("StdError.U = Estimated standard error of amount of change in resource estimate"),
                       tags$li("MarginofError.U = Margin of error of amount of change in resource estimate, representing 
                          difference between estimate and confidence bounds"),
                       tags$li("LCBXXPct.U = Lower confidence bound for amount of change resource, where XX represents the 
                          confidence level"),
                       tags$li("UCBXXPct.U = Upper confidence bound for amount of change resource, where XX represents the 
                          confidence level"),
                       tags$li("nResp_1, nResp_2 = Number of responses in category in survey 1 and survey 2, respectively"),
                       tags$li("Estimate.P_1, Estimate.P_2 = Estimated percent of resource in category in survey 1 
                               and survey 2, respectively"),
                       tags$li("StdError.P_1, StdError.P_2 = Estimated standard error of percent estimate in survey 1 
                               and survey 2, respectively"),
                       tags$li("MarginofError.P_1, MarginofError.P_2 = Margin of error of percent estimate, 
                               representing difference between estimate and confidence bounds in survey 1 
                               and survey 2, respectively"),
                       tags$li("LCBXXPct.P_1, LCBXXPct.P_2 = Lower confidence bound for percent, where XX represents 
                               the confidence level in survey 1 and survey 2, respectively"),
                       tags$li("UCBXXPct.P_1, UCBXXPct.P_2 = Upper confidence bound for percent, where XX represents 
                               the confidence level in survey 1 and survey 2, respectively"),
                       tags$li("Estimate.U_1, Estimate.U_2 = Estimated amount of resource in category in same units 
                               as weights used, in survey 1 and survey 2, respectively"),
                       tags$li("StdError.U_1, StdError.U_2 = Estimated standard error of amount of resource estimate 
                               in survey 1 and survey 2, respectively"),
                       tags$li("MarginofError.U_1, MarginofError.U_2 = Margin of error of amount of resource estimate,
                               representing difference between estimate and confidence bounds in survey 1 and survey 2,
                               respectively"),
                       tags$li("LCBXXPct.U_1, LCBXXPct.U_2 = Lower confidence bound for amount of resource, where 
                               XX represents the confidence level in survey 1 and survey 2, respectively"),
                       tags$li("UCBXXPct.U_1, UCBXXPct.U_2 = Upper confidence bound for amount of resource, 
                               where XX represents the confidence level in survey 1 and survey 2, respectively")
                       ),
               tags$ul(strong("Outputs for Continuous Analysis (Means):"),
                       tags$li("Survey_1 = Year or design cycle of first survey"),
                       tags$li("Survey_2 = Year or design cycle of second survey"),
                       tags$li("Type = Subpopulation group"),
                       tags$li("Subpopulation = Subpopulation name"),
                       tags$li("Indicator = Name of indicator"),
                       tags$li("DiffEst = Estimate of difference in mean for indicator (Survey_2 - Survey_1)"),
                       tags$li("StdError = Estimated standard error of change estimate"),
                       tags$li("MarginofError = Margin of error of estimated mean, representing 
                                       difference between estimate and confidence bounds"),
                       tags$li("LCBXXPct = Lower confidence bound for estimated mean, where XX represents 
                                       the confidence level"),
                       tags$li("UCBXXPct = Upper confidence bound for estimated mean, where XX represents 
                                       the confidence level"),
                       tags$li("nResp_1, nResp_2 = Number of responses for indicator in survey 1 and survey 2, respectively"),
                       tags$li("Estimate_1, Estimate_2 = Estimated mean of resource for indicator in survey 1 
                                       and survey 2, respectively"),
                       tags$li("StdError_1, StdError_2 = Estimated standard error of estimated mean in survey 1 
                                       and survey 2, respectively"),
                       tags$li("MarginofError_1, MarginofError_2 = Margin of error of estimated mean, 
                                       representing difference between estimate and confidence bounds in survey 1 
                                       and survey 2, respectively"),
                       tags$li("LCBXXPct_1, LCBXXPct_2 = Lower confidence bound for indicator mean, where XX represents 
                                       the confidence level in survey 1 and survey 2, respectively"),
                       tags$li("UCBXXPct_1, UCBXXPct_2 = Upper confidence bound for indicator mean, where XX represents 
                                       the confidence level in survey 1 and survey 2, respectively"),
                       ),
               tags$ul(strong("Outputs for Continuous Analysis (Medians):"),
                       tags$li("All of the output variable names match those for Categorical Change Analysis (see above)"),
                       tags$li("The two categories for this output are Greater_Than_Median and Less_Than_Median, 
                               but the interpretations are the same")
                       )),

               bsCollapsePanel(title = h4(strong("Plot Categorical Estimates")),
                  tags$ol(
                    tags$li("Either run population estimates on categorical data either within the
                            app or import results into the app."),
                    tags$li("Variables in dataset must match those expected as output from
                            spsurvey::cat_analysis function:", strong("Type, Subpopulation, Indicator,
                            Category, Estimate.P, StdError.P, LCBXXPct.P, UCBXXPct.P, Estimate.U,
                            StdError.U,	LCBXXPct.U,	UCBXXPct.U, where XX represents the confidence
                                                                      level.")),
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
                Value, Estimate.P, StdError.P, LCBXXPct.P, UCBXXPct.P, Estimate.U,
                StdError.U,	LCBXXPct.U,	UCBXXPct.U, where XX represents the confidence level.")),
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

# Prepare Data ------------------------------------------------------------

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
            column(3,
              shiny::selectizeInput("siteVar", label="Select site variable",
                                    choices=NULL, multiple=FALSE) %>%
                helper(type = "inline", icon = 'exclamation', colour='#B72E16',
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
                                              choices=NULL, multiple=TRUE)),

              h5(p(strong("If ANY changes have been made to your choices, you MUST click the button to prepare data for analysis again!"), style="color:#B72E16"))

            ),
            # Set up type of variance to use in estimates: local or SRS
            column(3,
                   radioButtons('locvar',"Type of variance estimate to use (select one)",
                                choices = c('Local neighborhood variance (recommended, used for NARS,
                                            requires site coordinates)' = 'local',
                                            'Simple Random Sample (requires stratum but not site
                                            coordinates)' = 'SRS'),
                                select = 'local'),
                   # If local, user must select x and y coordinates and convert to Albers if in lat/long
                   conditionalPanel(condition = "input.locvar == 'local'",
                                    selectizeInput("coordxVar","Select the X coordinate variable
                                                   (required only for local neighborhood variance)",
                                                   choices=NULL, multiple=FALSE, selected=NULL),
                                    selectizeInput("coordyVar","Select the Y coordinate variable
                                                   (required only for local neighborhood variance)",
                                                   choices=NULL, multiple=FALSE, selected=NULL)

                                                      ),

                   # Select stratum if Simple Random Sample
                   selectizeInput("stratumVar","Select a categorical stratum variable if desired. May be used for either variance type.",
                                  choices=NULL, multiple=FALSE)
            )
         ),
          # Press button to subset data for analysis - must click here first
         column(4, actionButton("resetBtn", "Click to revert back to full dataset or change data display")),
         column(4, actionButton("subsetBtn", strong("Click HERE to prepare data for analysis"), style="color:#B72E16"), offset=2),
         hr(),
         br(),

          # Show a table of the data
          h4("Data for Analysis"),
          DT::dataTableOutput("contents")

      ),

# Run Population Estimates ------------------------------------------------


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
                              radioButtons("cdf_pct", "Show CDF, percentile, mean, or total results",
                                           choices = c(CDF = 'cdf', Percentiles = 'pct', Means = 'mean', Totals = 'total'),
                                           selected = 'pct')),
             conditionalPanel(condition="input.chboxYear==true",
                              selectizeInput('selYear', 'Select the year for analysis',
                                             choices=NULL, multiple=FALSE)),
             
             radioButtons('ciSize', 'Confidence level', 
                          choices = c('90%' = '90', '95%' = '95'),
                          selected = '95'),

             p("If the", strong("Run/Refresh Estimates"), "button is grayed out, return to the",
               strong("Prepare Data for Analysis"), "tab and click the button that says",
               strong("Click HERE to prepare data for analysis")),
             p("Note that if all values are very small, the results may appear as zeroes. Save
               and view output file to see the results with full digits."),
             # Once data are prepared, user needs to click to run estimates, or rerun estimates on refreshed data
             shinyjs::disabled(actionButton('runBtn', "Run/Refresh estimates")),
             hr(),
             # Click to download results into a comma-delimited file
             shinyjs::disabled(downloadButton("dwnldcsv","Save Results as .csv file")),
             shinyjs::disabled(downloadButton("popcallcsv", "Save version info and the R code used for analysis"))),
             # Show results here
             column(8,
                    h4("If output is not as expected, be sure you chose the correct ",
                       strong("Type of Analysis"), "(Categorical or Continuous) for your data",
                       style="color:#225D9D"),
                    h4("Warnings"),
                    DT::dataTableOutput("warnest"),

                    h4("Analysis Output"),
                    DT::dataTableOutput("popest")
                    )
          )

      ),

# Run Change Analysis -----------------------------------------------------


      tabPanel(title="Run Change Analysis", value='change',
               fluidRow(
                 h4("  ", "If a different set of response variables from those
                 used in the population estimates is desired,
                    return to the", strong("Prepare Data for Analysis"),
                        "tab to re-select variables. Then click the button to
                        prepare data for analysis again.",
                    style="color:#225D9D"),
                 column(3,
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

                        radioButtons('ciSize_chg', 'Confidence level', 
                                     choices = c('90%' = '90', '95%' = '95'),
                                     selected = '95'),
                 # Once data are prepared, user needs to click to run estimates, or rerun estimates on refreshed data
                 hr(),
                 p("If the", strong("Run/Refresh Estimates"), "button is grayed out, return to the",
                   strong("Prepare Data for Analysis"), "tab and click the button that says",
                   strong("Click HERE to prepare data for analysis")),
                 shinyjs::disabled(actionButton('chgBtn', "Run/Refresh estimates")),
                 hr(),
                 # Click to download results into a comma-delimited file
                 shinyjs::disabled(downloadButton("chgcsv","Save Change Results as .csv file")),
                 shinyjs::disabled(downloadButton("chgcallcsv", "Save version info and the R code used for analysis"))),
                 column(8,
                         h4("Warnings"),
                        DT::dataTableOutput("warnchg"),

                        h4("Change Analysis Output"),
                        DT::dataTableOutput("changes")
                        )

               )

          ),
# Trend Analysis ------------------------------
tabPanel(title="Run Trend Analysis", value='trend',
         fluidRow(
           h3("Use for categorical analysis only - Please note:"),
           tags$ul(
             tags$li("This is only designed for analysis of categorical 
             variables and may convert any numerical variable to categorical. 
             Any subsetting of data to align different years or cycles
             must be done either on the", strong("Prepare Data for Analysis"),
             "tab or before importing the dataset."),
             br(),
            tags$li("If a different set of 
             response variables from those
             used in the population or change estimates is desired,
             return to the", strong("Prepare Data for Analysis"),
             "tab to re-select variables. Then click the button to
             prepare data for analysis again."),
            br(),
            tags$li("If the", strong("Run/Refresh Estimates"), "button is grayed out, return to the",
             strong("Prepare Data for Analysis"), "tab and click the button that says",
             strong("Click HERE to prepare data for analysis"))
            ),
           column(3,
                  # Once data are prepared, user needs to click to run estimates, or rerun estimates on refreshed data
                  shinyjs::disabled(actionButton('trendBtn', "Run/Refresh estimates")),
                  hr(),
                  # Click to download results into a comma-delimited file
                  shinyjs::disabled(downloadButton("trendcsv","Save Trend Results as .csv file")),
                  shinyjs::disabled(downloadButton("trendcallcsv", "Save version info and the R code used for analysis"))),
           column(8,
                  h4("Warnings"),
                  DT::dataTableOutput("warntrend"),
                  
                  h4("Trend Analysis Output"),
                  DT::dataTableOutput("trends")
           )
           
         ) 
          ),


# Plot Data ---------------------------------------------------------------


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
                   selectInput(inputId = "Estimate",
                               label = HTML("<b>Select Estimate Type</b>"),
                               choices = c("Proportion Estimates" = "P Estimates", "Unit Estimates" = "U Estimates"),
                               multiple = FALSE,
                               width = "300px")  %>%
                     #Estimate Type helper
                     helper(type = "inline", 
                            icon = 'circle-question',
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
                     helper(type = "inline", icon = 'circle-question',
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
                   textInput("resource", "Define Resource Type/Unit", value = "",
                             width = "300px", placeholder = "Resource") %>%
                     #Resource Type helper
                     helper(type = "inline", icon = 'circle-question',
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
                                                    icon = 'circle-question',
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
                                                    icon = 'circle-question',
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
                                             icon = 'circle-question',
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
                                             icon = 'circle-question',
                                             title = "Subpopulation Comparison",
                                             content = c("Cycle through population groups to compare categorical estimates by condition and download plot, if desired. If there are no subpopulations, no subpopulation plots will appear."),
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
                              label=strong("Choose Cumulative Distribution Function (CDF) Estimate Dataset to Use:"),
                              choices=c("Upload Estimate Data File", "Current Estimate Data"),
                              selected = "Upload Estimate Data File",
                              inline=FALSE),
                 uiOutput("conui"),
                            selectInput(inputId = "Estimate_CDF",
                                        label = HTML("<b>Select Estimate Type</b>"),
                                        choices = c("Proportion Estimates" = "P Estimates_CDF", "Unit Estimates" = "U Estimates_CDF"),
                                        multiple = FALSE,
                                        width = "300px")  %>%
                              #Estimate Type helper
                              helper(type = "inline",
                                     icon = 'circle-question',
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
                                     icon = 'circle-question',
                                     title = "Resource Type",
                                     content = c("This input defines the plot axis label. Resource
                                                 Type is the resource evaluated in your design
                                                 (e.g., Stream Miles, Wetland Area, Coastline)."),
                                     size = "s", easyClose = TRUE, fade = TRUE),
                            actionButton("plotbtncon", strong("Plot Continuous Estimates"), icon=icon("chart-bar"))
                            ,width=4),#sidebarPanel
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
                                           icon = 'circle-question',
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
                                           icon = 'circle-question',
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
   ),
# Site Footer ----
includeHTML("www/footer.html")) # END fluidPage

# Begin Server ----
server <- function(input, output, session) {
  observe_helpers()

# Read in Data for Preparation --------------------------------------------


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
    shinyjs::disable('popcallcsv')
    shinyjs::enable('chgBtn')
    shinyjs::enable('runBtn')
    shinyjs::disable('chgcsv')
    shinyjs::disable('chgcallcsv')
    shinyjs::enable('trendBtn')
    shinyjs::disable('trendcsv')
    shinyjs::disable('trendcallcsv')
    if(input$disp == 'head'){
      output$contents <- DT::renderDataTable({head(dataOut())}, options = list(digits=5, rownames=F,
                                             scrollX=TRUE, scrollY=TRUE))
    }else{
      output$contents <- DT::renderDataTable({dataOut()}, options = list(digits=5, rownames=F,
                                     scrollX=TRUE, scrollY=TRUE))
    }
  })

  observeEvent(input$resetBtn, {
    if(input$disp == 'head'){
      output$contents <- DT::renderDataTable({head(dataIn())},
                                             options = list(digits=5, rownames=F,
                                                            scrollX=TRUE, scrollY=TRUE))
    }else{
      output$contents <- DT::renderDataTable({dataIn()}, options = list(digits=5, rownames=F,
                                                                        scrollX=TRUE, scrollY=TRUE))
    }
  })


  # Use current dataset to refresh dropdown list of variables.
  observe({
    vars <- colnames(dataIn())
    updateSelectizeInput(session, "weightVar", "Select weight variable", choices=vars)

    updateSelectizeInput(session, 'respVar', 'Select up to 10 response variables - All must be either categorical or numeric',
                         choices=vars, selected = NULL,
                         options = list(maxItems=10))
    updateSelectizeInput(session, 'coordxVar', "Select the X coordinate variable \n(required only
                         for local neighborhood variance)",
                         choices=vars, selected = NULL)
    updateSelectizeInput(session, 'coordyVar', "Select the Y coordinate variable\n(required only
                         for local neighborhood variance)",
                         choices=vars, selected = NULL)
    updateSelectizeInput(session, 'siteVar', 'Select site variable', choices=vars)
    updateSelectizeInput(session, 'subpopVar', 'Select up to 10 subpopulation variables \n(required if not
                         national estimates only)', choices=vars, selected=NULL,
                         options = list(maxItems=10))
    updateSelectizeInput(session, "stratumVar", "Select a categorical stratum variable if desired. May be used for either variance type.",
                         choices=c('None', vars), selected='None')
    updateSelectizeInput(session, "yearVar","Select year variable",
                         choices=c('', vars))

    updateSelectizeInput(session, "subvar", choices=vars)

    updateSelectizeInput(session, 'szwtVar', choices=vars)
  })

  observeEvent(input$subvar,{

      catchoices <- as.character(sort(unique(dataIn()[[input$subvar]])))

      updateSelectizeInput(session, 'subcat', choices = catchoices, selected=NULL, server=TRUE)

  })


# After Prepare Data for Analysis Button Clicked --------------------------


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

      # print(input$subpopVar)

    if(input$subpop==TRUE & is.null(input$subpopVar)){
      validate(!is.null(input$subpopVar), "Choose a subpopulation variable or
               uncheck the subpopulation box!")
    }

      if(input$subpop == TRUE & !is.null(input$subpopVar)){
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


# Run Change Estimates Code -----------------------------------------------


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
      if(input$chgCatCont=='chgCat'){

        validate(
          need(all('character' %in% lapply(chgIn[,input$respVar], class)),
               'At least one response variable is numeric data. Do you mean to run CONTINUOUS analysis?')
        )
      }else{

        validate(
          need(all('numeric' %in% lapply(chgIn[,input$respVar], class)),
               'At least one response variable is character. Do you mean to run CATEGORICAL analysis?')
        )
      }

      # Need to order by siteID, yearVar
      chgIn <- chgIn[order(chgIn[,input$yearVar],chgIn[,input$siteVar]),]

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

      if(input$locvar != 'local'){
        xcoord.in <- NULL
      }else{
        xcoord.in <- input$coordxVar
      }

      if(input$locvar != 'local'){
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
      
      if(input$ciSize_chg == '95'){
        conf.in = 95
      }else{
        conf.in = 90
      }

       #revisitWgt <- FALSE # NOT SURE WHAT THIS SHOULD BE SO ASSUME DEFAULT
       show_modal_spinner(spin = 'flower', text = 'This might take a while...please wait.')

        # if(input$repeatBox==TRUE){
          if(input$chgCatCont == 'chgCat'){
            chgOut <- change_analysis(dframe = chgIn, vars_cat = input$respVar,
                                      subpops=subpops.in, surveyID = surveyID,
                                      survey_names = survey_names,
                                      siteID = input$siteVar, weight = input$weightVar,
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      stratumID = stratum.in,
                                      vartype = vartype, All_Sites = all_sites,
                                      conf = conf.in)
            
            chgCallInfo <- paste0("Code used to run analysis: 
                                  \nchange_analysis(dframe = chgIn, vars_cat = c('", 
                                  paste(input$respVar, collapse = "','"), "')",  
                                  ", \nsubpops = ", 
                                  ifelse(is.null(subpops.in), "NULL", 
                                         paste0("c('", paste0(subpops.in, collapse = "','"), "')")),
                                  ", \nsurvey_names = c('", 
                                  paste(survey_names, collapse = "','"), "')",
                                  ", \nsite_ID = '", input$siteVar, 
                                  "', \nweight = '", input$weightVar,
                                  "', \nxcoord = ", 
                                  ifelse(is.null(xcoord.in), "NULL",
                                         paste0("'", xcoord.in, "'")), 
                                  ", \nycoord = ", 
                                  ifelse(is.null(ycoord.in), "NULL", 
                                         paste0("'", ycoord.in, "'")),
                                  ", \nstratumID = ", 
                                  ifelse(is.null(stratum.in),
                                         "NULL", paste0("'", stratum.in, "'")), 
                                  ", \nvartype = '", vartype, 
                                  "', \nAll_Sites = ", all_sites,
                                  ", \nconf = ", conf.in, ")\n", 
                                  "using ", R.version.string, 
                                  ", spsurvey v.", as.character(packageVersion("spsurvey")), 
                                  " in ", "NARS Population Estimate Calculation Tool (v. 2.3.0)")
            
          }else{
            chgOut <- change_analysis(dframe = chgIn, vars_cont = input$respVar, 
                                      test = ttype,
                                      subpops=subpops.in, surveyID = surveyID,
                                      survey_names = survey_names,
                                      siteID = input$siteVar, weight = input$weightVar,
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      stratumID = stratum.in,
                                      vartype = vartype, All_Sites = all_sites,
                                      conf = conf.in)
            
            chgCallInfo <- paste0("Code used to run analysis: 
                                  \nchange_analysis(dframe = chgIn, vars_cont = c('", 
                                  paste(input$respVar, collapse = "','"), "')",  
                                  ", test = '", eval(ttype), 
                                  "', \nsubpops = ", 
                                  ifelse(is.null(subpops.in), "NULL", 
                                         paste0("c('", paste0(subpops.in, collapse = "','"), "')")),
                                  ", \nsurvey_names = c('", 
                                  paste(survey_names, collapse = "','"), "')",
                                  ", \nsite_ID = '", input$siteVar, 
                                  "', \nweight = '", input$weightVar,
                                  "', \nxcoord = ", 
                                  ifelse(is.null(xcoord.in), "NULL",
                                         paste0("'", xcoord.in, "'")), 
                                  ", \nycoord = ", 
                                  ifelse(is.null(ycoord.in), "NULL", 
                                         paste0("'", ycoord.in, "'")),
                                  ", \nstratumID = ", 
                                  ifelse(is.null(stratum.in),
                                    "NULL", paste0("'", stratum.in, "'")), 
                                  ", \nvartype = '", vartype, 
                                  "', \nAll_Sites = ", all_sites,
                                  ", \nconf = ", conf.in, ")\n", 
                                  "using ", R.version.string, 
                                  ", spsurvey v.", as.character(packageVersion("spsurvey")), 
                                  " in ", "NARS Population Estimate Calculation Tool (v. 2.3.0)")
            
          }
      
      remove_modal_spinner()

    if(input$chgCatCont == 'chgCat'){
      chgOut.1 <- chgOut$catsum

      # Order the Category values only if all values fall within those below using factors
      if(all(unique(chgOut.1$Category %in% c('Good','GOOD','Low','LOW',
                                                           'Fair','FAIR','MODERATE','Moderate',
                                                           'Poor','POOR','High','HIGH',
                                                           'Very High','VERY HIGH','Not Assessed')))){
            chgOut.1$Category <- factor(chgOut.1$Category,
                                                 levels=c('Good','GOOD','Low','LOW',
                                                          'Fair','FAIR','MODERATE','Moderate',
                                                          'Poor','POOR','High','HIGH',
                                                          'Very High','VERY HIGH','Not Assessed'),
                                                 ordered=TRUE)
            chgOut.1$Category <- droplevels(chgOut.1$Category)

            chgOut.1 <- chgOut.1[with(chgOut.1, order(Type, Subpopulation, Indicator, Category)),]
        }

    }else{
      if(input$testType == 'mean'){
        chgOut.1 <- chgOut$contsum_mean
      }else{
        chgOut.1 <- chgOut$contsum_median
      }
    }

    if(exists('warn_df') && ncol(warn_df)>1){
      outdf <- list(chgOut=chgOut.1, warndf=warn_df, fxnCall = chgCallInfo)
    }else{
      outdf <- list(chgOut=chgOut.1, warndf=data.frame(warnings='none'),
                    fxnCall = chgCallInfo)
    }


  })
  # Use change output to create a table
  output$changes <- DT::renderDataTable({
    chgEst()[['chgOut']]
  }, options = list(scrollX=TRUE, scrollY=TRUE, rownames=F, searching=FALSE))

  output$warnchg <- DT::renderDataTable({
    chgEst()[['warndf']]
  }, options = list(scrollX=TRUE, scrollY=TRUE, rownames=F, searching=FALSE))

  
  
# Single Year Population Estimates ----------------------------------------


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

    # VALIDATION of variable types

    if(input$atype=='categ'){

      validate(
        need(all('character' %in% lapply(dfIn[,input$respVar], class)),
        'At least one response variable is numeric data. Do you mean to run CONTINUOUS analysis?')
      )
    }else{

      validate(
        need(all('numeric' %in% lapply(dfIn[,input$respVar], class)),
             'At least one response variable is character. Do you mean to run CATEGORICAL analysis?')
      )
    }

    show_modal_spinner(spin = 'flower', text = 'This might take a while...please wait.')

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

      if(input$locvar != 'local'){
        xcoord.in <- NULL
      }else{
        xcoord.in <- input$coordxVar
      }

      if(input$locvar != 'local'){
        ycoord.in <- NULL
      }else{
        ycoord.in <- input$coordyVar
      }
    
      if(input$ciSize == '95'){
        conf.in = 95
      }else{
        conf.in = 90
      }

       # User selected categorical analysis, set up cat.analysis function depending on previous selections
      if(input$atype=='categ'){
            estOut <- cat_analysis(dframe = dfIn, siteID=input$siteVar, subpops=subpops.in,
                                   vars=input$respVar, weight = input$weightVar,
                                   xcoord = xcoord.in, ycoord = ycoord.in,
                                   # sizeweight = sizeweight.in, sweight = sweight.in,
                                   stratumID = stratum.in, vartype=vartype,
                                   conf = conf.in, All_Sites = all_sites)
            
            popCallInfo <- paste0("Code used to run analysis: 
                                  \ncat_analysis(dframe = dfIn, siteID = '", 
                                  input$siteVar, "', subpops = ",
                                  ifelse(is.null(subpops.in), "NULL", 
                                         paste0("c('", paste0(subpops.in, collapse = "','"), "')")),
                                  ", \nvars = c('", 
                                  paste(input$respVar, collapse = "','"), "')",  
                                  ", weight = '", input$weightVar, 
                                  "', \nxcoord = ", 
                                  ifelse(is.null(xcoord.in), "NULL",
                                         paste0("'", xcoord.in, "'")), 
                                  ", \nycoord = ", 
                                  ifelse(is.null(ycoord.in), "NULL", 
                                         paste0("'", ycoord.in, "'")),
                                  ", \nstratumID = ", 
                                  ifelse(is.null(stratum.in),
                                         "NULL", paste0("'", stratum.in, "'")), 
                                  ", \nvartype = '", vartype, 
                                  "', \nAll_Sites = ", all_sites,
                                  ", \nconf = ", conf.in, ")\n", 
                                  "using ", R.version.string, 
                                  ", spsurvey v.", as.character(packageVersion("spsurvey")), 
                                  " in ", "NARS Population Estimate Calculation Tool (v. 2.3.0)")

      }else{

            if(input$cdf_pct=='cdf'){ # Produce CDFs
              estOut <- cont_analysis(dframe = dfIn, siteID=input$siteVar, subpops=subpops.in,
                                      vars=input$respVar, weight = input$weightVar,
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      # sizeweight = sizeweight.in, sweight = sweight.in,
                                      stratumID = stratum.in, vartype=vartype,
                                      conf = conf.in, All_Sites = all_sites,
                                      statistics = 'CDF')$CDF
              
              popCallInfo <- paste0("Code used to run analysis: 
                                  \ncont_analysis(dframe = dfIn, siteID = '", 
                     input$siteVar, "', subpops = ",
                     ifelse(is.null(subpops.in), "NULL", 
                            paste0("c('", paste0(subpops.in, collapse = "','"), "')")),
                     ", \nvars = c('", 
                     paste(input$respVar, collapse = "','"), "')",  
                     ", weight = '", input$weightVar, 
                     "', \nxcoord = ", 
                     ifelse(is.null(xcoord.in), "NULL",
                            paste0("'", xcoord.in, "'")), 
                     ", \nycoord = ", 
                     ifelse(is.null(ycoord.in), "NULL", 
                            paste0("'", ycoord.in, "'")),
                     ", \nstratumID = ", 
                     ifelse(is.null(stratum.in),
                            "NULL", paste0("'", stratum.in, "'")), 
                     ", \nvartype = '", vartype, 
                     "', \nAll_Sites = ", all_sites,
                     ", \nconf = ", conf.in, 
                     ", statistics = 'CDF')$CDF ", 
                     "\nusing ", R.version.string, 
                     ", spsurvey v.", as.character(packageVersion("spsurvey")), 
                     " in ", "NARS Population Estimate Calculation Tool (v. 2.3.0)")

            }else if(input$cdf_pct=='pct'){ # Just produce percentiles
              estOut <- cont_analysis(dframe = dfIn, siteID=input$siteVar, subpops=subpops.in,
                                      vars=input$respVar, weight = input$weightVar,
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      # sizeweight = sizeweight.in, sweight = sweight.in,
                                      stratumID = stratum.in, vartype=vartype,
                                      conf = conf.in, All_Sites = all_sites,
                                      statistics = c('Pct'))$Pct
              
              popCallInfo <- paste0("Code used to run analysis: 
                                  \ncont_analysis(dframe = dfIn, siteID = '", 
                                    input$siteVar, "', subpops = ",
                                    ifelse(is.null(subpops.in), "NULL", 
                                           paste0("c('", paste0(subpops.in, collapse = "','"), "')")),
                                    ", \nvars = c('", 
                                    paste(input$respVar, collapse = "','"), "')",  
                                    ", weight = '", input$weightVar, 
                                    "', \nxcoord = ", 
                                    ifelse(is.null(xcoord.in), "NULL",
                                           paste0("'", xcoord.in, "'")), 
                                    ", \nycoord = ", 
                                    ifelse(is.null(ycoord.in), "NULL", 
                                           paste0("'", ycoord.in, "'")),
                                    ", \nstratumID = ", 
                                    ifelse(is.null(stratum.in),
                                           "NULL", paste0("'", stratum.in, "'")), 
                                    ", \nvartype = '", vartype, 
                                    "', \nAll_Sites = ", all_sites,
                                    ", \nconf = ", conf.in, 
                                    ", statistics = 'Pct')$Pct ", 
                                    "\nusing ", R.version.string, 
                                    ", spsurvey v.", as.character(packageVersion("spsurvey")), 
                                    " in ", "NARS Population Estimate Calculation Tool (v. 2.3.0)")
            }else if(input$cdf_pct=='mean'){
              estOut <- cont_analysis(dframe = dfIn, siteID=input$siteVar, subpops=subpops.in,
                                      vars=input$respVar, weight = input$weightVar,
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      # sizeweight = sizeweight.in, sweight = sweight.in,
                                      stratumID = stratum.in, vartype=vartype,
                                      conf = conf.in, All_Sites = all_sites,
                                      statistics = c('Mean'))$Mean
              
              popCallInfo <- paste0("Code used to run analysis: 
                                  \ncont_analysis(dframe = dfIn, siteID = '", 
                                    input$siteVar, "', subpops = ",
                                    ifelse(is.null(subpops.in), "NULL", 
                                           paste0("c('", paste0(subpops.in, collapse = "','"), "')")),
                                    ", \nvars = c('", 
                                    paste(input$respVar, collapse = "','"), "')",  
                                    ", weight = '", input$weightVar, 
                                    "', \nxcoord = ", 
                                    ifelse(is.null(xcoord.in), "NULL",
                                           paste0("'", xcoord.in, "'")), 
                                    ", \nycoord = ", 
                                    ifelse(is.null(ycoord.in), "NULL", 
                                           paste0("'", ycoord.in, "'")),
                                    ", \nstratumID = ", 
                                    ifelse(is.null(stratum.in),
                                           "NULL", paste0("'", stratum.in, "'")), 
                                    ", \nvartype = '", vartype, 
                                    "', \nAll_Sites = ", all_sites,
                                    ", \nconf = ", conf.in, 
                                    ", statistics = 'Mean')$Mean ", 
                                    "\nusing ", R.version.string, 
                                    ", spsurvey v.", as.character(packageVersion("spsurvey")), 
                                    " in ", "NARS Population Estimate Calculation Tool (v. 2.3.0)")
            }else{
              estOut <- cont_analysis(dframe = dfIn, siteID=input$siteVar, subpops=subpops.in,
                                      vars=input$respVar, weight = input$weightVar,
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      # sizeweight = sizeweight.in, sweight = sweight.in,
                                      stratumID = stratum.in, vartype=vartype,
                                      conf = conf.in, All_Sites = all_sites,
                                      statistics = c('Total'))$Total
              
              popCallInfo <- paste0("Code used to run analysis: 
                                  \ncont_analysis(dframe = dfIn, siteID = '", 
                                    input$siteVar, "', subpops = ",
                                    ifelse(is.null(subpops.in), "NULL", 
                                           paste0("c('", paste0(subpops.in, collapse = "','"), "')")),
                                    ", \nvars = c('", 
                                    paste(input$respVar, collapse = "','"), "')",  
                                    ", weight = '", input$weightVar, 
                                    "', \nxcoord = ", 
                                    ifelse(is.null(xcoord.in), "NULL",
                                           paste0("'", xcoord.in, "'")), 
                                    ", \nycoord = ", 
                                    ifelse(is.null(ycoord.in), "NULL", 
                                           paste0("'", ycoord.in, "'")),
                                    ", \nstratumID = ", 
                                    ifelse(is.null(stratum.in),
                                           "NULL", paste0("'", stratum.in, "'")), 
                                    ", \nvartype = '", vartype, 
                                    "', \nAll_Sites = ", all_sites,
                                    ", \nconf = ", conf.in, 
                                    ", statistics = 'Total')$Total ", 
                                    "\nusing ", R.version.string, 
                                    ", spsurvey v.", as.character(packageVersion("spsurvey")), 
                                    " in ", "NARS Population Estimate Calculation Tool (v. 2.3.0)")
            }
          # }
      }

      remove_modal_spinner()

    if(exists('warn_df') && ncol(warn_df)>1){
      outdf <- list(estOut=estOut, warndf=warn_df,
                    popCall = popCallInfo)
    }else{
      outdf <- list(estOut=estOut, warndf=data.frame(warnings='none'),
                    popCall = popCallInfo)
    }


  })


  # Output the population estimates to a table
  output$popest <- DT::renderDataTable({
    dataEst()[['estOut']]
  }, options = list(scrollX=TRUE, scrollY=TRUE, rownames=F, searching=FALSE))

  output$warnest <- DT::renderDataTable({
    dataEst()[['warndf']]
  }, options = list(scrollX=TRUE, scrollY=TRUE, rownames=F, searching=FALSE))

  
# Run Trend Analysis ------------------------------
  trendEst <- eventReactive(input$trendBtn,{
    
    if(exists("warn_df") && is.data.frame(get("warn_df"))){
      rm("warn_df", envir=.GlobalEnv)
    }
    
    trendIn <- dataOut()
    
    # Check for duplicate rows for siteID
    freqSiteTrend <- as.data.frame(table(siteID = trendIn[,input$siteVar],Year = trendIn[,input$yearVar]))
    
    validate(
      need(nrow(subset(freqSiteTrend, Freq>1))==0,
           paste("There are", nrow(subset(freqSiteTrend, Freq>1)),
                 "duplicated sites in this dataset within years or cycles.
                   Only one row per site-design cycle combination is permitted in the input data."))
    )
    
    validate(
      need(all('character' %in% lapply(trendIn[,input$respVar], class)),
           'At least one response variable is numeric data. Please select only 
           categorical variables for trend analysis.')
    )
    
    # Check yearVar to see if it is numeric or can easily be converted to numeric
    # If not, try to extract first year from year range
    # If this can't be done, trigger an error and ask user to create a 
    # numeric year variable
    trendIn <- as.data.frame(trendIn)
    trendIn$Year <- as.integer(trendIn[, input$yearVar])
    
    validate(
      need(all(!is.na(trendIn$Year)),
           'The year variable cannot be converted to numeric. Please provide a 
           single numeric value for each row in the column used as year.')
    )
    
    show_modal_spinner(spin = 'flower', text = 'This might take a while...please wait.')
    
    if(input$natpop==TRUE & input$subpop==TRUE){
      all_sites <- TRUE
    }else{
      all_sites <- FALSE
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
    
    if(input$locvar != 'local'){
      xcoord.in <- NULL
    }else{
      xcoord.in <- input$coordxVar
    }
    
    if(input$locvar != 'local'){
      ycoord.in <- NULL
    }else{
      ycoord.in <- input$coordyVar
    }
    
    trendOut <- trend_analysis(
      dframe = trendIn,
      vars_cat = input$respVar,
      siteID=input$siteVar, 
      yearID = 'Year',
      subpops=subpops.in,
      weight = input$weightVar,
      xcoord = xcoord.in, 
      ycoord = ycoord.in,
      stratumID = stratum.in, 
      vartype = vartype,
      conf = 95, 
      All_Sites = all_sites,
      model_cat = 'SLR'
    )$catsum
    
    # Create table of sample size by indicator and category and subpopulation
    trendCts <- data.frame() # SORT OF WORKS BUT NEED to REVISE COLUMN NAMES AND ADD VARIABLES WITH RESPONSE AND SUBPOP NAMES
    if(input$subpop==TRUE){
      for(i in 1:length(input$subpopVar)){
        for(j in 1:length(input$respVar)){
          tempdf <- group_by(trendIn, eval(as.name(input$subpopVar[i])),
                   eval(as.name(input$respVar[j]))) |> 
            count(Year, 
                  name = 'Count',
                  sort = TRUE) |> 
            ungroup() |> 
            rename(c(Subpopulation = 'eval(as.name(input$subpopVar[i]))',
                   Category = 'eval(as.name(input$respVar[j]))')) |> 
            mutate(Indicator = input$respVar[j],
                   Type = input$subpopVar[i])
          # tempin <- subset(trendIn, select(input$subpopVar[i], input$respVar[j]))
          # tempdf <- count(trendIn, eval(as.name(input$yearVar)), 
          #                 name = "Year",
          #                          sort = TRUE)
          
          trendCts <- bind_rows(trendCts, tempdf) |> 
            arrange(Indicator, Type, Subpopulation, Category, Year)
        }
  
      }
    }else{
        for(i in 1:length(input$respVar)){
          tempdf <- group_by(trendIn, eval(as.name(input$respVar[i]))) |> 
            count(Year, 
                  name = 'Count',
                  sort = TRUE) |> 
            ungroup() |> 
            rename(c(Category = 'eval(as.name(input$respVar[i]))')) |> 
            mutate(Indicator = input$respVar[i])
            trendCts <- bind_rows(trendCts, tempdf) |> 
              arrange(Indicator, Category, Year)
          }
        }
    

    trendCallInfo <- paste0("Code used to run analysis: 
                            \ntrend_analysis(\ndframe = trendIn,\nvars_cat = c('", 
                          paste(input$respVar, collapse = "','"), "')",  
                          ", \nsite_ID = '", input$siteVar,
                          "', \nyearID = '", input$yearVar,
                          "', \nsubpops = ", 
                          ifelse(is.null(subpops.in), "NULL", 
                                 paste0("c('", paste0(subpops.in, collapse = "','"), "')")),
                          ", \nweight = '", input$weightVar,
                          "', \nxcoord = ", 
                          ifelse(is.null(xcoord.in), "NULL",
                                 paste0("'", xcoord.in, "'")), 
                          ", \nycoord = ", 
                          ifelse(is.null(ycoord.in), "NULL", 
                                 paste0("'", ycoord.in, "'")),
                          ", \nstratumID = ", 
                          ifelse(is.null(stratum.in),
                                 "NULL", paste0("'", stratum.in, "'")), 
                          ", \nvartype = '", vartype, 
                          "', \nAll_Sites = ", all_sites,
                          ", \nconf = 95)\n", 
                          "using ", R.version.string, 
                          ", spsurvey v.", as.character(packageVersion("spsurvey")), 
                          " in ", "NARS Population Estimate Calculation Tool (v. 2.3.0)")
    
    remove_modal_spinner()
    
    if(exists('warn_df') && ncol(warn_df)>1){
      outdf <- list(trendOut = trendOut, 
                    warndf = warn_df,
                    trendCall = trendCallInfo)
    }else{
      outdf <- list(trendOut = trendOut, 
                    warndf = data.frame(warnings='none'),
                    trendCall = trendCallInfo)
    } 
  })
  
  # Use change output to create a table
  output$trends <- DT::renderDataTable({
    trendEst()[['trendOut']]
  }, options = list(scrollX=TRUE, scrollY=TRUE, rownames=F, searching=FALSE))
  
  output$warntrend <- DT::renderDataTable({
    trendEst()[['warndf']]
  }, options = list(scrollX=TRUE, scrollY=TRUE, rownames=F, searching=FALSE))
  

# Download Analysis Results -----------------------------------------------


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
        }else if(input$cdf_pct=='pct'){
          paste("Continuous_Percentiles_Output_",Sys.Date(), ".csv", sep = "")
        }else if(input$cdf_pct=='mean'){
          paste("Continuous_Means_Output_", Sys.Date(), ".csv", sep="")
        }else{
          paste("Continuous_Totals_Output_", Sys.Date(), ".csv", sep='')
        }
      }
    },
    content = function(file) {
      write.csv(dataEst()[['estOut']], file, row.names = FALSE)
    }
  )
  
  observe({shinyjs::toggleState('popcallcsv',length(dataEst()[['estOut']])!=0)})
  # Name output file based on type of analysis selected and write to comma-delimited file
  output$popcallcsv <- downloadHandler(
    filename = paste0("NARSPopEst_PopulationEstimate_Code_", Sys.Date(), ".txt"),
    content = function(file) {
      writeLines(dataEst()[['popCall']], file)
    }
  )

  # Only enable download button once population estimates are produced
  observe({shinyjs::toggleState('chgcsv', length(chgEst()[['chgOut']])!=0)})
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
    
    # Only enable download button once population estimates are produced
  observe({shinyjs::toggleState('chgcallcsv', length(chgEst()[['chgOut']])!=0)})
    # Name output file based on type of analysis selected and write to comma-delimited file
  output$chgcallcsv <- downloadHandler(
    filename = paste0("NARSPopEst_ChangeAnalysis_Code_", Sys.Date(), ".txt"),
    content = function(file) {
      writeLines(chgEst()[['fxnCall']], file)
    }
  )   

  observe({shinyjs::toggleState('trendcsv', length(trendEst()[['trendOut']])!=0)})
  # Name output file based on type of analysis selected and write to comma-delimited file
  output$trendcsv <- downloadHandler(
    filename = function() {
        paste("Trend_Categ_Est_Output_",Sys.Date(), ".csv", sep = "")
      },
    content = function(file) {
      write.csv(trendEst()[['trendOut']], file, row.names = FALSE)
    }
  )
  
  # Only enable download button once population estimates are produced
  observe({shinyjs::toggleState('trendcallcsv', length(trendEst()[['trendOut']])!=0)})
  # Name output file based on type of analysis selected and write to comma-delimited file
  output$trendcallcsv <- downloadHandler(
    filename = paste0("NARSPopEst_TrendAnalysis_Code_", Sys.Date(), ".txt"),
    content = function(file) {
      writeLines(trendEst()[['trendCall']], file)
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
             icon = 'circle-question',
             title = "Categorical Estimate File",
             content = paste("Choose a file with the same output which the
                                     spsurvey package", strong("cat_analysis()"),
                             "function renders. If the
                                                 dataset is missing required variables, no
                                                 selections will show up in the dropdown menu.
                                                 The expected and required variables are:", strong("Type,
                                                 Indicator, Subpopulation, Category, NResp, Estimate.P,
                                                 StdError.P, LCBXXPct.P, UCBXXPct.P, Estimate.U,
                                                 StdError.U, LCBXXPct.U, UCBXXPct.U, where XX is the
                                                 confidence level (90 or 95 from the analysis part of
                                                 this application)")),
             size = "s", easyClose = TRUE, fade = TRUE)
  })

  plotDataset <- eventReactive(c(input$userinput, input$runBtn, input$catinput), {
    if(input$catinput == "Current Estimate Data" && input$atype == 'categ') {

      dataEst()[['estOut']]

    } else {
      cival <- str_extract(colnames(userEst()), '[:digit:][:digit:]') %>%
        unique()
      
      cival <- subset(cival, !is.na(cival))
      
      validate(need(length(cival)==1, message = 'More than one confidence level included. Must only have values from a single confidence level.'))
      
      necVars <- c('Type', 'Indicator', 'Subpopulation', 'Category', 'Estimate.P',
                   'StdError.P', paste0('LCB', cival, 'Pct.P'), 
                   paste0('UCB', cival, 'Pct.P'), 'Estimate.U',
                   'StdError.U', paste0('LCB', cival, 'Pct.U'), 
                   paste0('UCB', cival, 'Pct.U'))

      validate(need(all(necVars %in% colnames(userEst())),
                    message = "Dataset does not include all variables in standardized output from spsurvey."))

      userEst <- userEst()
      # print(colnames(userEst))
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

    cival <- str_extract(colnames(plotDataset()), '[:digit:][:digit:]') %>%
      unique()
    
    cival <- subset(cival, !is.na(cival))

    if (input$Estimate == "P Estimates") {
      Dataset <- plotDataset()

      Dataset <- subset(Dataset, select = c('Type', 'Subpopulation', 'Indicator', 'Category',
                                            'Estimate.P', 'StdError.P', paste0('LCB', cival, 'Pct.P'),
                                            paste0('UCB', cival, 'Pct.P')))

      names(Dataset)[names(Dataset) == paste0("LCB", cival, "Pct.P")] <- "LCB"
      names(Dataset)[names(Dataset) == paste0("UCB", cival, "Pct.P")] <- "UCB"
      
      Dataset$LCB <- with(Dataset, ifelse(LCB < 0, 0, LCB))
      Dataset$UCB <- with(Dataset, ifelse(UCB > 100, 100, UCB))
      Dataset$Estimate.P <- with(Dataset, round(Estimate.P, 0))
      Dataset$UCB <- with(Dataset, round(UCB, 0))
      Dataset$LCB <- with(Dataset, round(LCB, 0))


      names(Dataset)[names(Dataset) == "Estimate.P"] <- "Estimate"
      names(Dataset)[names(Dataset) == "StdError.P"] <- "StdError"

    } else {
      Dataset <- plotDataset()

      Dataset <- subset(Dataset, select = c('Type', 'Subpopulation', 'Indicator', 'Category',
                                            'Estimate.U', 'StdError.U', paste0('LCB', cival, 'Pct.U'),
                                            paste0('UCB', cival, 'Pct.U')))
      names(Dataset)[names(Dataset) == "Estimate.U"] <- "Estimate"
      names(Dataset)[names(Dataset) == "StdError.U"] <- "StdError"
      names(Dataset)[names(Dataset) == paste0("LCB", cival, "Pct.U")] <-"LCB"
      names(Dataset)[names(Dataset) == paste0("UCB", cival, "Pct.U")] <- "UCB"

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
      geom_errorbar(aes(ymin = LCB, ymax = UCB, color = Category),
                    linewidth=2, width=0) +
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
      P1 <- P1 + geom_text(aes(label=paste(format(LCB),"%",
                                           sep=""), y=LCB), hjust = 1.1, size = 3.5, fontface = "bold",
                           color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15)) +
        geom_text(aes(label=paste(format(UCB),"%",
                                  sep=""), y=UCB), hjust = -.15,size = 3.5, fontface = "bold",
                  color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15))
    }

    if (input$indconlim == TRUE && input$Estimate == "U Estimates") {
      ylim <- max(popest$UCB95) * 1.1

      P1 <- P1 + geom_text(aes(label = format(round(LCB), big.mark = ","), y=LCB),
                           hjust = 1.1, size = 3.5, fontface = "bold", color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15)) +
        geom_text(aes(label = format(round(UCB), big.mark = ","), y=UCB),
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

    cival <- str_extract(colnames(plotDataset()), '[:digit:][:digit:]') %>%
      unique()
    
    cival <- subset(cival, !is.na(cival))
    

    if (input$Estimate == "P Estimates") {
      Dataset <- plotDataset()

      Dataset <- subset(Dataset, select = c('Type', 'Subpopulation', 'Indicator', 'Category',
                                            'Estimate.P', 'StdError.P', 
                                            paste0('LCB', cival, 'Pct.P'),
                                            paste0('UCB', cival, 'Pct.P'))) 
      names(Dataset)[names(Dataset) == paste0('LCB', cival, 'Pct.P')] <- "LCB"
      names(Dataset)[names(Dataset) == paste0('UCB', cival, 'Pct.P')] <- "UCB"
      
      Dataset$LCB <- with(Dataset, ifelse(LCB < 0, 0, LCB))
      Dataset$UCB <- with(Dataset, ifelse(UCB > 100, 100, UCB))
      Dataset$Estimate.P <- with(Dataset, round(Estimate.P, 0))
      Dataset$UCB <- with(Dataset, round(UCB, 0))
      Dataset$LCB <- with(Dataset, round(LCB, 0))


      names(Dataset)[names(Dataset) == "Estimate.P"] <- "Estimate"
      names(Dataset)[names(Dataset) == "StdError.P"] <- "StdError"
      

    } else {
      Dataset <- plotDataset()

      Dataset <- subset(Dataset, select = c('Type', 'Subpopulation', 'Indicator', 'Category',
                                            'Estimate.U', 'StdError.U', 
                                            paste0('LCB', cival, 'Pct.U'),
                                            paste0('UCB', cival, 'Pct.U'))) 
      
      names(Dataset)[names(Dataset) == paste0('LCB', cival, 'Pct.U')] <- "LCB"
      names(Dataset)[names(Dataset) == paste0('UCB', cival, 'Pct.U')] <- "UCB"
      
      names(Dataset)[names(Dataset) == "Estimate.U"] <- "Estimate"
      names(Dataset)[names(Dataset) == "StdError.U"] <- "StdError"
      
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
      geom_errorbar(aes(ymin = LCB, ymax = UCB, color = Category), linewidth=2, width=0) +
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
      P2 <- P2 + geom_text(aes(label=paste(format(LCB),"%",
                                           sep=""), y=LCB), hjust = 1.1, size = 3.5,
                           fontface = "bold",
                           color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15)) +
        geom_text(aes(label=paste(format(UCB),"%",
                                  sep=""), y=UCB), hjust = -.15, size = 3.5, fontface = "bold",
                  color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15))
    }

    if (input$subconlim == TRUE && input$Estimate == "U Estimates") {
      ylim <- max(popest2$UCB95) * 1.1

      P2 <- P2 + geom_text(aes(label = format(round(LCB), big.mark = ","), y=LCB),
                           hjust = 1.1, size = 3.5, fontface = "bold", color = "#4D4D4D",
                           family="sans", position = position_nudge(x = 0.15)) +
        geom_text(aes(label = format(round(UCB), big.mark = ","), y=UCB),
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

    if(length(popest$Subpopulation) < 4){
      calcheight <- 120 * length(popest$Subpopulation)
    }else{
      calcheight <- 70 * length(popest$Subpopulation)
    }
  })

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
             icon = 'circle-question',
             title = "CDF Estimate File",
             content = paste("Choose a file with the same output which the
                                     spsurvey package", strong("cont_analysis()"),
                             "function renders. If the
                                                 dataset is missing required variables, no
                                                 selections will show up in the dropdown menu.
                                     The expected and required variables are:", strong("Type,
                                     Subpopulation, Indicator, Value, Estimate.P, Estimate.U,
                                     StdError.P, StdError.U, LCBXXPct.P, UCBXXPct.P, Estimate.U,
                                                 StdError.U, LCBXXPct.U, UCBXXPct.U, where XX is the
                                                 confidence level (90 or 95 from the analysis part of
                                                 this application)")),
             size = "s", easyClose = TRUE, fade = TRUE)
  })


  CDFDataset <- eventReactive(c(input$ConCDFinput, input$runBtn, input$coninput), {

    if(input$coninput == "Current Estimate Data" && input$cdf_pct=='cdf') {
      dataEst()[['estOut']]
    } else {
      cival <- str_extract(colnames(userCDFEst()), '[:digit:][:digit:]') %>%
        unique()
      
      cival <- subset(cival, !is.na(cival))
      
      validate(need(length(cival)==1, message = 'More than one confidence level included. Must only have values from a single confidence level.'))
      
      necVars <- c('Type', 'Indicator', 'Subpopulation', 'Value', 'Estimate.P',
                   'StdError.P', paste0('LCB', cival, 'Pct.P'), 
                   paste0('UCB', cival, 'Pct.P'), 'Estimate.U',
                   'StdError.U', paste0('LCB', cival, 'Pct.U'), 
                   paste0('UCB', cival, 'Pct.U'))
      
      validate(need(all(necVars %in% colnames(userCDFEst())),
                    message = "Dataset does not include all variables in standardized output from spsurvey."))
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
    
    cival <- str_extract(colnames(CDFDataset()), '[:digit:][:digit:]') %>%
      unique()
    
    cival <- subset(cival, !is.na(cival))
    
    req(input$plotbtncon, input$SubPop_Con)
    necVars <- c('Type', 'Subpopulation', 'Indicator', 'Value',
                 'Estimate.P', 'Estimate.U',
                 'StdError.P', 'StdError.U', 
                 paste0('LCB', cival, 'Pct.P'), 
                 paste0('UCB', cival, 'Pct.P'), 
                 paste0('LCB', cival, 'Pct.U'), 
                 paste0('UCB', cival, 'Pct.U'))

    validate(need(all(necVars %in% colnames(CDFDataset())),
                  message = "Dataset does not include all variables in standardized output from spsurvey."))

    CDFDataset <- CDFDataset()

    if (input$Estimate_CDF == "P Estimates_CDF") {
      names(CDFDataset)[names(CDFDataset) == "Estimate.P"] <- "Estimate"
      names(CDFDataset)[names(CDFDataset) == "StdError.P"] <- "StdError"
      names(CDFDataset)[names(CDFDataset) == paste0("LCB", cival, "Pct.P")] <- "LCB"
      names(CDFDataset)[names(CDFDataset) == paste0("UCB", cival, "Pct.P")] <- "UCB"
      CDFDataset <- subset(CDFDataset, select = c('Type', 'Subpopulation', 'Indicator', 'Value', 'Estimate', 'StdError', 'LCB', 'UCB'))
    } else {
      names(CDFDataset)[names(CDFDataset) == "Estimate.U"] <- "Estimate"
      names(CDFDataset)[names(CDFDataset) == "StdError.U"] <- "StdError"
      names(CDFDataset)[names(CDFDataset) == paste0("LCB", cival, "Pct.U")] <- "LCB"
      names(CDFDataset)[names(CDFDataset) == paste0("UCB", cival, "Pct.U")] <- "UCB"
      CDFDataset <- subset(CDFDataset, select = c('Type', 'Subpopulation', 'Indicator', 'Value', 'Estimate', 'StdError', 'LCB', 'UCB'))
    }

    CDFDataset <- subset(CDFDataset, Indicator == input$Ind_Con & Subpopulation %in% input$SubPop_Con)
    CDFDataset$Subpopulation <-factor(CDFDataset$Subpopulation, levels = rev(unique(CDFDataset$Subpopulation)))

    g <- ggplot(CDFDataset, aes(y=Estimate, x=Value, color = Subpopulation, fill = Subpopulation)) +
      geom_step(linewidth=1) +
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
      g <- g + geom_vline(xintercept = input$Thresh, color = "red", 
                          linewidth = 1, linetype = "longdash")
    }
    if (input$conflim == TRUE) {
      g <- g + geom_ribbon(aes(ymin = LCB, ymax = UCB, fill = Subpopulation), alpha = 0.2,
                           colour = "transparent", show.legend = FALSE)
    }
    if (input$log == TRUE) {      
      g <-  g + scale_x_continuous(trans='log10') +
        annotation_logticks(sides='b')         
    }
    

    g
  })

  Dist_plot <- reactive({
    req(input$plotbtncon)
    
    cival <- str_extract(colnames(CDFDataset()), '[:digit:][:digit:]') %>%
      unique()
    
    cival <- subset(cival, !is.na(cival))
    
    CDFDataset <- CDFDataset()
    if (input$Estimate_CDF == "P Estimates_CDF") {

      names(CDFDataset)[names(CDFDataset) == "Estimate.P"] <- "Estimate"
      names(CDFDataset)[names(CDFDataset) == "StdError.P"] <- "StdError"
      names(CDFDataset)[names(CDFDataset) == paste0("LCB", cival, "Pct.P")] <- "LCB"
      names(CDFDataset)[names(CDFDataset) == paste0("UCB", cival, "Pct.P")] <- "UCB"
      CDFDataset <- subset(CDFDataset, select = c('Type', 'Subpopulation', 'Indicator', 'Value', 'Estimate', 'StdError', 'LCB', 'UCB'))
    } else {

      names(CDFDataset)[names(CDFDataset) == "Estimate.U"] <- "Estimate"
      names(CDFDataset)[names(CDFDataset) == "StdError.U"] <- "StdError"
      names(CDFDataset)[names(CDFDataset) == paste0("LCB", cival, "Pct.U")] <- "LCB"
      names(CDFDataset)[names(CDFDataset) == paste0("UCB", cival, "Pct.U")] <- "UCB"
      CDFDataset <- subset(CDFDataset, select = c('Type', 'Subpopulation', 'Indicator', 'Value', 'Estimate', 'StdError', 'LCB', 'UCB'))
    }

    CDFDataset <- subset(CDFDataset, Indicator == input$Ind_Con & Subpopulation %in% input$SubPop_Con)
    #CDFDataset <- with(CDFDataset, reorder(Subpopulation, Estimate, decreasing=FALSE))
    CDFDataset$Subpopulation <-factor(CDFDataset$Subpopulation, levels = rev(unique(CDFDataset$Subpopulation)))
    #CDFDataset <- suborder[order(CDFDataset$Estimate),]
    # CDFDataset$Subpopulation <- forcats::fct_reorder(CDFDataset$Subpopulation, CDFDataset$Estimate)

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
      g + geom_density_ridges(scale = 100000, jittered_points = TRUE,
                              position = position_points_jitter(width = 0.5, height = 0),
                              point_shape = "|", point_size = 3,
                              quantile_lines = TRUE, alpha=0.8)
    } else if (length(input$SubPop_Con) == 1)  {
      g + geom_density_ridges(scale = 50, jittered_points = TRUE,
                              position = position_points_jitter(width = 0.5, height = 0),
                              point_shape = "|", point_size = 3,
                              quantile_lines = TRUE, alpha=0.8)
    } else {
      g + geom_density_ridges(scale = 1, rel_min_height = 0.03, jittered_points = TRUE,
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

    if (length(input$SubPop_Con) < 3) {
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
  if(!dir.exists('/home/vcap/app')) {
    session$onSessionEnded(function() {
        # rm(warn_df,envir=.GlobalEnv)
       
      stopApp()
    })
  }

}

# Run the application
shinyApp(ui = ui, server = server)
