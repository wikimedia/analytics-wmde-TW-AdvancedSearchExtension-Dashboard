### ---------------------------------------------------------------------------
### --- Advanced Search Extension Dashboard
### --- Script: ui.R, v. Beta 0.1
### --- Technical Wishlist, WMDE
### ---------------------------------------------------------------------------

### --- Setup

### --- general
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(dygraphs)

### --- outputs
library(ggvis)

# - options
options(warn = -1)

### --- User Interface w. {shinydashboard}

shinyUI(
  
  ### --- dashboardPage
  ### --------------------------------
  
  dashboardPage(skin = "black",
                
                ### --- dashboarHeader
                ### --------------------------------
                
                dashboardHeader(
                  # - Title
                  title = "Advanced Search Extension",
                  titleWidth = 300
                ), 
                ### ---- END dashboardHeader
                
                ### --- dashboardSidebar
                ### --------------------------------
                
                dashboardSidebar(
                  sidebarMenu(
                    id = "tabsWDCM",
                    menuItem(text = "Overview", 
                             tabName = "overview", 
                             icon = icon("barcode"),
                             selected = TRUE
                    ),
                    menuItem(text = "Projects", 
                             tabName = "projects", 
                             icon = icon("bar-chart", class = NULL, lib = "font-awesome")
                    ),
                    menuItem(text = "Correlations", 
                             tabName = "correlations", 
                             icon = icon("bar-chart", class = NULL, lib = "font-awesome")
                    ),
                    menuItem(text = "Special:Search", 
                             tabName = "specialsearch", 
                             icon = icon("bar-chart", class = NULL, lib = "font-awesome")
                    ),
                    menuItem(text = "Search Keywords", 
                             tabName = "specialsearchkeys", 
                             icon = icon("bar-chart", class = NULL, lib = "font-awesome")
                    )
                  )
                ),
                ### --- END dashboardSidebar
                
                ### --- dashboardBody
                ### --------------------------------
                
                dashboardBody(
                  tags$head(tags$style(HTML('.content-wrapper, .right-side {
                                              background-color: #ffffff;
                                            }'))),
                  tabItems(
                    
                    ### --- TAB: Overview
                    ### --------------------------------
                    
                    tabItem(tabName = "overview",
                            fluidRow(
                              column(width = 6,
                                     HTML('<p style="font-size:80%;"><b>Overview. </b>This page presents three charts: the data on the 
                                          Advanced Search Extension usage for (1) the last 24h, (2) during the previous week, and (3) the previous 120 
                                          days. Use the drop-down menus next to the respective chart to select the metric of interest. The source of 
                                          the present data is the <a href="https://meta.wikimedia.org/wiki/Schema:AdvancedSearchRequest" target="_blank">
                                          AdvancedSearchRequest schema</a>.</p>')
                                     )
                            ),
                            fluidRow(
                              column(width = 12, 
                                     h3("Previous 24h")
                                     )
                            ),
                            fluidRow(
                              column(width = 2, 
                                     selectizeInput("selectMetric_gO24",
                                                    "Select Metric",
                                                    multiple = F,
                                                    choices = NULL,
                                                    selected = "Searches")
                                     ),
                              column(width = 7,
                                     ggvisOutput('gO24_plot')
                                     ),
                              column(width = 3,
                                     withSpinner(valueBoxOutput("gO24_previousDayBox", width = 12), size = .5)
                                     )
                            ),
                            fluidRow(
                              column(width = 12, 
                                     hr()
                                     )
                            ),
                            fluidRow(
                              column(width = 12, 
                                     h3("Previous 7 Days")
                              )
                            ),
                            fluidRow(
                              column(width = 2, 
                                     selectizeInput("selectMetric_gOWeek",
                                                    "Select Metric",
                                                    multiple = F,
                                                    choices = NULL,
                                                    selected = "Searches")
                              ),
                              column(width = 7,
                                     ggvisOutput('gOWeek_plot')
                              ),
                              column(width = 3,
                                     withSpinner(valueBoxOutput("gOWeek_previousWeekBox", width = 12), size = .5)
                                     )
                            ),
                            fluidRow(
                              column(width = 12, 
                                     hr()
                              )
                            ),
                            fluidRow(
                              column(width = 12, 
                                     h3("Previous 120 days")
                              )
                            ),
                            fluidRow(
                              column(width = 2, 
                                     selectizeInput("selectMetric_gOMonth",
                                                    "Select Metric",
                                                    multiple = F,
                                                    choices = NULL,
                                                    selected = "Searches")
                              ),
                              column(width = 7,
                                     ggvisOutput('gOMonth_plot')
                              ),
                              column(width = 3,
                                     withSpinner(valueBoxOutput("gOWeek_previousMonthBox", width = 12), size = .5)
                              )
                            ),
                            fluidRow(
                              hr(),
                              column(width = 1,
                                     br(),
                                     img(src = 'Technical_Wishes.png',
                                         align = "left")
                                      ),
                              column(width = 11,
                                     hr(),
                                     HTML('<b>Advanced Search Extension Usage Dashboard :: WMDE Technical Wishlist 2018</b><br>'),
                                     HTML('<b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm'),
                                     br(),
                                     br()
                                     )
                              )
                            ),
                  ### --- END TAB: Overview
                  
                  ### --- TAB: Projects
                  ### --------------------------------
                  
                  tabItem(tabName = "projects",
                          fluidRow(
                            column(width = 6,
                                   HTML('<p style="font-size:80%;"><b>Projects. </b>The presentation of the data is essentially the same is in the Overview 
                                          tab except for the additional drop-down menu that allows for a selection of a particular project to focus on. 
                                          Thus, this page presents three charts: the data on the Advanced Search Extension usage for (1) the last 24h, 
                                          (2) during the previous week, and (3) the previous 120 days. Use the drop-down menus next to the respective chart to 
                                          select (a) the metric and (b) the project of interest. The source of 
                                          the present data is the <a href="https://meta.wikimedia.org/wiki/Schema:AdvancedSearchRequest" target="_blank">
                                          AdvancedSearchRequest schema</a>.</p>')
                            )
                          ),
                          fluidRow(
                            column(width = 12, 
                                   h3("Previous 24h")
                            )
                          ),
                          fluidRow(
                            column(width = 2, 
                                   selectizeInput("selectMetric_gO24_proj",
                                                  "Select Metric",
                                                  multiple = F,
                                                  choices = NULL,
                                                  selected = "Searches"),
                                   selectizeInput("selectProject_gO24_proj",
                                                  "Select Project",
                                                  multiple = F,
                                                  choices = NULL)
                            ),
                            column(width = 7,
                                   ggvisOutput('gO24_proj_plot')
                            ),
                            column(width = 3,
                                   withSpinner(valueBoxOutput("gO24_previousDayBox_proj", width = 12), size = .5)
                            )
                          ),
                          fluidRow(
                            column(width = 12, 
                                   hr()
                            )
                          ),
                          fluidRow(
                            column(width = 12, 
                                   h3("Previous 7 Days")
                            )
                          ),
                          fluidRow(
                            column(width = 2, 
                                   selectizeInput("selectMetric_gOWeek_proj",
                                                  "Select Metric",
                                                  multiple = F,
                                                  choices = NULL,
                                                  selected = "Searches"),
                                   selectizeInput("selectProject_week_proj",
                                                  "Select Project",
                                                  multiple = F,
                                                  choices = NULL)
                                   ),
                            column(width = 7,
                                   ggvisOutput('gOWeek_plot_proj')
                            ),
                            column(width = 3,
                                   withSpinner(valueBoxOutput("gOWeek_previousWeekBox_proj", width = 12), size = .5)
                            )
                          ),
                          fluidRow(
                            column(width = 12, 
                                   hr()
                            )
                          ),
                          fluidRow(
                            column(width = 12, 
                                   h3("Previous 120 days")
                            )
                          ),
                          fluidRow(
                            column(width = 2, 
                                   selectizeInput("selectMetric_gOMonth_proj",
                                                  "Select Metric",
                                                  multiple = F,
                                                  choices = NULL,
                                                  selected = "Searches"),
                                   selectizeInput("selectProject_3months_proj",
                                                  "Select Project",
                                                  multiple = F,
                                                  choices = NULL)
                            ),
                            column(width = 7,
                                   ggvisOutput('gOMonth_plot_proj')
                            ),
                            column(width = 3,
                                   withSpinner(valueBoxOutput("gOWeek_previousMonthBox_proj", width = 12), size = .5)
                            )
                          ),
                          fluidRow(
                            hr(),
                            column(width = 1,
                                   br(),
                                   img(src = 'Technical_Wishes.png',
                                       align = "left")
                            ),
                            column(width = 11,
                                   hr(),
                                   HTML('<b>Advanced Search Extension Usage Dashboard :: WMDE Technical Wishlist 2018</b><br>'),
                                   HTML('<b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm'),
                                   br(),
                                   br()
                            )
                          )
                          ),
                  ### --- END TAB: Projects
                  
                  ### --- tabItem: Correlations
                  tabItem(tabName = "correlations",
                          fluidRow(
                            column(width = 6,
                                   HTML('<p style="font-size:80%;"><b>Correlations. </b>Based on the source data from the 
                                          <a href="https://meta.wikimedia.org/wiki/Schema:AdvancedSearchRequest" target="_blank">AdvancedSearchRequest schema</a>, 
                                        this page presents the co-occurences in the usage of Advanced Search Extension specific keywords. Three tables are provided: 
                                        (1) for the previous 24h, (2) the previous week, and (3) the previous 120 days. </p>')
                            )
                          ),
                          fluidRow(
                            column(width = 12, 
                                   h3("Previous Week"),
                                   withSpinner(plotOutput('keyOccurWeekCorPlot', width = "50%")),
                                   h3("Previous Month"),
                                   withSpinner(plotOutput('keyOccurMonthCorPlot', width = "50%")),
                                   h3("Previous 3 months"),
                                   withSpinner(plotOutput('keyOccur3MonthsCorPlot', width = "50%"))
                                   
                            )
                          ),
                          fluidRow(
                            hr(),
                            column(width = 1,
                                   br(),
                                   img(src = 'Technical_Wishes.png',
                                       align = "left")
                            ),
                            column(width = 11,
                                   hr(),
                                   HTML('<b>Advanced Search Extension Usage Dashboard :: WMDE Technical Wishlist 2018</b><br>'),
                                   HTML('<b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm'),
                                   br(),
                                   br()
                            )
                          )
                  ),
                  
                  ### --- tabItem: Special:Search
                  tabItem(tabName = "specialsearch",
                          fluidRow(
                            column(width = 6,
                                   HTML('<p style="font-size:80%;"><b>Special:Search Requests. </b>This page presents four charts based on the historical data: (1) the total count of Special:Search requests, (2) the count of Special:Search requests 
                                        where the Advanced Search Extension was used, (3) the percent of Special:Search requests where the Advanced 
                                        Search Extension was used, and (4) the count of Special:Search requests excluding those where the Advanced Search 
                                        Extension was used. <br>The source of the present data is the <a href="https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Traffic/Cirrus" 
                                        target="_blank"><i>CirrusSearchRequestSet</i> Hive table</a> of the <a href="https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Traffic/Webrequest" 
                                        target="_blank"><i>wmf_raw</i> database</a> in the <a href="https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake" 
                                        target="_blank">WMF Analytics Data Lake</a>.</p>')
                            )
                          ),
                          fluidRow(
                            column(width = 12, 
                                   h4("Special:Search Total Count"),
                                   withSpinner(dygraphOutput('SpecialSearchCount', width = "90%")),
                                   hr(),
                                   h4("Advanced Search Extension Count"),
                                   withSpinner(dygraphOutput('AdvancedSearchCount', width = "90%")),
                                   hr(),
                                   h4("Advanced Search Extension Percent"),
                                   withSpinner(dygraphOutput('AdvancedSearchPercent', width = "90%")),
                                   hr(),
                                   h4("Special:Search Count (excluding Advanced Search Extension)"),
                                   withSpinner(dygraphOutput('SpecialSearchOnlyCount', width = "90%"))
                            )
                          ),
                          fluidRow(
                            hr(),
                            column(width = 1,
                                   br(),
                                   img(src = 'Technical_Wishes.png',
                                       align = "left")
                            ),
                            column(width = 11,
                                   hr(),
                                   HTML('<b>Advanced Search Extension Usage Dashboard :: WMDE Technical Wishlist 2018</b><br>'),
                                   HTML('<b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm'),
                                   br(),
                                   br()
                            )
                          )
                  ),
                  
                  ### --- tabItem: Special:Search Keywords
                  tabItem(tabName = "specialsearchkeys",
                          fluidRow(
                            column(width = 6,
                                   HTML('<p style="font-size:80%;"><b>Special:Search Keywords. </b>This page presents two charts based on the historical data: (1) the count of Special:Search requests and  (2) the count of Special:Search requests 
                                        where the Advanced Search Extension was used, offering a selection of a specific search keyword of interest 
                                        (the "Select keyword:" drop-down menu above the charts). <br>The source of the present data is the <a href="https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Traffic/Cirrus" 
                                        target="_blank"><i>CirrusSearchRequestSet</i> Hive table</a> of the <a href="https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake/Traffic/Webrequest" 
                                        target="_blank"><i>wmf_raw</i> database</a> in the <a href="https://wikitech.wikimedia.org/wiki/Analytics/Data_Lake" 
                                        target="_blank">WMF Analytics Data Lake</a>.</p>')
                            )
                          ),
                          fluidRow(
                            column(width = 12, 
                                   selectizeInput('keywords',
                                                  'Select keyword:',
                                                  choices = NULL,
                                                  multiple = FALSE),
                                   h4("Special:Search"),
                                   withSpinner(dygraphOutput('SpecialSearchKeywords', width = "90%")),
                                   hr(),
                                   h4("Advanced Search Extension"),
                                   withSpinner(dygraphOutput('AdvancedSearchKeywords', width = "90%"))
                            )
                          ),
                          fluidRow(
                            hr(),
                            column(width = 1,
                                   br(),
                                   img(src = 'Technical_Wishes.png',
                                       align = "left")
                            ),
                            column(width = 11,
                                   hr(),
                                   HTML('<b>Advanced Search Extension Usage Dashboard :: WMDE Technical Wishlist 2018</b><br>'),
                                   HTML('<b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm'),
                                   br(),
                                   br()
                            )
                          )
                  )
                  
                ) ### --- END tabItems
                
                ) ### --- END dashboardBody
                
                ) ### --- dashboardPage
  
  ) # END shinyUI

