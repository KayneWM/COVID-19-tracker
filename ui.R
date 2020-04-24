library(shiny)
library(shinythemes)

ui <- navbarPage("COVID-19 Tracker Navigation Bar:",
                
                 
tabPanel("Manual Plotting",
                          
  sidebarLayout(
  sidebarPanel(
    img(src = "medical.jpg",height = 100, width = 200),
    br(),
    h3("Manual Plotting"),
    selectInput("dataset", "Choose a dataset", c("country", "state", "county", "combined")),
    selectInput("column", "Select 'Location'", "------"),
    selectInput("level", "Select level(s) (click in box, type)", "----",multiple = TRUE),
    selectInput("plotinfo", "Choose info to plot", 
                c("Cases","Cases_Growth_Rate", "Deaths","Cases_by_Population",
                "Deaths_by_Population","Deaths_by_Cases")),
    
    
    checkboxInput("logarithmicY", "show y-axis in log10", FALSE),
    br(),
    #br(),
    
    
  ), #sidebarpanelclose
  
  mainPanel(
    
    h5(textOutput("title_panel1")),
    h6(textOutput("title_panel5")),
    titlePanel("COVID-19 Tracker | Manual Plotting, Table"),
    h6("See tabs below"),
    
    #h6(textOutput("title_panel2")),
    #h6(textOutput("title_panel3")),
    #h6(textOutput("title_panel4")),
    
    
    tabsetPanel(type = "tabs",
               
                
                tabPanel("Instructions", 
                         
                         h3('See the Navigation Bar at the top of the screen'),
                         
                         h5('The navigation bar allows the user to toggle between 
                            the Manual Plotting page, the Auto Plotting page, and More, i.e.
                            the About and References pages. The application
                            starts on the Manual Plotting page.'),
                         
                         h2('Instructions for manual plots and the table'),
                         
                         
                         h5("These instructions describe the steps a user should take 
                            to create manual plots and the reference table, accessable by tabs
                            on the manual plot page. The steps are outlined briefly in text with example screenshots.
                            The data table will autopopulate based on the manual plot choices."),
                         
                         br(),
                         
                         h4("Step 1: Use the first drop down to choose country, county, or combined."),
                         img(src = "step1.jpg",height = 300, width = 600),
                         br(),
                         br(),
                         h4("Step 2: Use the second drop down to choose location."),
                         img(src = "step2.jpg",height = 300, width = 600),
                         br(),
                         br(),
                         h4("Step 3: Use the third drop down to choose one or more specific countries and/or counties to plot."),
                         img(src = "step3.jpg",height = 300, width = 600),
                         br(),
                         br(),
                         h4("Step 4: Use the fourth drop down to choose the data to be plotted, such as cases, deaths, etc."), 
                         h6("Note: do not choose 'Top 10' plots for a manual plot."),
                         img(src = "step4.jpg",height = 300, width = 600),
                         br(),
                         br(),
                         h4("Optional: Click on the logrithm box to see the y-axis in logrithmic form."),
                         h6("Logrithmic format is typical for exponential growth."),
                         h6("Logrithmic will not change the growth rate plot."),
                         img(src = "step5.jpg",height = 300, width = 600),
                         br(),
                         br(),
                         h4("Table: Responds to the filters created in the above steps. The information on the table can be sorted by using the arrows at the top of the columns."),
                         h6("In the example below, the Table has been sorted by date, which gives the most recent counts."),
                         img(src = "step6.jpg",height = 300, width = 600),
                         br(),
                         br(),
                         
                         
                         ),
                
                tabPanel("Manual Plot", uiOutput('plot'),
                         h6("Note: countries, US states or US counties with few data points will
                            not have regressed lines, and revert the plot
                            to scatterpoints."),
                        h6("Note: Log10 for y-axis not applied to growth rate plots.")),
                
                
                tabPanel("Table",  h2("Table of Data"),
                         DT::dataTableOutput("table"))
                
                
                
    ), #tabsetPanelclose
    
    
    
    
    ) #mainpanelclose
) #sibebarLayoutclose

), #tabpanel1close

tabPanel("Auto Plotting",
         
         sidebarLayout(
           sidebarPanel(
             img(src = "medical.jpg",height = 100, width = 200),
             br(),
             h3("Top10 Auto Plots"),
             
             h6("Ensure the dropdown menu chosen (World Country, US State, US County)
                matches the plot tab selected to the right."),
             
             selectInput("plotinfo4", "Choose a Top10 World Country plot", 
                         c("Top10_Country_Cumulative_Cases",
                           "Top10_Country_Cumulative_Deaths",
                           "Top10_Country_Cumulative_Cases_by_Population",
                           "Top10_Country_Cumulative_Deaths_by_Population",
                           "Top10_Country_New_Cases",
                           #"Top10_Country_New_Deaths",
                           "Top10_Country_New_Cases_by_Population",
                           "Top10_Country_New_Deaths_by_Population")), 
             
            selectInput("plotinfo3", "Choose a Top10 US State plot", 
                         c("Top10_State_Cumulative_Cases",
                           "Top10_State_Cumulative_Deaths",
                           "Top10_State_Cumulative_Cases_by_Population",
                           "Top10_State_Cumulative_Deaths_by_Population",
                           "Top10_State_New_Cases",
                           #"Top10_State_New_Deaths",
                           "Top10_State_New_Cases_by_Population",
                           "Top10_State_New_Deaths_by_Population")), 
            
             selectInput("plotinfo2", "Choose a Top10 US County plot", 
                         c("Top10_County_Cumulative_Cases",
                           "Top10_County_Cumulative_Deaths",
                           "Top10_County_Cumulative_Cases_by_Population",
                           "Top10_County_Cumulative_Deaths_by_Population",
                           "Top10_County_New_Cases",
                           #"Top10_County_New_Deaths",
                           "Top10_County_New_Cases_by_Population",
                           "Top10_County_New_Deaths_by_Population")),
             
             
             #checkboxInput("logarithmicY", "show y-axis in log10", FALSE),
             br(),
             #br(),
             
             
           ), #sidebarPanelclosebracket
           
           mainPanel(
             
             titlePanel("COVID-19 Tracker | Top10 Auto Plot"),
             h6("See tabs below"),
             ##h4(textOutput("title_panel1")),
             #h6(textOutput("title_panel2")),
             #h6(textOutput("title_panel3")),
             #h6(textOutput("title_panel4")),
             ##h6(textOutput("title_panel5")),
             
             tabsetPanel(type = "tabs",
                         
                         tabPanel("Instructions", 
                                  
                                  #br(),
                                  
                                  h2('Instructions for automated plots'),
                                  br(),
                                  
                                  h5("For World Countries, US Counties and US States, there are a series of automated plots that filter the data by 'Top 10' categories, such as cases, deaths, etc. Choose a Top 10 plot to see the results. 
                                     Ensure that the appropriate plot tab is chosen (World
                                     country, US state, or US county) depending on the choice bar you are using."),
                                  img(src = "step7.jpg",height = 300, width = 600),
                                  
                                  
                         ),
                         
                         tabPanel("Top10 World Country", uiOutput('plotauto4'),
                                  h6("Note: plots will update based on the raw data sources.")),
                         
                        tabPanel("Top10 US_State", uiOutput('plotauto2'),
                                  h6("Note: plots will update based on the raw data sources.")),
                         
                         tabPanel("Top10 US_County", uiOutput('plotauto'),
                                  h6("Note: plots will update based on the raw data sources."))
                         
                         
                    ), #tabsetPanelclosebracket
                         
                         
             ) #mainPanelclosebracket
             
             
             
             
           
         ) #sidebarLayoutclose
         
), #closetabpanel2

navbarMenu("More",
           
tabPanel("References", 
         h2('Data Sources'),
         br(),
         h5("County Data: live feed URL from The New York Times, based on reports from state and local health agencies, https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html"
         ),
         h5("Country Data: live feed URL from the European Centre for Disease Prevention and Control, 
https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
"
         ),
         h5("Population Data: USMAP in CRAN for R, https://CRAN.R-project.org/package=usmap"
         )
),

tabPanel("About", 
         h5("The COVID-19 tracker is an interactive application
                            using the most recent COVID-19 data to produce regressed scatter 
                            plots of countries and/or US counties chosen by the user."),
         
         h5("Single or multiple locations can be plotted. 
                         A sortable table of
                            results is also generated from the same user input."),
         
         h5("The table contains 
                            numerical data of new and cumulative cases and deaths, cases and 
                            deaths by population, and deaths per cases. Review of the data can
                            also be performed by categories other than location."),
         
         #br(),
         
         h5("Automated plots are also available in barplot format, showing the 
                            Top 10 US counties for new and cumulative cases from the most 
                            recent data available. Plots of new and cumulative COVID-19 deaths
                            are also available to plot for these Top 10 counties."),
         
         h5("The code for this Shinyapp is available at 
                            https://github.com/KayneWM/COVID-19-tracker")
         
)

) #navbarclosingbracket

) #closingbracket

 
