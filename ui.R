library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("spacelab"),
  
                
  sidebarLayout(
  sidebarPanel(
    img(src = "medical.jpg",height = 100, width = 200),
    br(),
    selectInput("dataset", "(Manual Plot) Choose a dataset", c("country", "county", "combined")),
    selectInput("column", "(Manual Plot) Select 'location'", "-----"),
    selectInput("level", "(Manual Plot) Select level(s) (click in box)", "----",multiple = TRUE),
    selectInput("plotinfo", "(Manual/Auto Plot): Choose info to plot", c("Cases","Deaths","Cases_by_Population",
                "Deaths_by_Population","Deaths_by_Cases", "Top_10_US_Cumulative_Cases",
                "Top_10_US_Cumulative_Deaths",
                "Top_10_US_Cumulative_Cases_by_Population",
                "Top_10_US_Cumulative_Deaths_by_Population",
                "Top_10_US_New_Cases",
                "Top_10_US_New_Cases_by_Population",
                "Top_10_US_New_Deaths_by_Population")),
    checkboxInput("logarithmicY", "show y-axis in log10", FALSE),
    br(),
    #br(),
    p(strong("Directions:")),
    p(em("Manual")),
    h6("Choose a dataset, parameter, one or more levels (such as counties), and what to plot."), 
    #br(),
    p(em("Automatic")),
    h6("Choose an autobuilt Top 10 plot. (Note: Top 10 plots ignore manual choices)."),
    
  ),
  mainPanel(
    
    titlePanel("COVID-19 Tracker | Plot, Table"),
    h6(textOutput("title_panel1")),
    #h6(textOutput("title_panel2")),
    #h6(textOutput("title_panel3")),
    #h6(textOutput("title_panel4")),
    h6(textOutput("title_panel5")),
    
    tabsetPanel(type = "tabs",
                tabPanel("Plot", uiOutput('plot'),
                         h6("Note: countries or US counties with few data points will
                            not have regressed lines, and revert the plot
                            to scatterpoints.")),
                
                tabPanel("Table",  h2("Table of Data"),
                         DT::dataTableOutput("table")),
                
                tabPanel("Instructions", 
                         
                         h2('Instructions for manual plots and the table'),
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
                         img(src = "step5.jpg",height = 300, width = 600),
                         br(),
                         br(),
                         h4("Table: Responds to the filters created in the above steps. The information on the table can be sorted by using the arrows at the top of the columns."),
                         h6("In the example below, the Table has been sorted by date, which gives the most recent counts."),
                         img(src = "step6.jpg",height = 300, width = 600),
                         br(),
                         br(),
                         
                         h2('Instructions for automated plots'),
                         br(),
                         
                         h4("For US counties, there are a series of automated plots that filter the data by 'Top 10' categories, such as cases, deaths, etc. Choose a Top 10 plot to see the results."),
                         img(src = "step7.jpg",height = 300, width = 600),
                         
                         
                         ),
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
    ),
    
    
    
    
    )
))


