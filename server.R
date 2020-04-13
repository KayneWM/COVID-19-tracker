library(utils)
library(DT)
library(tidyverse)
#counties <- read.csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
#counties$date <- as.Date(counties$date, format = "%m/%d/%Y") #need to be converted here so that the arrange works correctly

counties <- read.csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
counties$date <- as.Date(counties$date, format = "%Y-%m-%d")

countiesforNY <- read.csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
countiesforNY$date <- as.Date(countiesforNY$date, format = "%Y-%m-%d")

#write.csv(counties,"C:/Users/Kayne/OneDrive/Desktop/Data2/countries.csv", row.names = TRUE)

countries <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
countries$dateRep <- as.Date(countries$dateRep, format = "%d/%m/%Y") #need to be converted here so that the arrange works correctly

#counties=read.csv("C:/Users/Kayne/OneDrive/Desktop/Data/us-counties.csv")
#counties$date <- as.Date(counties$date, format = "%m/%d/%Y")

#countries=read.csv("C:/Users/Kayne/OneDrive/Desktop/Data/countries_Adj.csv")
#countries$date <- as.Date(countries$date, format = "%m/%d/%Y")

#detach(package:plyr)
library(dplyr)
#Make sure that you have the “utils” and “httr” packages installed.

#########################################################################
################# LOAD POPULATIONS ######################################
#########################################################################

#POPULATIONS
#usmap for the US counties. Package also must be installed
library(usmap)
data(countypop)
data(citypop)

keeps <- c("fips","pop_2015") #what to keep from country database
countypopmerge <- countypop[ , (names(countypop) %in% keeps)]

keeps <- c("most_populous_city","city_pop") #keep from city
citypopmerge <- citypop[ , (names(citypop) %in% keeps)]

############################################################################
###################  LOAD COUNTRIES ########################################
#################### COUNTRIES IMPORT AS NEW CASES##########################
#################### NEED TO ADD CUMULATIVE CASES###########################
############################################################################

#these libraries need to be loaded
#library(utils)
#library(httr)


#download the dataset from the ECDC website to a local temporary file
#GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into “R”. CORONAVIRUS CASES BY COUNTRY
#countries <- read.csv(tf)
countries <-as.data.frame(countries)
names(countries)[names(countries) == "dateRep"] <- "date"
names(countries)[names(countries) == "countriesAndTerritories"] <- "location"
names(countries)[names(countries) == "popData2018"] <- "population"
names(countries)[names(countries) == "cases"] <- "casesnew"
names(countries)[names(countries) == "deaths"] <- "deathsnew"

keeps <- c("date","location", "casesnew","deathsnew","population")
countries <- countries[ , (names(countries) %in% keeps)]

countries$date <- as.Date(countries$date, format = "%d/%m/%Y") #need to be converted here so that the arrange works correctly

countries <- countries %>% arrange(location, date) %>% group_by(location) %>%
  mutate(casessum = cumsum(casesnew))
countries <- countries %>% arrange(location, date) %>% group_by(location) %>%
  mutate(deathssum = cumsum(deathsnew))


countries$casesnew <- as.double(countries$casesnew)
countries$deathsnew <- as.double(countries$deathsnew)

############################################################################
###################  LOAD COUNTIES (AND NYC)#################################
#################### COUNTIES IMPORT AS CUMULATIVE CASES####################
#################### NEED TO ADD NEW CASES###########################
############################################################################

library (readr)

#urlfile="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
#counties<-read_csv(url(urlfile))
#countiesoriginal<-read_csv(url(urlfile))

counties <- merge(countypopmerge,counties,by="fips") #merge two county databases
names(counties)[names(counties) == "pop_2015"] <- "population" #rename location


#since there are duplicate counties in different states, need to paste county and state into the location column

counties <- counties %>% 
  unite(location, c("county", "state"))
counties$location <- as.factor(counties$location)

#### Add new York City by creating same format. It does not come through New York Times at same time because the merge above deletes it since it does not appear in the population data base

#countyNYC <- read_csv(url(urlfile))
countyNYC <- countiesforNY[countiesforNY$county=="New York City",]
names(countyNYC)[names(countyNYC) == "county"] <- "location"

names(citypopmerge)[names(citypopmerge) == "most_populous_city"] <- "location" #rename location
names(citypopmerge)[names(citypopmerge) == "city_pop"] <- "population" #rename location
citypopmerge <- citypopmerge[citypopmerge$location=="New York City",]
cities <- merge(countyNYC,citypopmerge,by="location")
names(counties)[names(counties) == "cases"] <- "casessum"
names(counties)[names(counties) == "deaths"] <- "deathssum"
names(cities)[names(cities) == "cases"] <- "casessum"
names(cities)[names(cities) == "deaths"] <- "deathssum"

keeps <- c("date","location", "casessum","deathssum", "population")
counties <- counties[ , (names(counties) %in% keeps)]
cities <- cities[ , (names(cities) %in% keeps)]


counties <- rbind(counties,cities)
counties <-  counties %>% arrange(location, date) %>% group_by(location) %>%
  mutate(casesnew = casessum - lag(casessum, default = first(casessum)))
counties <-  counties %>% arrange(location, date) %>% group_by(location) %>%
  mutate(deathsnew = deathssum - lag(deathssum, default = first(deathssum)))

#########################################################################
################### STATES HAVE NOT DONE ANYTHING WITH THESE YET ########
#########################################################################

#urlfile2="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-#states.csv"
#states<-read_csv(url(urlfile2))
#states


##########################################################################
##########################################################################
################### CONVERTING TO COMMON VARIABLE RATEDATA ###############

counties$type <- "county"
countries$type <- "country"


ratedata <- rbind(counties, countries)

ratedata$location <-as.factor(ratedata$location)

#ratedata

## change to date field, and 'days' category

library(dplyr)
library(zoo)

#ratedata$date <- as.Date(ratedata$date, format = "%m/%d/%Y")

ratedata <- ratedata[!ratedata$casesnew == 0,]

country <- countries[!countries$casesnew == 0,]
county <-counties[!counties$casesnew == 0,]
combined <- ratedata

combined <- combined %>% arrange(location, date) %>% group_by(location) %>%
  mutate(diffDate = as.numeric(difftime(date, lag(date,1), units='days')))
country <- country %>% arrange(location, date) %>% group_by(location) %>%
  mutate(diffDate = as.numeric(difftime(date, lag(date,1), units='days')))
county <- county %>% arrange(location, date) %>% group_by(location) %>%
  mutate(diffDate = as.numeric(difftime(date, lag(date,1), units='days')))

combined$diffDate[is.na(combined$diffDate)] <- 0
country$diffDate[is.na(country$diffDate)] <- 0
county$diffDate[is.na(county$diffDate)] <- 0

combined <- combined %>% group_by(location) %>% mutate(Days = cumsum(diffDate))
country <- country %>% group_by(location) %>% mutate(Days = cumsum(diffDate))
county <- county %>% group_by(location) %>% mutate(Days = cumsum(diffDate))

#####
combined <- combined %>%
  # first sort by year
  arrange(location, Days) %>% group_by(location) %>%
  mutate(Diffgrowth = casesnew - lag(casesnew), # Difference in route between years
         Rate_percent = (Diffgrowth / diffDate)/casesnew * 100) # growth rate in percent
country <- country %>%
  # first sort by year
  arrange(location, Days) %>% group_by(location) %>%
  mutate(Diffgrowth = casesnew - lag(casesnew), # Difference in route between years
         Rate_percent = (Diffgrowth / diffDate)/casesnew * 100) # growth rate in percent
county<- county %>%
  # first sort by year
  arrange(location, Days) %>% group_by(location) %>%
  mutate(Diffgrowth = casesnew - lag(casesnew), # Difference in route between years
         Rate_percent = (Diffgrowth / diffDate)/casesnew * 100) # growth rate in percent

World <- country %>%
  # first sort by year
  arrange(date) %>% group_by(date) %>%
  mutate(Cases_All = sum(casessum))  %>%
  mutate(Deaths_All = sum(deathssum)) %>%
  mutate(Cases_New = sum(casesnew)) %>%
  mutate(Deaths_New = sum(deathsnew))

World2 <- c("Cases_All"=max(World$Cases_All), "Deaths_All"=max(World$Deaths_All),
            "Cases_New"=max(World$Cases_New),"Deaths_New"=max(World$Deaths_New))

World2<- as.data.frame(World2)
World2 <- as.data.frame(t(World2))
World2$Date <- as.Date(max(World$date))



#combined <- combined %>% filter(
#  location == "Centre_Pennsylvania" | location == "New York City"| location == "South_Korea" | location == "Spain" |location=="Orleans_Louisiana" & casessum>1
#)

###############TOP 10 CODE ##########################################

## COUNTY ORIGINAL top ten
## change to date field, andd 'days' category

library(dplyr)
library(zoo)

####

ratedata_o <- counties

#ratedata_o$date <- as.Date(countiesoriginal$date, format = "%m/%d/%Y")

ratedata_o <- ratedata_o[!ratedata_o$casessum == 0,]

ratedata_o <- ratedata_o %>% arrange(location, date) %>% group_by(location) %>%
  mutate(diffDate = as.numeric(difftime(date, lag(date,1), units='days')))


ratedata_o$diffDate[is.na(ratedata_o$diffDate)] <- 0

ratedata_o <- ratedata_o %>% group_by(location) %>% mutate(Days = cumsum(diffDate))

#ratedata_o 

ratedata_o <- ratedata_o %>%
  arrange(location, Days) %>% group_by(location) %>%
  mutate(Diffgrowth = casessum - lag(casessum), # Difference in route between years
         Rate_percent = (Diffgrowth / diffDate)/casessum * 100) # growth rate in percent 


ratedata_o <- 
  ratedata_o %>%
  mutate(date=as.Date(date, '%m/%d/%Y')) %>% 
  group_by(location) %>% 
  arrange(desc(date)) %>% 
  slice(1:1)

ratedata_o$casepop <- ratedata_o$casessum/ratedata_o$population*100000
ratedata_o$deathpop <- ratedata_o$deathssum/ratedata_o$population*100000
ratedata_o$deathcase <- ratedata_o$deathssum/ratedata_o$casessum*1000

ratedata_o_sub1 <-
  ratedata_o %>%
  group_by(location) %>%
  summarize(casessum = max(casessum), date=date)
ratedata_o_sub1 <- ratedata_o_sub1[with(ratedata_o_sub1,order(-casessum)),]
ratedata_o_sub1 <- ratedata_o_sub1[1:10,]

ratedata_o_sub1a <-
  ratedata_o %>%
  group_by(location) %>%
  summarize(deathssum = max(deathssum), date=date)
ratedata_o_sub1a <- ratedata_o_sub1a[with(ratedata_o_sub1a,order(-deathssum)),]
ratedata_o_sub1a <- ratedata_o_sub1a[1:10,]

ratedata_o_sub2 <-
  ratedata_o %>%
  group_by(location) %>%
  summarize(casepop = max(casepop), date=date)
ratedata_o_sub2 <- ratedata_o_sub2[with(ratedata_o_sub2,order(-casepop)),]
ratedata_o_sub2<- ratedata_o_sub2[1:10,]

ratedata_o_sub3 <-
  ratedata_o %>%
  group_by(location) %>%
  summarize(deathpop = max(deathpop), date=date)
ratedata_o_sub3 <- ratedata_o_sub3[with(ratedata_o_sub3,order(-deathpop)),]
ratedata_o_sub3<- ratedata_o_sub3[1:10,]

ratedata_o_sub1$date <- format(ratedata_o_sub1$date,'%A, %B %d, %Y')
ratedata_o_sub2$date <- format(ratedata_o_sub2$date,'%A, %B %d, %Y')
ratedata_o_sub3$date <- format(ratedata_o_sub3$date,'%A, %B %d, %Y')

#### new

ratedata_n <- ratedata_o %>%
  arrange(location, Days) %>% group_by(location) %>%
  mutate(Diffgrowth = casesnew - lag(casesnew), # Difference in route between years
         Rate_percent = (Diffgrowth / diffDate)/casesnew * 100) # growth rate in percent 


ratedata_n <- 
  ratedata_n %>%
  mutate(date=as.Date(date, '%m/%d/%Y')) %>% 
  group_by(location) %>% 
  arrange(desc(date)) %>% 
  slice(1:1)

ratedata_n$casepop <- ratedata_n$casesnew/ratedata_n$population*100000
ratedata_n$deathpop <- ratedata_n$deathsnew/ratedata_n$population*100000
ratedata_n$deathcase <- ratedata_n$deathsnew/ratedata_n$casesnew*1000

ratedata_n_sub1 <-
  ratedata_n %>%
  group_by(location) %>%
  summarize(casesnew = max(casesnew), date=date)
ratedata_n_sub1 <- ratedata_n_sub1[with(ratedata_n_sub1,order(-casesnew)),]
ratedata_n_sub1 <- ratedata_n_sub1[1:10,]

ratedata_n_sub1a <-
  ratedata_n %>%
  group_by(location) %>%
  summarize(deathsnew = max(deathsnew), date=date)
ratedata_n_sub1a <- ratedata_n_sub1a[with(ratedata_n_sub1a,order(-deathsnew)),]
ratedata_n_sub1a <- ratedata_n_sub1a[1:10,]

ratedata_n_sub2 <-
  ratedata_n %>%
  group_by(location) %>%
  summarize(casepop = max(casepop), date=date)
ratedata_n_sub2 <- ratedata_n_sub2[with(ratedata_n_sub2,order(-casepop)),]
ratedata_n_sub2<- ratedata_n_sub2[1:10,]

ratedata_n_sub3 <-
  ratedata_n %>%
  group_by(location) %>%
  summarize(deathpop = max(deathpop), date=date)
ratedata_n_sub3 <- ratedata_n_sub3[with(ratedata_n_sub3,order(-deathpop)),]
ratedata_n_sub3<- ratedata_n_sub3[1:10,]

ratedata_n_sub1$date <- format(ratedata_n_sub1$date,'%A, %B %d, %Y')
ratedata_n_sub1a$date <- format(ratedata_n_sub1a$date,'%A, %B %d, %Y')
ratedata_n_sub2$date <- format(ratedata_n_sub2$date,'%A, %B %d, %Y')
ratedata_n_sub3$date <- format(ratedata_n_sub3$date,'%A, %B %d, %Y')



####################### SERVER ################################################

server <- function(input, output, session){
  dataset <- reactive({
    get(input$dataset)
  })
  
    plotinfo <- reactive({
    get(input$plotinfo)
  })
  
  
  observe({
    updateSelectInput(session, "column", choices = names(dataset())) 
  })
  
  observeEvent(input$column, {
    column_levels <- as.character(sort(unique(
      dataset()[[input$column]]
    )))
    updateSelectInput(session, "level", choices = column_levels)
  })
  
  output$table <- DT::renderDataTable({
    subset(dataset(), dataset()[[input$column]] == input$level)
  })
  
  output$title_panel1 <- renderText({
    paste0("All World Cases: ", World2$Cases_All, 
           ", All World Deaths ", World2$Deaths_All,
           ", New World Cases: ", World2$Cases_New,
           ", New World Deaths: ", World2$Deaths_New)
  })
  
#  output$title_panel2 <- renderText({
#  paste0("All World Deaths ", World2$Deaths_All )
#  })
  
#  output$title_panel3 <- renderText({
#    paste0("New World Cases: ", World2$Cases_New,
#           ", New World Deaths: ", World2$Deaths_New)
#  })
  
#    output$title_panel4 <- renderText({
#    paste0( )
#  })
    
    output$title_panel5 <- renderText({
      paste0("as of ", World2$Date )
    })
  
  ################# CHOICES OF PLOTS ###############
  
  output$plot <- renderUI({
    if(input$plotinfo=="Cases"){
      output$plot1<-renderPlot({
        dataplot <- dataset()
        
        theme_set(theme_bw())  # pre-set the bw theme.
        g <- ggplot(data=subset(dataset(), dataset()[[input$column]] == input$level), aes(x=Days,y=casessum)) + 
          labs(subtitle="Cases over Time by Location",
               title="Bubble chart | Cases, Deaths",x="Days",y="Cases")+
         geom_jitter(aes(col=location, size=deathssum)) + 
          geom_smooth(aes(col=location), method="loess", se=F)
        
        if(input$logarithmicY)
          g <- g + scale_y_log10()
        
        return(g)
        
      })
      plotOutput("plot1")
    }
    
    else if(input$plotinfo=="Deaths"){
      output$plot1a<-renderPlot({
        dataplot <- dataset()
        
        theme_set(theme_bw())  # pre-set the bw theme.
        g <- ggplot(data=subset(dataset(), dataset()[[input$column]] == input$level), aes(x=Days,y=deathssum)) + 
          labs(subtitle="Deaths over Time by Location",
               title="Bubble chart | Deaths",x="Days",y="Log[Deaths]")+
         geom_jitter(aes(col=location, size=casessum)) + 
          geom_smooth(aes(col=location), method="loess", se=F) 
        
        if(input$logarithmicY)
          g <- g + scale_y_log10()
        
        return(g)
        
      })
      plotOutput("plot1a")
    }
    
    
    
    else if(input$plotinfo=="Cases_by_Population"){
      output$plot2<-renderPlot({
        dataplot <- dataset()
        
        theme_set(theme_bw())  # pre-set the bw theme.
        g <- ggplot(data=subset(dataset(), dataset()[[input$column]] == input$level), aes(x=Days,y=casessum/population*100000)) + 
          labs(subtitle="Cases (Per 100K People) over Time by Location",
               title="Bubble chart | Cases, Deaths",x="Days",y="New Cases Per 100K People") +
        geom_jitter(aes(col=location, size=deathssum)) + 
          geom_smooth(aes(col=location), method="loess", se=F) 
        
        if(input$logarithmicY)
          g <- g + scale_y_log10()
        
        return(g)
        
        
      })
      plotOutput("plot2")
    }
    
    
    else if(input$plotinfo=="Deaths_by_Population"){
      
      output$plot3<-renderPlot({
        dataplot <- dataset()
        
        dataplot <- dataset()
        
        theme_set(theme_bw())  # pre-set the bw theme.
        g <- ggplot(data=subset(dataset(), dataset()[[input$column]] == input$level), aes(x=Days,y=deathssum/population*100000)) + 
          labs(subtitle="Deaths (Per 100K People) over Time by Location",
               title="Bubble chart | Deaths",x="Days",y="Deaths Per 100K People") +
                geom_jitter(aes(col=location, size=casessum)) + 
          geom_smooth(aes(col=location), method="loess", se=F)  
          
        if(input$logarithmicY)
          g <- g + scale_y_log10()
        
        return(g)
        
         
      })
      plotOutput("plot3")
    }
    
    else if(input$plotinfo=="Deaths_by_Cases"){
      
      output$plot4<-renderPlot({
        dataplot <- dataset()
        
        theme_set(theme_bw())  # pre-set the bw theme.
        g <- ggplot(data=subset(dataset(), dataset()[[input$column]] == input$level), aes(x=Days,y=deathssum/casessum*1000)) + 
          labs(subtitle="Deaths per 1000 Cases over Time by Location",
               title="Bubble chart | Deaths per 1000 Cases",x="Days",y="Deaths per 1000 Cases") +
                geom_jitter(aes(col=location, size=deathssum)) + 
          geom_smooth(aes(col=location), method="loess", se=F)
        
        if(input$logarithmicY)
          g <- g + scale_y_log10()
        
        return(g)
        
      })
      plotOutput("plot4")
    }
    
    
    else if(input$plotinfo=="Top_10_US_Cumulative_Cases"){
      
      output$plot5<-renderPlot({
        theme_set(theme_bw())
       g <- ggplot(data=ratedata_o_sub1 , aes(x=reorder(location,-casessum), y=casessum, fill=date)) +
          geom_bar(stat="identity")+
          labs(subtitle="Top 10 by US County/City",
               title="Cumulative Cases",x="Counties",y="Cumulative Cases") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.2, size=10))+theme(plot.title = element_text(hjust = 0.5))
     
       if(input$logarithmicY)
         g <- g + scale_y_log10()
       
       return(g)
       
       
        })
      plotOutput("plot5")
    }
    
    
    else if(input$plotinfo=="Top_10_US_Cumulative_Deaths"){
      
      output$plot5a<-renderPlot({
        theme_set(theme_bw())
        g <- ggplot(data=ratedata_o_sub1a , aes(x=reorder(location,-deathssum), y=deathssum, fill=date)) +
          geom_bar(stat="identity")+
          labs(subtitle="Top 10 by US County/City",
               title="Cumulative Deaths",x="Counties",y="Cumulative Deaths") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.2, size=10))+theme(plot.title = element_text(hjust = 0.5))
        
        if(input$logarithmicY)
          g <- g + scale_y_log10()
        
        return(g)
        
        
      })
      plotOutput("plot5a")
    }
    
    else if(input$plotinfo=="Top_10_US_Cumulative_Cases_by_Population"){
      
      output$plot6<-renderPlot({
        theme_set(theme_bw())  # pre-set the bw theme.
       g <- ggplot(data=ratedata_o_sub2, aes(x=reorder(location,-casepop),y=casepop, fill=date)) +
          geom_bar(stat="identity")+
          labs(subtitle="Top 10 by US County/City",
               title="Cumulative Cases by 100K People",x="Counties",y="Cumulative Cases by 100K People") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.2, size=10))+
          theme(plot.title = element_text(hjust = 0.5))
       
       if(input$logarithmicY)
         g <- g + scale_y_log10()
       
       return(g)
       
       
      })
      plotOutput("plot6")
    }
    
    else if(input$plotinfo=="Top_10_US_Cumulative_Deaths_by_Population"){
      
      output$plot7<-renderPlot({
        theme_set(theme_bw())  # pre-set the bw theme.
        g <- ggplot(data=ratedata_o_sub3, aes(x=reorder(location,-deathpop),y=deathpop, fill=date)) + geom_bar(stat="identity")+
          labs(subtitle="Top 10 by US County/City",
               title="Cumulative Deaths by 100K People",x="Counties",y="Cumulative Deaths by 100K People") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.2, size=10))+
          theme(plot.title = element_text(hjust = 0.5))
        
        if(input$logarithmicY)
          g <- g + scale_y_log10()
        
        return(g)
        
      })
      plotOutput("plot7")
    }
    
    
    else if(input$plotinfo=="Top_10_US_New_Cases"){
      
      output$plot8<-renderPlot({
        theme_set(theme_bw())
        g <- ggplot(data=ratedata_n_sub1 , aes(x=reorder(location,-casesnew), y=casesnew, fill=date)) +
          geom_bar(stat="identity")+
          labs(subtitle="Top 10 by US County/City",
               title="New Cases",x="Counties",y="New Cases") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.2, size=10))+theme(plot.title = element_text(hjust = 0.5))
        
        if(input$logarithmicY)
          g <- g + scale_y_log10()
        
        return(g)
        
        
      })
      plotOutput("plot8")
    }
    
    
    else if(input$plotinfo=="Top_10_US_New_Deaths"){
      
      output$plot8a<-renderPlot({
        theme_set(theme_bw())
        g <- ggplot(data=ratedata_n_sub1a , aes(x=reorder(n), y=deathsnew, fill=date)) +
          geom_bar(stat="identity")+
          labs(subtitle="Top 10 by US County/City",
               title="New Deaths",x="Counties",y="New Deaths") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.2, size=10))+theme(plot.title = element_text(hjust = 0.5))
        
        if(input$logarithmicY)
          g <- g + scale_y_log10()
        
        return(g)
        
        
      })
      plotOutput("plot8a")
    }
    
    else if(input$plotinfo=="Top_10_US_New_Cases_by_Population"){
      
      output$plot9<-renderPlot({
        theme_set(theme_bw())  # pre-set the bw theme.
        g <- ggplot(data=ratedata_n_sub2, aes(x=reorder(location,-casepop),y=casepop, fill=date)) +
          geom_bar(stat="identity")+
          labs(subtitle="Top 10 by US County/City",
               title="New Cases by 100K People",x="Counties",y="New Cases by 100K People") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.2, size=10))+
          theme(plot.title = element_text(hjust = 0.5))
        
        if(input$logarithmicY)
          g <- g + scale_y_log10()
        
        return(g)
        
        
      })
      plotOutput("plot9")
    }
    
    else if(input$plotinfo=="Top_10_US_New_Deaths_by_Population"){
      
      output$plot10<-renderPlot({
        theme_set(theme_bw())  # pre-set the bw theme.
        g <- ggplot(data=ratedata_n_sub3, aes(x=reorder(location,-deathpop),y=deathpop, fill=date)) + geom_bar(stat="identity")+
          labs(subtitle="Top 10 by US County/City",
               title="New Deaths by 100K People",x="Counties",y="New Deaths by 100K People") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.2, size=10))+
          theme(plot.title = element_text(hjust = 0.5))
        
        if(input$logarithmicY)
          g <- g + scale_y_log10()
        
        return(g)
        
      })
      plotOutput("plot10")
    }
    
             
    
    
    
    
    
  })
  
  
  
}