library(shiny)
library(readxl)
library(plotly)
library(ggplot2)
library(reshape2)
library(shinydashboard)
library(ggpubr)
library(Hmisc)
KansasCrimeLong <- read_excel("~/Desktop/KUMC/Fall2021/DATA 824/Final Project/KansasCrimeLong.xlsx")
KCrime <- KansasCrimeLong

KCrimeRe <- reshape(KCrime, idvar = "County", varying = c("2012", "2013", "2014", "2015", "2016", "2017"), 
        times = c("2012", "2013", "2014", "2015", "2016", "2017"), 
        new.row.names = 1:1000, v.name = c("Crime Rate"), direction = "long")

colnames(KCrimeRe)[2] <- c("Year")

fit <- lm(Year ~ `Crime Rate`, data = KCrimeRe)

Mean_Crime_Rates2 <- read_excel("~/Desktop/KUMC/Fall2021/DATA 824/Final Project/Mean Crime Rates2.xlsx")


testbody <- {
  tabItem("Tab1", plotlyOutput("KansasCountiesCrimeRates"), 
          plotlyOutput("KansasCountiesCrimeRates2"),
          valueBoxOutput("KCrime"))
}



                    
ui <- dashboardPage( skin = "red",
                     dashboardHeader(title = "Crime Rates"),
                     dashboardSidebar(
                       sidebarMenu( id="menu",
                                    menuItem("Kansas County", tabName = "Tab1", icon = icon("fish")),
                                    selectInput("county","County",choices=c("",KCrime$County, selected=""))
                                    )
                                 ),
                                    dashboardBody(
                                                tabItem(tabName = "Tab1", testbody)
                                                  )
                                              )

server <- function(input, output) {
  output$KansasCountiesCrimeRates<-renderPlotly({ 
    KCrimeRe<-KCrimeRe[KCrimeRe$County==input$county,]
    ggplot(KCrimeRe, aes(x=Year, y=`Crime Rate`)) + geom_bar(stat="Identity") +
      ggtitle("Crime Rate Bar Plot")})
  
  output$KansasCountiesCrimeRates2<-renderPlotly({ 
    KCrimeRe2<-KCrimeRe[KCrimeRe$County==input$county,]
    ggplot(KCrimeRe2, aes(x= Year, y=`Crime Rate`)) + 
                  geom_point() + 
                  geom_smooth(method='lm',formula=KCrimeRe2$`Crime Rate`~KCrimeRe2$Year) +
                  ggtitle("Crime Rate Scatter Plot")})
  
  output$KCrime<-renderValueBox({
    KCrimeRe3<-Mean_Crime_Rates2[Mean_Crime_Rates2$County==input$county,]
    valueBox( "Mean Crime Rate",
    KCrimeRe3$`Mean Crime Rate`,
    color = "red",
    width =6)
  })
  
}

shinyApp(ui, server)

