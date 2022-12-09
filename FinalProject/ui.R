#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#The Following packages were used in this analysis
library(tidyverse)
library(knitr)
library(shiny)
library(shinydashboard)
library(DT)
library(Lahman)
library(mathjaxr)
library(caret)
library(randomForest)


teamSubsetFinal <- read_csv("teamSubsetFinal.csv")
userDataRaw <- read_csv("userDataRaw.csv")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Baseball"),
                    dashboardSidebar(sidebarMenu(
                      menuItem("About", tabName = "About", icon = icon("baseball")),
                      menuItem("Data Exploration", tabName = "EDA", icon = icon("medal")), 
                      menuItem("Modeling", tabName = "Modeling", icon = icon("ranking-star")), 
                      menuItem("Data Manipulation", tabName = "Data", icon = icon("baseball-bat-ball"))
                      )
                      ),
                    dashboardBody(
                      tabItems(
                        #About tab
                        tabItem(tabName = "About",
                                box(
                                  #show MLB logo
                                  HTML('<center><img src="mlbLogo.png" width="200"></center>'),
                                  #output markdown file
                                  uiOutput('mymarkdown'), 
                                  width = 8
                                  )
                                ),
                        #EDA Tab
                        tabItem(tabName = "EDA",
                                fluidRow(
                                  box(
                                    title = h3("Exploratory Data Analysis"),
                                    selectInput("stat", "Select Data to Display", 
                                                choices = c("Offensive Stats" = "offStat", 
                                                            "Defensive Stats" = "defStat", 
                                                            "Franchise Performance" = "frPer", 
                                                            "Frachise Summaries" = "frSum")),
                                    conditionalPanel(condition = "input.stat == 'offStat'", 
                                                     selectInput("offense", "Select Offensive Statistic", 
                                                                 choices = c("Runs Per Game" = "avgRunsScored",
                                                                             "Hits Per Game" = "avgHits",
                                                                             "Total Bases Per Game" = "avgTotalBases",
                                                                             "Home Runs Per Game" = "avgHomeRuns",
                                                                             "Walks Per Game" = "avgWalks"), 
                                                                 selected = "avgRunsScored"), 
                                                     sliderInput("yrSliderO", "Select MLB Seasons",
                                                                 min = 1981, max = 2021, 
                                                                 value = c(1981, 2021), sep = '')),
                                    conditionalPanel(condition = "input.stat == 'defStat'", 
                                                     selectInput("defense", "Select Defensive Statistic", 
                                                                 choices = c("Runs Allowed Per Game" = "avgRunsAllowed",
                                                                      "Earned Runs Allowed (ERA)" = "ERA",
                                                                      "Hits Allowed Per Game" = "avgHitsAllowed",
                                                                      "Home Runs Allowed Per Game" = "avgHomeRunsAllowed",
                                                                      "Walks Allowed Per Game" = "avgWalksAllowed",
                                                                      "Errors Per Game" = "avgErrors", 
                                                                      "Fielding Percentage" = "fieldingPct"),
                                                                 selected = "avgRunsAllowed"), 
                                                     sliderInput("yrSliderD", "Select MLB Seasons", 
                                                                 min = 1981, max = 2021, 
                                                                 value = c(1981, 2021), sep = '')),
                                    conditionalPanel(condition = "input.stat == 'frPer'", 
                                                     selectInput("perTeam", "Select MLB Franchise", 
                                                                  choices = levels(as.factor(teamSubsetFinal$team)), 
                                                                  selected = "St. Louis Cardinals")), 
                                    conditionalPanel(condition = "input.stat == 'frSum'", 
                                                     radioButtons("lg", "Select Franchise's Current League", 
                                                                  choices = c("NL", "AL"), 
                                                                  selected = "NL"),
                                                     radioButtons("div", "Select Franchise's Current League", 
                                                                  choices = c("East", "Central", "West"), 
                                                                  selected = "East")),
                                    width=12)
                                  ),
                                fluidRow(
                                  box(plotOutput("plot1"), width = 6),
                                  box(dataTableOutput("table1"), width = 6)
                                  )
                                ),
                        tabItem(tabName = "Modeling",
                                fluidRow(
                                  h3("coming soon"),
                                  tabBox(
                                    tabPanel("Model Info", "Information about Modeling",
                                             h3("All About Modeling"),
                                             p("Lots of Info coming soon"),
                                             uiOutput('ex3')),
                                    tabPanel("Modeling",  
                                             fluidRow(
                                               box(
                                                 column(
                                                   h4("General Options"),
                                                   sliderInput("trDataProp", 
                                                               "Select Proportion of Data to Use for Training Model",
                                                               min = 0.65, max = 0.85, value = 0.75, step = 0.05),
                                                   checkboxGroupInput("modelParams", 
                                                                      "Select Predictors to Include in Modeling",
                                                              choices = c("Runs Per Game" = "avgRunsScored",
                                                                 "Hits Per Game" = "avgHits",
                                                                  "Total Bases Per Game" = "avgTotalBases",
                                                                  "Home Runs Per Game" = "avgHomeRuns", 
                                                                  "Walks Per Game" = "avgWalks", 
                                                                  "Runs Allowed Per Game" = "avgRunsAllowed",
                                                                  "Earned Runs Allowed (ERA)" = "ERA",
                                                                  "Hits Allowed Per Game" = "avgHitsAllowed",
                                                                  "Home Runs Allowed Per Game" = "avgHomeRunsAllowed", 
                                                                  "Walks Allowed Per Game" = "avgWalksAllowed",
                                                                  "Errors Per Game" = "avgErrors", 
                                                                  "Fielding Percentage" = "fieldingPct"), 
                                                              selected = "avgRunsScored"),
                                                   width = 3), 
                                                 column(
                                                   h4("Linear Options"), 
                                                   radioButtons("linearCV", "Select 5-fold or 10-fold cross-validation", 
                                                                choices = c("5-fold" = "five", 
                                                                            "10-fold" = "ten"),
                                                                selected = "five"),
                                                   width = 3),
                                                 column(
                                                   h4("Boosted Tree Model Options"), 
                                                   radioButtons("tree1", "Select 5-fold or 10-fold cross-validation",
                                                                choices = c("5-fold" = "five", 
                                                                            "10-fold" = "ten"),
                                                                selected = "five"),
                                                   radioButtons("treeShrink", "Tuning: Select shrinkage", 
                                                                choices = c("0.1" = "std", 
                                                                            "0.001" = "small"),
                                                                selected = "std"),
                                                   sliderInput("treeN", "Tuning: Select n.trees", 
                                                               min = 100, max = 300, value = 200, step = 50), 

                                                   sliderInput("treeMax", "Tuning: Select interaction.depth", 
                                                              min = 3, max = 6, value = 4, step = 1), 
                                                   sliderInput("treeObs", "Tuning: Select n.minobsinnode", 
                                                              min = 5, max = 10, value = 10, step = 1),
                                                   
                                                   width = 3),
                                                 column(
                                                   h4("Random Forest Model Options"), 
                                                   radioButtons("rf1", "Linear Regression: Select 5-fold or 10-fold CV", 
                                                                choices = c("5-fold" = "five", 
                                                                            "10-fold" = "ten"),
                                                                selected = "five"),
                                                   radioButtons("rfMtry", "Tuning: mtry", 
                                                                choices = c("2 (for fewer than 9 predictors)" = "two", 
                                                                            "3 (for 9 or more predictors)" = "three"),
                                                                selected = "two"),
                                                   width = 3), 
                                                 width = 12)
                                               ),
                                             fluidRow(
                                               box(
                                                 actionButton("runModel", "Click Here to Run Models", 
                                                              class = "btn-success"), 
                                                 align = "center",
                                                 width = 12)
                                             ),
                                             fluidRow(
                                               box(
                                               h3("Linear Model Ouput", align = "center"),
                                               box(dataTableOutput("lmTableTrain")), 
                                               box(dataTableOutput("lmTableTest")), 
                                               width = 12
                                               )),
                                             fluidRow(
                                               box(
                                               h3("Tree Model Output", align = "center"), 
                                               box(dataTableOutput("treeResultsTable"), width = 4), 
                                               box(plotOutput("treeResultsPlot"), width = 4), 
                                               box(dataTableOutput("treeTest"), width = 4), 
                                               width = 12
                                             )),
                                             fluidRow(
                                               h3("Random Forest Model Output", align = "center"), 
                                               box(dataTableOutput("rfResultsTable"), width = 4), 
                                               box(plotOutput("rfPlot"), width = 4), 
                                               box(dataTableOutput("rfTest"), width = 4), 
                                               width = 12
                                             )
                                             
                                             ),
                                    tabPanel("Prediction", "Prediction Tab"), width = 12)
                                  )
                                ),
                        tabItem(tabName = "Data", 
                                sidebarLayout(
                                  sidebarPanel(
                                    fluidRow(
                                      h3("Data Set Subsetting Options"),
                                      radioButtons("userStat", "Select Data to Display", 
                                                   choices = c("Show All Data" = "allStat",
                                                               "Show Offensive Data Only" = "offStat",
                                                               "Show Defensive Data Only" = "defStat"),
                                                   selected = c("allStat")),
                                      h3("Data Set Filtering Options"),
                                      checkboxInput("userTeamSelect", "Click to Filter Data by Team", value = FALSE),
                                      conditionalPanel(condition = "input.userTeamSelect == true", 
                                                       selectInput("userTeam", "Select MLB Franchise", 
                                                                   choices = levels(as.factor(teamSubsetFinal$team)), 
                                                                   selected = "St. Louis Cardinals")),
                                      checkboxInput("userYrSelect", "Click to Filter Data by Year", value = FALSE),
                                      conditionalPanel(condition = "input.userYrSelect == true", 
                                                       sliderInput("userYrSlider", "Select MLB Seasons",
                                                                   min = 1981, max = 2021, 
                                                                   value = c(1981, 2021), sep = '')),
                                      h3("Download Data to .csv"),
                                      downloadButton("downloadData", "Download"), 
                                      width = 12)
                                    ),
                                  # Show the table
                                  mainPanel(
                                    fluidRow(
                                      box(
                                        dataTableOutput("userData"), width = 12)
                                      )
                                    )
                                  )
                                )
                        )
                      )
                    )
