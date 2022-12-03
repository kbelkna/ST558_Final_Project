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

# Define UI for application that draws a histogram
dashboardPage(skin = "blue",
  dashboardHeader(title = "Baseball"),
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName = "About", icon = icon("baseball")),
    menuItem("Data Exploration", tabName = "EDA", icon = icon("medal")), 
    menuItem("Modeling", tabName = "Modeling", icon = icon("ranking-star"))), 
    menuItem("Data Manipulation", tabName = "Data", icon = icon("baseball-bat-ball"))),

  dashboardBody(
  
    dashboardBody(
    
        tabItems(
        
       #About tab
        tabItem(tabName = "About",
                h2("About")
        ),

        #EDA Tab
        tabItem(tabName = "EDA",
                fluidRow(
                  box(
                    title = "Exploratory Data Analysis",
                    selectInput("stat", "Select Data to Display", choices = c("Offensive Stats" = "offStat", 
                                                                              "Defensive Stats" = "defStat", 
                                                                              "Franchise Performance" = "frPer", 
                                                                              "Frachise Summaries" = "frSum")),
                    conditionalPanel(condition = "input.stat == 'offStat'", 
                                     selectInput("offense", "Select Offensive Statistic", 
                                                 choices = c("Runs Per Game" = "rpg",
                                                             "Hits Per Game" = "hpg",
                                                             "Total Bases Per Game" = "tbpg",
                                                             "Home Runs Per Game" = "hrpg", 
                                                             "Walks Per Game" = "bbpg"), 
                                                 selected = "rpg"), 
                                     sliderInput("yrSliderO", "Select MLB Seasons",
                                                 min = 1981, 
                                                 max = 2021, 
                                                 value = c(1981, 2021), 
                                                 sep = '')),
                    conditionalPanel(condition = "input.stat == 'defStat'", 
                                     selectInput("defense", "Select Defensive Statistic", 
                                                 choices = c("Runs Allowed Per Game" = "rapg",
                                                             "Earned Runs Allowed (ERA)" = "ERA",
                                                             "Hits Allowed Per Game" = "hapg",
                                                             "Home Runs Allowed Per Game" = "hrapg", 
                                                             "Walks Allowed Per Game" = "bbapg",
                                                             "Errors Per Game" = "epg", 
                                                             "Fielding Percentage" = "FP"),
                                                selected = "rapg"), 
                                     sliderInput("yrSliderD", "Select MLB Seasons", 
                                                 min = 1981, 
                                                 max = 2021, 
                                                 value = c(1981, 2021), 
                                                 sep = '')),
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
                    
                    
                    width=12
                  )),
                 fluidRow(
                  box(plotOutput("plot1")),
                  
                  box(dataTableOutput("table1"))

              )
        ),
        
        tabItem(tabName = "Modeling",
                h2("Modeling Info!")
        ),
        
        tabItem(tabName = "Data",
                h2("Data Content!")
        )
      )
    )
  ))
    