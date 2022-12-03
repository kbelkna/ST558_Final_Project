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
dashboardPage(skin = "red",
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
                                                 choices = c("Runs Per Game" = "RPG",
                                                             "Hits Per Game" = "HPG",
                                                             "Total Bases Per Game" = "TBPG",
                                                             "Home Runs Per Game" = "HRPG", 
                                                             "Walks Per Game" = "BBPG"), 
                                                 selected = "RPG")),
                    conditionalPanel(condition = "input.stat == 'defStat'", 
                                     selectInput("defense", "Select Defensive Statistic", 
                                                 choices = c("Runs Allowed Per Game" = "RAPG",
                                                             "Earned Runs Allowed (ERA)" = "ERA",
                                                             "Hits Allowed Per Game" = "HAPG",
                                                             "Home Runs Allowed Per Game" = "HRAPG", 
                                                             "Walks Allowed Per Game" = "BBAPG",
                                                             "Errors Per Game" = "EPG", 
                                                             "Fielding Percentage" = "FP"),
                                                selected = "RAPG")),
                    conditionalPanel(condition = "input.stat == 'frPer'", 
                                     selectInput("perTeam", "Select MLB Franchise", 
                                                 choices = levels(as.factor(teamSubsetFinal$team)), 
                                                 selected = "St. Louis Cardinals")),
                    sliderInput("slider", "Number of observations:", 1, 100, 50), width = 4
                  ),
                  
                  box(plotOutput("plot1", height = 400), width = 8)

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
    