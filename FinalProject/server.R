#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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


data("Teams")

#Narrow Data down to 1981-2021
teamsSubset <- Teams %>%
  filter(yearID>1980) 

#Changing all Angels teams to "Los Angeles Angels"
teamsSubset$name[teamsSubset$name == "California Angels"] <- "Los Angeles Angels"
teamsSubset$name[teamsSubset$name == "Anaheim Angels"] <- "Los Angeles Angels"
teamsSubset$name[teamsSubset$name == "Los Angeles Angels of Anaheim"] <- "Los Angeles Angels"

#Changing Tampa Bay Devil Rays to "Tampa Bay Rays"
teamsSubset$name[teamsSubset$name == "Tampa Bay Devil Rays"] <- "Tampa Bay Rays"

#Changing Florida Marlins to "Miami Marlins"
teamsSubset$name[teamsSubset$name == "Florida Marlins"] <- "Miami Marlins"

#Changing Cleveland Indians to Cleveland Guardians.
teamsSubset$name[teamsSubset$name == "Cleveland Indians"] <- "Cleveland Guardians"

#Montreal Expos moved to Washington Nationals in 2005. For Simplicity, merging these teams under Washington Nationals since they belong to the same franchise.... 
teamsSubset$name[teamsSubset$name == "Montreal Expos"] <- "Washington Nationals"

# No Post-Season play in 1994. Replacing NA values with "N". 
teamsSubset$WSWin[is.na(teamsSubset$WSWin)] <- "N"
teamsSubset$LgWin[is.na(teamsSubset$LgWin)] <- "N"

# Change Astros & Brewers to their respective current leagues
teamsSubset$lgID[teamsSubset$franchID == "HOU"] <- "AL"
teamsSubset$lgID[teamsSubset$franchID == "MIL"] <- "NL"

# rename lgID to currentLeague & name to team
teamsSubset <- rename(teamsSubset, currentLeague = lgID,
                      team = name)

#adding new columns
teamSubsetMutate <- teamsSubset %>%
  mutate(winPct = W/(W+L), 
         rpg = R/G, 
         hpg = H/G,
         tbpg = ((H + X2B + 2*X3B + 3*HR)/G), 
         hrpg = HR/G,
         bbpg = BB/G,
         avgAttendance = attendance/Ghome, 
         rapg = RA/G,
         hapg = HA/G,
         hrapg = HRA/G,
         bbapg = BBA/G,
         epg = E/G)

#removing columns that won't be used in final analysis
teamSubsetFinal <- teamSubsetMutate %>%
  select(-W, -L, -WCWin, -DivWin, -X2B, -X3B, 
         -AB, -SB, -CS, -HBP, -SF, -IPouts, -SO,
         -DP, -BPF, -PPF, -teamIDBR, -teamIDlahman45, 
         -teamIDretro, -divID, -Rank, -CG, -Ghome, -teamID, 
         -ER, -CG, -SHO, -SV, -SOA, -R, -H, -HR, -BB,
         -RA, -HA, -HRA, -BBA, -E, -park, -G, -attendance)



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  set.seed(122)
  histdata <- rnorm(500)

 output$plot1 <- renderPlot({ 
   
   if(input$stat == "offStat"){
     
     if(input$offense == "RPG"){
      o1 <- ggplot(teamSubsetFinal, aes(x = winPct, y = rpg))
      o1 + geom_point(size = 2, aes(color = WSWin)) + 
        ggtitle("Runs Scored Per Game") + 
        theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
        labs(x = "Proportion of Games Won", 
             y = "Average Runs Scored Per Game")
     } else if(input$offense == "HPG"){
       o2 <- ggplot(teamSubsetFinal, aes(x = winPct, y = hpg))
       o2 + geom_point(aes(color = WSWin)) +
         ggtitle("Hits Per Game") +
         theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
         labs(x = "Proportion of Games Won", 
              y = "Average Hits Per Game")
     } else if(input$offense == "TBPG"){
       o3 <- ggplot(teamSubsetFinal, aes(x = winPct, y = tbpg))
       o3 + geom_point(aes(color = WSWin)) +
         ggtitle("Total Bases Per Game")
         theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
         labs(x = "Proportion of Games Won", 
              y = "Total Bases Per Game")
     } else if(input$offense == "HRPG"){
       o4 <- ggplot(teamSubsetFinal, aes(x = winPct, y = hrpg))
       o4 + geom_point(aes(color = WSWin)) + 
         ggtitle("Home Runs Per Game")
         theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
         labs(x = "Proportion of Games Won", 
              y = "Home Runs Per Game")
     } else if(input$offense == "BBPG"){
       o5 <- ggplot(teamSubsetFinal, aes(x = winPct, y = bbpg))
       o5 + geom_point(aes(color = WSWin)) +
         ggtitle("Walks Per Game") +
         theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
         labs(x = "Proportion of Games Won", 
              y = "Walks Per Game")
     }
      
   } else if(input$stat == "defStat"){
       if (input$defense == "RAPG") {
         d1 <- ggplot(teamSubsetFinal, aes(x = winPct, y = rapg))
         d1 + geom_point(aes(color = WSWin))
       } else if(input$defense == "ERA") {
         d2 <- ggplot(teamSubsetFinal, aes(x=winPct, y = ERA))
         d2 + geom_point(aes(color = WSWin))
       } else if(input$defense == "HAPG") {
         d3 <- ggplot(teamSubsetFinal, aes(x=winPct, y = hapg))
         d3 + geom_point(aes(color = WSWin))
       } else if(input$defense == "HRAPG") {
         d4 <- ggplot(teamSubsetFinal, aes(x=winPct, y = hrapg))
         d4 + geom_point(aes(color = WSWin))
       } else if(input$defense == "BBAPG") {
         d5 <- ggplot(teamSubsetFinal, aes(x=winPct, y = bbapg))
         d5 + geom_point(aes(color = WSWin))
       } else if(input$defense == "EPG") {
         d6 <- ggplot(teamSubsetFinal, aes(x=winPct, y = epg))
         d6 + geom_point(aes(color = WSWin))
       } else if(input$defense == "FP") {
         d7 <- ggplot(teamSubsetFinal, aes(x=winPct, y = FP))
         d7 + geom_point(aes(color = WSWin))
       }
         
   } else if(input$stat == "frPer") {
     teamSubsetFinal2 <- teamSubsetFinal %>%
       filter(team == input$perTeam)
     
     n <- ggplot(teamSubsetFinal2, aes(x=yearID, y=winPct))
     n + geom_line(color = "red") + 
       geom_point(size = 4, aes(shape = WSWin, color = LgWin)) + 
       labs(title = paste0("Proportion of Games Won by Year for ", teamSubsetFinal2$team),
            x = "Year", 
            y = "Proportion of Games Won")
       } 
   
   
   
   
   
   else { 
      o2 <- ggplot(teamSubsetFinal, aes(x = winPct, y = hpg))
      o2 + geom_point(aes(color = WSWin))
     
     
   }
   
   
   
   
 })
  
  
#  output$plot1 <- renderPlot({
#    data <- histdata[seq_len(input$slider)]
#    hist(data)
#    })


})
