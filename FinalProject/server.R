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

#assign current divisions
teamsSubset$divID[teamsSubset$franchID == "STL"] <- "Central"
teamsSubset$divID[teamsSubset$franchID == "CHC"] <- "Central"
teamsSubset$divID[teamsSubset$franchID == "MIL"] <- "Central"
teamsSubset$divID[teamsSubset$franchID == "PIT"] <- "Central"
teamsSubset$divID[teamsSubset$franchID == "CIN"] <- "Central"
teamsSubset$divID[teamsSubset$franchID == "CLE"] <- "Central"
teamsSubset$divID[teamsSubset$franchID == "CHW"] <- "Central"
teamsSubset$divID[teamsSubset$franchID == "MIN"] <- "Central"
teamsSubset$divID[teamsSubset$franchID == "DET"] <- "Central"
teamsSubset$divID[teamsSubset$franchID == "KCR"] <- "Central"
teamsSubset$divID[teamsSubset$franchID == "ATL"] <- "East"
teamsSubset$divID[teamsSubset$franchID == "NYM"] <- "East"
teamsSubset$divID[teamsSubset$franchID == "PHI"] <- "East"
teamsSubset$divID[teamsSubset$franchID == "FLA"] <- "East"
teamsSubset$divID[teamsSubset$franchID == "WSN"] <- "East"
teamsSubset$divID[teamsSubset$franchID == "NYY"] <- "East"
teamsSubset$divID[teamsSubset$franchID == "TOR"] <- "East"
teamsSubset$divID[teamsSubset$franchID == "TBD"] <- "East"
teamsSubset$divID[teamsSubset$franchID == "BAL"] <- "East"
teamsSubset$divID[teamsSubset$franchID == "BOS"] <- "East"
teamsSubset$divID[teamsSubset$franchID == "LAD"] <- "West"
teamsSubset$divID[teamsSubset$franchID == "SDP"] <- "West"
teamsSubset$divID[teamsSubset$franchID == "SFG"] <- "West"
teamsSubset$divID[teamsSubset$franchID == "ARI"] <- "West"
teamsSubset$divID[teamsSubset$franchID == "COL"] <- "West"
teamsSubset$divID[teamsSubset$franchID == "HOU"] <- "West"
teamsSubset$divID[teamsSubset$franchID == "SEA"] <- "West"
teamsSubset$divID[teamsSubset$franchID == "ANA"] <- "West"
teamsSubset$divID[teamsSubset$franchID == "TEX"] <- "West"
teamsSubset$divID[teamsSubset$franchID == "OAK"] <- "West"


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
         -teamIDretro, -Rank, -CG, -Ghome, -teamID, 
         -ER, -CG, -SHO, -SV, -SOA, -R, -H, -HR, -BB,
         -RA, -HA, -HRA, -BBA, -E, -park, -G, -attendance)



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

 #create plot1 conditionally based on input stat.
 output$plot1 <- renderPlot({ 
   
   if(input$stat == "offStat") {
     
     offData <- teamSubsetFinal %>%
       filter((yearID <= input$yrSliderO[2]) & (yearID >input$yrSliderO[1]))
     
     if(input$offense == "rpg") {
      o1 <- ggplot(offData, aes(x = winPct, y = rpg))
      o1 + geom_point(size = 2, aes(color = LgWin, shape = WSWin)) + 
        ggtitle("Runs Scored Per Game") + 
        theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
        scale_color_hue(h = c(240, 5)) + 
        labs(x = "Proportion of Games Won", 
             y = "Average Runs Scored Per Game")
     } else if(input$offense == "hpg") {
       o2 <- ggplot(offData, aes(x = winPct, y = hpg))
       o2 + geom_point(size = 2, aes(color = LgWin, shape = WSWin)) +
         ggtitle("Hits Per Game") +
         theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
         scale_color_hue(h = c(240, 5)) +
         labs(x = "Proportion of Games Won", 
              y = "Average Hits Per Game")
     } else if(input$offense == "tbpg"){
       o3 <- ggplot(offData, aes(x = winPct, y = tbpg))
       o3 + geom_point(size = 2, aes(color = LgWin, shape = WSWin)) +
         ggtitle("Total Bases Per Game") + 
         theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
         scale_color_hue(h = c(240, 5)) +
         labs(x = "Proportion of Games Won", 
              y = "Total Bases Per Game")
     } else if(input$offense == "hrpg") {
       o4 <- ggplot(offData, aes(x = winPct, y = hrpg))
       o4 + geom_point(size = 2, aes(color = LgWin, shape = WSWin)) + 
         ggtitle("Home Runs Per Game") + 
         theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
         scale_color_hue(h = c(240, 5)) +
         labs(x = "Proportion of Games Won", 
              y = "Home Runs Per Game")
     } else if(input$offense == "bbpg") {
       o5 <- ggplot(offData, aes(x = winPct, y = bbpg))
       o5 + geom_point(size = 2, aes(color = LgWin, shape = WSWin)) +
         ggtitle("Walks Per Game") +
         theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
         scale_color_hue(h = c(240, 5)) +
         labs(x = "Proportion of Games Won", 
              y = "Walks Per Game")
     }
      
   } else if(input$stat == "defStat") {
    
     defData <- teamSubsetFinal %>%
       filter((yearID <= input$yrSliderD[2]) & (yearID >input$yrSliderD[1]))
    
       if (input$defense == "rapg") {
         d1 <- ggplot(defData, aes(x = winPct, y = rapg))
         d1 + geom_point(size = 2, aes(color = LgWin, shape = WSWin)) +
           ggtitle("Runs Allowed Per Game") + 
           theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
           scale_color_hue(h = c(240, 5)) +
           labs(x = "Proportion of Games Won", 
                y = "Runs Allowed Per Game")
       } else if(input$defense == "ERA") {
         d2 <- ggplot(defData, aes(x=winPct, y = ERA))
         d2 + geom_point(size = 2, aes(color = LgWin, shape = WSWin)) +
           ggtitle("ERA") + 
           theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
           scale_color_hue(h = c(240, 5)) +
           labs(x = "Proportion of Games Won", 
                y = "ERA")
       } else if(input$defense == "hapg") {
         d3 <- ggplot(defData, aes(x=winPct, y = hapg))
         d3 + geom_point(size = 2, aes(color = LgWin, shape = WSWin)) + 
           ggtitle("Hits Allowed Per Game") + 
           theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
           scale_color_hue(h = c(240, 5)) +
           labs(x = "Proportion of Games Won", 
                y = "Hits Allowed Per Game")
       } else if(input$defense == "hrapg") {
         d4 <- ggplot(defData, aes(x=winPct, y = hrapg))
         d4 + geom_point(size = 2, aes(color = LgWin, shape = WSWin)) + 
           ggtitle("Home Runs Allowed Per Game") + 
           theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
           scale_color_hue(h = c(240, 5)) +
           labs(x = "Proportion of Games Won", 
                y = "Home Runs Allowed Per Game")
       } else if(input$defense == "bbapg") {
         d5 <- ggplot(defData, aes(x=winPct, y = bbapg))
         d5 + geom_point(size = 2, aes(color = LgWin, shape = WSWin)) +
           ggtitle("Walks Allowed Per Game") + 
           theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
           scale_color_hue(h = c(240, 5)) +
           labs(x = "Proportion of Games Won", 
                y = "Walks Allowed Per Game")
       } else if(input$defense == "epg") {
         d6 <- ggplot(defData, aes(x=winPct, y = epg))
         d6 + geom_point(size = 2, aes(color = LgWin, shape = WSWin)) +
           ggtitle("Errors Per Game") + 
           theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
           scale_color_hue(h = c(240, 5)) +
           labs(x = "Proportion of Games Won", 
                y = "Errors Per Game")
       } else if(input$defense == "FP") {
         d7 <- ggplot(defData, aes(x=winPct, y = FP))
         d7 + geom_point(size = 2, aes(color = LgWin, shape = WSWin)) +
           ggtitle("Fielding Percentage") + 
           theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
           scale_color_hue(h = c(240, 5)) +
           labs(x = "Proportion of Games Won", 
                y = "Fielding Percentage")
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
   else if (input$stat == "frSum") { 
     
     leagueVar <- input$lg
     divisionVar <- input$div
     
     frSumData <- teamSubsetFinal %>%
       group_by(team) %>%
       filter(currentLeague == leagueVar & divID == divisionVar) 
     
     frSumPlot <- ggplot(data = frSumData, aes(x = team, y = winPct)) 
     frSumPlot + geom_boxplot()
     
   }
 })
 
 #create data table conditionally
 output$table1 <- renderDataTable({ 
   
    if(input$stat == "offStat") {
   
      offVar <- input$offense
     
      off1 <- teamSubsetFinal %>%
         filter(LgWin == "Y") %>%
         filter((yearID <= input$yrSliderO[2]) & (yearID >= input$yrSliderO[1])) %>%
         mutate(across(c(offVar, winPct), round, 2)) %>%
         select(yearID, WSWin, team, offVar, winPct) %>%
         rename("Year" = yearID, "World Series Winner?" = WSWin, 
                "Franchise Name" = team, "Proportion of Games Won" = winPct)
   
      datatable(off1)
    } else if(input$stat == "defStat") {
      
      defVar <- input$defense
      
      def1 <- teamSubsetFinal %>%
        filter(LgWin == "Y") %>%
        filter((yearID <= input$yrSliderD[2]) & (yearID >= input$yrSliderD[1])) %>%
        mutate(across(c(defVar, winPct), round, 2)) %>%
        select(yearID, WSWin, team, defVar, winPct) %>%
        rename("Year" = yearID, "World Series Winner?" = WSWin, 
               "Franchise Name" = team, "Proportion of Games Won" = winPct)
      
      datatable(def1)
      
    } else if(input$stat == "frPer") {
      
      teamVar <- input$perTeam
    
      per1 <- teamSubsetFinal %>%
        filter(team == teamVar) %>%
        mutate(across(c(winPct), round, 2)) %>%
        select(team, yearID, WSWin, winPct) %>%
        arrange(desc(winPct)) %>%
        rename("Year" = yearID, "World Series Winner?" = WSWin, 
             "Franchise Name" = team, "Proportion of Games Won" = winPct)
    
      datatable(per1)
    }
   
   
   
   else if(input$stat == "frSum") {
     
      lgVar <- input$lg

      teamWLSummary <- teamSubsetFinal %>%
        group_by(team) %>%
        filter((currentLeague == lgVar)) %>%
        mutate(winningSeason = ifelse(winPct > 0.5, 1, 0), 
              losingSeason = ifelse(winPct <= 0.5, 1, 0), 
              worldSeries = ifelse(WSWin == "Y", 1, 0), 
              leagueW = ifelse(LgWin == "Y", 1, 0)) %>%
        summarise(totalSeasons = sum(winningSeason, losingSeason),
                 winningSeasons = sum(winningSeason), 
                 losingSeasons = sum(losingSeason), 
                 winningSeasonPct = winningSeasons/(losingSeasons+winningSeasons) * 100, 
                 leagueWins = sum(leagueW), 
                 worldSeriesWins = sum(worldSeries)) %>%
        select(team, winningSeasonPct, leagueWins, worldSeriesWins) %>%
        mutate(mutate(across(c(winningSeasonPct), round, 2))) %>%
        arrange(desc(winningSeasonPct)) %>%
        rename("Franchise Name" = team, "Percentage of Winning Seasons" = winningSeasonPct, 
               "League Championships" = leagueWins, "World Series Titles" = worldSeriesWins)

      datatable(teamWLSummary)
}
   
})

})
