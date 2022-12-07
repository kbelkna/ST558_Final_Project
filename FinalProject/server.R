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
  filter(yearID>1980 & yearID<2022) 

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


# rename lgID to league & name to team
teamsSubset <- rename(teamsSubset, league = lgID,
                      team = name, division = divID,
                      fieldingPct = FP, 
                      worldSeriesWin = WSWin, 
                      leagueWin = LgWin)

#adding new columns
teamSubsetMutate <- teamsSubset %>%
  mutate(pctGamesWon = W/(W+L)*100, 
         avgRunsScored = R/G, 
         avgHits = H/G,
         avgTotalBases = ((H + X2B + 2*X3B + 3*HR)/G), 
         avgHomeRuns = HR/G,
         avgWalks = BB/G,
         avgRunsAllowed = RA/G,
         avgHitsAllowed = HA/G,
         avgHomeRunsAllowed = HRA/G,
         avgWalksAllowed = BBA/G,
         avgErrors = E/G) %>%
  rename(year = yearID)

#removing columns that won't be used in final analysis
teamSubsetFinal <- teamSubsetMutate %>%
  select(-W, -L, -WCWin, -DivWin, -X2B, -X3B, 
         -AB, -SB, -CS, -HBP, -SF, -IPouts, -SO,
         -DP, -BPF, -PPF, -teamIDBR, -teamIDlahman45, 
         -teamIDretro, -Rank, -CG, -Ghome, -teamID, 
         -ER, -CG, -SHO, -SV, -SOA, -R, -H, -HR, -BB,
         -RA, -HA, -HRA, -BBA, -E, -park, -G, -attendance)

col_order <- c("year", "league", "division",
               "team", "pctGamesWon", "avgRunsScored", "avgHits", 
               "avgTotalBases", "avgHomeRuns", "avgWalks", "avgRunsAllowed", 
               "ERA", "avgHitsAllowed", "avgHomeRunsAllowed", "avgWalksAllowed", 
               "avgErrors", "fieldingPct", "leagueWin", "worldSeriesWin")

teamSubsetFinal_reorder <- teamSubsetFinal[ , col_order]

userDataRaw <- teamSubsetFinal_reorder %>%
  mutate(across(is.numeric, round, 2)) 





# Define server logic 
shinyServer(function(input, output, session) {
  
  #create markdown file to be used for about section
  output$mymarkdown <- renderUI({  
    rmarkdown::render(input = "about.Rmd",
                      output_file = 'about.html')  
    shiny::includeHTML('about.html') 
  }) 
  

 #create plot1 conditionally based on input stat.
 output$plot1 <- renderPlot({ 
   
   if(input$stat == "offStat") {
     
     offData <- teamSubsetFinal %>%
       filter((year <= input$yrSliderO[2]) & (year >input$yrSliderO[1]))
     
     if(input$offense == "avgRunsScored") {
      o1 <- ggplot(offData, aes(x = pctGamesWon, y = avgRunsScored))
      o1 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) + 
        ggtitle("Runs Scored Per Game") + 
        theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
        scale_color_hue(h = c(240, 5)) + 
        labs(x = "Percentage of Games Won", 
             y = "Average Runs Scored Per Game")
     } else if(input$offense == "avgHits") {
       o2 <- ggplot(offData, aes(x = pctGamesWon, y = avgHits))
       o2 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) +
         ggtitle("Hits Per Game") +
         theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
         scale_color_hue(h = c(240, 5)) +
         labs(x = "Percentage of Games Won", 
              y = "Average Hits Per Game")
     } else if(input$offense == "avgTotalBases"){
       o3 <- ggplot(offData, aes(x = pctGamesWon, y = avgTotalBases))
       o3 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) +
         ggtitle("Total Bases Per Game") + 
         theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
         scale_color_hue(h = c(240, 5)) +
         labs(x = "Percentage of Games Won", 
              y = "Total Bases Per Game")
     } else if(input$offense == "avgHomeRuns") {
       o4 <- ggplot(offData, aes(x = pctGamesWon, y = avgHomeRuns))
       o4 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) + 
         ggtitle("Home Runs Per Game") + 
         theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
         scale_color_hue(h = c(240, 5)) +
         labs(x = "Percentage of Games Won", 
              y = "Home Runs Per Game")
     } else if(input$offense == "avgWalks") {
       o5 <- ggplot(offData, aes(x = pctGamesWon, y = avgWalks))
       o5 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) +
         ggtitle("Walks Per Game") +
         theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
         scale_color_hue(h = c(240, 5)) +
         labs(x = "Percentage of Games Won", 
              y = "Walks Per Game")
     }
      
   } else if(input$stat == "defStat") {
    
     defData <- teamSubsetFinal %>%
       filter((year <= input$yrSliderD[2]) & (year >input$yrSliderD[1]))
    
       if (input$defense == "avgRunsAllowed") {
         d1 <- ggplot(defData, aes(x = pctGamesWon, y = avgRunsAllowed))
         d1 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) +
           ggtitle("Runs Allowed Per Game") + 
           theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
           scale_color_hue(h = c(240, 5)) +
           labs(x = "Percentage of Games Won", 
                y = "Runs Allowed Per Game")
       } else if(input$defense == "ERA") {
         d2 <- ggplot(defData, aes(x=pctGamesWon, y = ERA))
         d2 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) +
           ggtitle("ERA") + 
           theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
           scale_color_hue(h = c(240, 5)) +
           labs(x = "Percentage of Games Won", 
                y = "ERA")
       } else if(input$defense == "avgHitsAllowed") {
         d3 <- ggplot(defData, aes(x=pctGamesWon, y = avgHitsAllowed))
         d3 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) + 
           ggtitle("Hits Allowed Per Game") + 
           theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
           scale_color_hue(h = c(240, 5)) +
           labs(x = "Percentage of Games Won", 
                y = "Hits Allowed Per Game")
       } else if(input$defense == "avgHomeRunsAllowed") {
         d4 <- ggplot(defData, aes(x=pctGamesWon, y = avgHomeRunsAllowed))
         d4 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) + 
           ggtitle("Home Runs Allowed Per Game") + 
           theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
           scale_color_hue(h = c(240, 5)) +
           labs(x = "Percentage of Games Won", 
                y = "Home Runs Allowed Per Game")
       } else if(input$defense == "avgWalksAllowed") {
         d5 <- ggplot(defData, aes(x=pctGamesWon, y = avgWalksAllowed))
         d5 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) +
           ggtitle("Walks Allowed Per Game") + 
           theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
           scale_color_hue(h = c(240, 5)) +
           labs(x = "Percentage of Games Won", 
                y = "Walks Allowed Per Game")
       } else if(input$defense == "avgErrors") {
         d6 <- ggplot(defData, aes(x=pctGamesWon, y = avgErrors))
         d6 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) +
           ggtitle("Errors Per Game") + 
           theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
           scale_color_hue(h = c(240, 5)) +
           labs(x = "Percentage of Games Won", 
                y = "Errors Per Game")
       } else if(input$defense == "fieldingPct") {
         d7 <- ggplot(defData, aes(x=pctGamesWon, y = fieldingPct))
         d7 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) +
           ggtitle("Fielding Percentage") + 
           theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
           scale_color_hue(h = c(240, 5)) +
           labs(x = "Percentage of Games Won", 
                y = "Fielding Percentage")
       }
         
   } else if(input$stat == "frPer") {
     
     
      teamSubsetFinal2 <- teamSubsetFinal %>%
        filter(team == input$perTeam)
     
      n <- ggplot(teamSubsetFinal2, aes(x=year, y=pctGamesWon))
      n + geom_line(color = "red") + 
        geom_point(size = 4, aes(shape = worldSeriesWin, color = leagueWin)) + 
        theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
        labs(title = paste0("Percentage of Games Won by Year for ", teamSubsetFinal2$team),
            x = "Year", 
            y = "Percentage of Games Won")
     
     } 
   else if (input$stat == "frSum") { 
     
     leagueVar <- input$lg
     divisionVar <- input$div
     
     frSumData <- teamSubsetFinal %>%
       group_by(team) %>%
       filter(league == leagueVar & division == divisionVar) 
     
     frSumPlot <- ggplot(data = frSumData, aes(x = team, y = pctGamesWon)) 
     frSumPlot + geom_boxplot() + 
       theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
       labs(title = paste0("Percentage of Games Won from 1981 - 2021 for teams in the ", leagueVar, " ", divisionVar), 
            x = "Team", 
            y = "Percentage of Games Won by Season")
     
   }
 })
 
 #create data table conditionally
 output$table1 <- renderDataTable({ 
   
    if(input$stat == "offStat") {
   
      offVar <- input$offense
     
      off1 <- teamSubsetFinal %>%
         filter((year <= input$yrSliderO[2]) & (year >= input$yrSliderO[1])) %>%
         mutate(across(c(offVar, pctGamesWon), round, 2)) %>%
         select(year, worldSeriesWin, team, offVar, pctGamesWon) %>%
         rename("Year" = year, "World Series Winner?" = worldSeriesWin, 
                "Franchise Name" = team, "Percentage of Games Won" = pctGamesWon)
   
      datatable(off1, caption = htmltools::tags$caption(style = 'caption-side: top;
                                                text-align: left;
                                                color:black;
                                                font-size:20px;
                                                font-weight: bold;', "Offensive Statistics"))
      
    } else if(input$stat == "defStat") {
      
      defVar <- input$defense
      
      def1 <- teamSubsetFinal %>%
        filter((year <= input$yrSliderD[2]) & (year >= input$yrSliderD[1])) %>%
        mutate(across(c(defVar, pctGamesWon), round, 2)) %>%
        select(year, worldSeriesWin, team, defVar, pctGamesWon) %>%
        rename("Year" = year, "World Series Winner?" = worldSeriesWin, 
               "Franchise Name" = team, "Percentage of Games Won" = pctGamesWon)
      
      datatable(def1, caption = htmltools::tags$caption(style = 'caption-side: top;
                                                text-align: left;
                                                color:black;
                                                font-size:20px;
                                                font-weight: bold;', "Defensive Statistics"))
      
    } else if(input$stat == "frPer") {
      
      teamVar <- input$perTeam
    
      per1 <- teamSubsetFinal %>%
        filter(team == teamVar) %>%
        mutate(across(c(pctGamesWon), round, 2)) %>%
        select(team, year, worldSeriesWin, pctGamesWon) %>%
        arrange(desc(pctGamesWon)) %>%
        rename("Year" = year, "World Series Winner?" = worldSeriesWin, 
             "Franchise Name" = team, "Percentage of Games Won" = pctGamesWon)
    
      datatable(per1, caption = htmltools::tags$caption(style = 'caption-side: top;
                                                text-align: left;
                                                color:black;
                                                font-size:20px;
                                                font-weight: bold;', "Individual Team Performance"))
    } else if(input$stat == "frSum") {
     
      lgVar <- input$lg

      teamWLSummary <- teamSubsetFinal %>%
        group_by(team) %>%
        filter((league == lgVar)) %>%
        mutate(winningSeason = ifelse(pctGamesWon > 50, 1, 0), 
              losingSeason = ifelse(pctGamesWon <= 50, 1, 0), 
              worldSeries = ifelse(worldSeriesWin == "Y", 1, 0), 
              leagueW = ifelse(leagueWin == "Y", 1, 0)) %>%
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

      datatable(teamWLSummary, caption = htmltools::tags$caption(style = 'caption-side: top;
                                                text-align: left;
                                                color:black;
                                                font-size:20px;
                                                font-weight: bold;', "League Statistics"))
}
   
})
 
 c("year", "league", "division",
   "team", "pctGamesWon", "avgRunsScored", "avgHits", 
   "avgTotalBases", "avgHomeRuns", "avgWalks", "avgRunsAllowed", 
   "ERA", "avgHitsAllowed", "avgHomeRunsAllowed", "avgWalksAllowed", 
   "avgErrors", "fieldingPct", "leagueWin", "worldSeriesWin")
 
 
 
 userDataSet <- reactive({
   
   simpleStats <- c("year", "league", "division",
                    "team", "pctGamesWon", "leagueWin", "worldSeriesWin")
   offStats <- c("avgRunsScored", "avgHits", 
                 "avgTotalBases", "avgHomeRuns", "avgWalks")
   defStats <- c("avgRunsAllowed", 
                 "ERA", "avgHitsAllowed", "avgHomeRunsAllowed", "avgWalksAllowed", 
                 "avgErrors", "fieldingPct")
   

   if(input$userStat == "allStat") {
     
     userDataAll <- userDataRaw %>%
       select(simpleStats, offStats, defStats)
     
      if(input$userTeamSelect == 'TRUE' & input$userYrSelect == 'TRUE') {
        userDataAll2 <- userDataAll %>%
          filter(team == input$userTeam & 
                (year <= input$userYrSlider[2]) & (year >= input$userYrSlider[1]))
        return(userDataAll2)
      } else if(input$userTeamSelect == 'TRUE' & input$userYrSelect == 'FALSE') {
        userDataAll3 <- userDataAll %>%
          filter(team == input$userTeam)
        return(userDataAll3)
      } else if(input$userTeamSelect == 'FALSE' & input$userYrSelect == 'TRUE') {
        userDataAll4 <- userDataAll %>%
          filter((year <= input$userYrSlider[2]) & (year >= input$userYrSlider[1]))
        return(userDataAll4)
      } else {
          return(userDataAll)
      }
     
   } else if(input$userStat == "offStat") {
     
     userDataOff <- userDataRaw %>%
       select(simpleStats, offStats)

     if(input$userTeamSelect == 'TRUE' & input$userYrSelect == 'TRUE') {
       userDataOff2 <- userDataOff %>%
         filter(team == input$userTeam & 
                  (year <= input$userYrSlider[2]) & (year >= input$userYrSlider[1]))
       return(userDataOff2)
     } else if(input$userTeamSelect == 'TRUE' & input$userYrSelect == 'FALSE') {
       userDataOff3 <- userDataOff %>%
         filter(team == input$userTeam)
       return(userDataOff3)
     } else if(input$userTeamSelect == 'FALSE' & input$userYrSelect == 'TRUE') {
       userDataOff4 <- userDataAll %>%
         filter((year <= input$userYrSlider[2]) & (year >= input$userYrSlider[1]))
       return(userDataOff4)
     } else {
       return(userDataOff)
     }
     
   } else if(input$userStat == "defStat") {
     
     userDataDef <- userDataRaw %>%
       select(simpleStats, defStats)
     
     if(input$userTeamSelect == 'TRUE' & input$userYrSelect == 'TRUE') {
       userDataDef2 <- userDataDef %>%
         filter(team == input$userTeam & 
                  (year <= input$userYrSlider[2]) & (year >= input$userYrSlider[1]))
       return(userDataDef2)
     } else if(input$userTeamSelect == 'TRUE' & input$userYrSelect == 'FALSE') {
       userDataDef3 <- userDataDef %>%
         filter(team == input$userTeam)
       return(userDataDef3)
     } else if(input$userTeamSelect == 'FALSE' & input$userYrSelect == 'TRUE') {
       userDataDef4 <- userDataDef %>%
         filter((year <= input$userYrSlider[2]) & (year >= input$userYrSlider[1]))
       return(userDataDef4)
     } else {
       return(userDataDef)
     }

     
   }
     
   
 })
 
 #Display dataframe in table
 output$userData <- renderDataTable({
   datatable(userDataSet(), options = list(scrollX = TRUE))
 })
 
 #Download dataframe
 output$downloadData <- downloadHandler(
   filename = "test.csv",
   content = function(file) {
     write.csv(userDataSet(), file, row.names = FALSE)
   }
 )

})

