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
library(mathjaxr)
library(caret)
library(randomForest)


# Define server logic 
server <- shinyServer(function(input, output, session) {
  
  teamSubsetFinal <- read_csv("teamSubsetFinal.csv")
  userDataRaw <- read_csv("userDataRaw.csv")
  
  
#The following content is used for the about section:
  
  #create markdown file to be used for about section
  output$mymarkdown <- renderUI({  
    rmarkdown::render(input = "about.Rmd",
                      output_file = 'about.html')  
    shiny::includeHTML('about.html') 
  }) 
  

#The following content is used for the EDA section:
  
 #create plot1 conditionally based on input stat.
  output$plot1 <- renderPlot({ 
    #If the requested stat is offensive stat
    if(input$stat == "offStat") {
      #Create dataset offData with filtered years
      offData <- teamSubsetFinal %>%
        filter((year <= input$yrSliderO[2]) & (year >input$yrSliderO[1]))
      
      #Create plots based on requested statistic.
      if(input$offense == "avgRunsScored") {
        o1 <- ggplot(offData, aes(x = pctGamesWon, y = avgRunsScored))
           o1 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) + 
             ggtitle("Runs Scored Per Game") + 
             theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
             scale_color_hue(h = c(240, 5)) + 
             labs(x = "Percentage of Games Won", y = "Average Runs Scored Per Game")
           } else if(input$offense == "avgHits") {
             o2 <- ggplot(offData, aes(x = pctGamesWon, y = avgHits))
                o2 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) +
                ggtitle("Hits Per Game") +
                theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
                scale_color_hue(h = c(240, 5)) +
                labs(x = "Percentage of Games Won", y = "Average Hits Per Game")
                } else if(input$offense == "avgTotalBases"){
                  o3 <- ggplot(offData, aes(x = pctGamesWon, y = avgTotalBases))
                  o3 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) +
                    ggtitle("Total Bases Per Game") + 
                    theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
                    scale_color_hue(h = c(240, 5)) +
                    labs(x = "Percentage of Games Won", y = "Total Bases Per Game")
                  } else if(input$offense == "avgHomeRuns") {
                    o4 <- ggplot(offData, aes(x = pctGamesWon, y = avgHomeRuns))
                    o4 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) + 
                      ggtitle("Home Runs Per Game") + 
                      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
                      scale_color_hue(h = c(240, 5)) +
                      labs(x = "Percentage of Games Won", y = "Home Runs Per Game")
                    } else if(input$offense == "avgWalks") {
                      o5 <- ggplot(offData, aes(x = pctGamesWon, y = avgWalks))
                      o5 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) +
                        ggtitle("Walks Per Game") +
                        theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
                        scale_color_hue(h = c(240, 5)) +
                        labs(x = "Percentage of Games Won", y = "Walks Per Game")
                    }
      }
    #If user selects defensive stats
    else if(input$stat == "defStat") {
      defData <- teamSubsetFinal %>%
        filter((year <= input$yrSliderD[2]) & (year >input$yrSliderD[1]))
      
      #Create plot based on defensive stat selected.
      if (input$defense == "avgRunsAllowed") {
        d1 <- ggplot(defData, aes(x = pctGamesWon, y = avgRunsAllowed))
        d1 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) +
          ggtitle("Runs Allowed Per Game") + 
          theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
          scale_color_hue(h = c(240, 5)) +
          labs(x = "Percentage of Games Won", y = "Runs Allowed Per Game")
        } else if(input$defense == "ERA") {
          d2 <- ggplot(defData, aes(x=pctGamesWon, y = ERA))
          d2 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) +
            ggtitle("ERA") + 
            theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
            scale_color_hue(h = c(240, 5)) +
            labs(x = "Percentage of Games Won", y = "ERA")
          } else if(input$defense == "avgHitsAllowed") {
            d3 <- ggplot(defData, aes(x=pctGamesWon, y = avgHitsAllowed))
            d3 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) + 
              ggtitle("Hits Allowed Per Game") + 
              theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
              scale_color_hue(h = c(240, 5)) +
              labs(x = "Percentage of Games Won", y = "Hits Allowed Per Game")
            } else if(input$defense == "avgHomeRunsAllowed") {
              d4 <- ggplot(defData, aes(x=pctGamesWon, y = avgHomeRunsAllowed))
              d4 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) + 
                ggtitle("Home Runs Allowed Per Game") + 
                theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
                scale_color_hue(h = c(240, 5)) +
                labs(x = "Percentage of Games Won", y = "Home Runs Allowed Per Game")
              } else if(input$defense == "avgWalksAllowed") {
                d5 <- ggplot(defData, aes(x=pctGamesWon, y = avgWalksAllowed))
                d5 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) +
                  ggtitle("Walks Allowed Per Game") + 
                  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
                  scale_color_hue(h = c(240, 5)) +
                  labs(x = "Percentage of Games Won", y = "Walks Allowed Per Game")
                } else if(input$defense == "avgErrors") {
                  d6 <- ggplot(defData, aes(x=pctGamesWon, y = avgErrors))
                  d6 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) +
                    ggtitle("Errors Per Game") + 
                    theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
                    scale_color_hue(h = c(240, 5)) +
                    labs(x = "Percentage of Games Won", y = "Errors Per Game")
                  } else if(input$defense == "fieldingPct") {
                    d7 <- ggplot(defData, aes(x=pctGamesWon, y = fieldingPct))
                    d7 + geom_point(size = 2, aes(color = leagueWin, shape = worldSeriesWin)) +
                      ggtitle("Fielding Percentage") + 
                      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
                      scale_color_hue(h = c(240, 5)) +
                      labs(x = "Percentage of Games Won", y = "Fielding Percentage")
                    }
    }
    
    #If user selects performance
    else if(input$stat == "frPer") {
      teamSubsetFinal2 <- teamSubsetFinal %>%
        filter(team == input$perTeam)
     
      n <- ggplot(teamSubsetFinal2, aes(x=year, y=pctGamesWon))
      n + geom_line(color = "red") + 
        geom_point(size = 4, aes(shape = worldSeriesWin, color = leagueWin)) + 
        theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
        labs(title = paste0("Percentage of Games Won by Year for ", teamSubsetFinal2$team),
            x = "Year", y = "Percentage of Games Won")
      } 
    
    #if user selects summary
    else if (input$stat == "frSum") {
      
      leagueVar <- input$lg
      divisionVar <- input$div
     
      frSumData <- teamSubsetFinal %>%
        group_by(team) %>%
        filter(league == leagueVar & division == divisionVar) 
     
      frSumPlot <- ggplot(data = frSumData, aes(x = team, y = pctGamesWon)) 
      frSumPlot + geom_boxplot() + 
        theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
        labs(title = paste0("Season Winning Percentages from 1981-2021 for Teams in the ", leagueVar, " ", divisionVar), 
            x = "Team", y = "Percentage of Games Won by Season")
      }
    })
 
 #create data table conditionally
  output$table1 <- renderDataTable({
    
    #if stat is offsense
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
    } 
    
    #if stat is defense
    else if(input$stat == "defStat") {
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
      
      } 
    
    #if stat is performance
    else if(input$stat == "frPer") {
      
      teamVar <- input$perTeam
    
      per1 <- teamSubsetFinal %>%
        filter(team == teamVar) %>%
        mutate(across(c(pctGamesWon), round, 2)) %>%
        select(team, year, pctGamesWon, worldSeriesWin) %>%
        arrange(desc(pctGamesWon)) %>%
        rename("Year" = year, "World Series Winner?" = worldSeriesWin, 
             "Franchise Name" = team, "Percentage of Games Won" = pctGamesWon)
    
      datatable(per1, caption = htmltools::tags$caption(style = 'caption-side: top;
                                                text-align: left;
                                                color:black;
                                                font-size:20px;
                                                font-weight: bold;', "Individual Team Performance"))
      } 
    
    #if stat is summary
    else if(input$stat == "frSum") {
     
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
        rename("Franchise Name" = team, "Percentage of Winning Seasons (> 50%)" = winningSeasonPct, 
               "League Championships" = leagueWins, "World Series Titles" = worldSeriesWins)

      datatable(teamWLSummary, caption = htmltools::tags$caption(style = 'caption-side: top;
                                                text-align: left;
                                                color:black;
                                                font-size:20px;
                                                font-weight: bold;', "League Statistics"))
      }
    })
 
#The following code is used by the modeling section:  
  
  # Create and run linear model
  lmOutput <- eventReactive(input$runModel, {
    
    set.seed(555)
    modelingData <- teamSubsetFinal
    trainIndex <- createDataPartition(modelingData$pctGamesWon, p = isolate(input$trDataProp), list = FALSE)
    modelTrain <- modelingData[trainIndex, ]
    modelTest <- modelingData[-trainIndex, ]
    
    predictorVars <- paste(isolate(input$modelParams), collapse = "+")
    
    cvNumberLM <- ifelse(isolate(input$linearCV) == "five", 5, 10)
    
    lmFit = train(as.formula(paste('pctGamesWon ~', predictorVars)), data = modelTrain,
                  method="lm",
                  preProcess = c("center", "scale"),
                  trControl = trainControl(method = "CV", number = cvNumberLM))
    
    lm_out = data.frame(lmFit$results)
    
    lm_out_format <- lm_out %>%
      mutate_if(is.numeric, round, 3)
    
    tableTrain <- datatable(lm_out_format, caption = htmltools::tags$caption(style = 'caption-side: top;
                                                text-align: left;
                                                color:black;
                                                font-size:20px;
                                                font-weight: bold;', "Linear Model: Performance on Training Data"))
    
    lmResults = postResample(pred = predict(lmFit, newdata = modelTest), 
                             obs = modelTest$pctGamesWon)
    
    
    metric_lm_format <- data.frame(lmResults) %>%
      mutate_if(is.numeric, round, 3)
    
    tableTest <- datatable(metric_lm_format, caption = htmltools::tags$caption(style = 'caption-side: top;
                                                text-align: left;
                                                color:black;
                                                font-size:20px;
                                                font-weight: bold;', "Linear Model: Performance on Test Data"))

    list(modelTrain = modelTrain, modelTest = modelTest, lmFit = lmFit, tableTrain = tableTrain, tableTest = tableTest)
    
    
  })
  
  #output table for training data
  output$lmTableTrain <- renderDataTable({
    
     lmOutput()$tableTrain
    
    }) 
  
  #output table for test data
  output$lmTableTest <- renderDataTable({
    
    lmOutput()$tableTest
    
  })  
  
 # Create and run tree model
  treeOutput <- eventReactive(input$runModel, {
    
    predictorVars <- paste(isolate(input$modelParams), collapse = "+")
    
    cvNumberTree <- ifelse(isolate(input$tree1) == "five", 5, 10)

    train.control = trainControl(method = "cv", number = cvNumberTree)
    
    trShrinkage <- ifelse(isolate(input$treeShrink) == "std", 0.1, 0.001)
    
  
    tunG = expand.grid(n.trees = seq(25,isolate(input$treeN),25),
                     interaction.depth = 1:isolate(input$treeMax),
                     shrinkage = trShrinkage,
                     n.minobsinnode = isolate(input$treeObs))
  
    gbmFit <- train(as.formula(paste('pctGamesWon ~', predictorVars)),
                  data = lmOutput()$modelTrain,
                  method = "gbm",
                  preProcess = c("center","scale"),
                  trControl = train.control,
                  tuneGrid = tunG,
                  verbose = FALSE
  )
  
   gbm_out <- data.frame(gbmFit$results)
    gbm_out <- gbm_out %>%
      arrange(RMSE) %>%
      mutate_if(is.numeric, round, 3)
  
    treeResults <- datatable(gbm_out, options = list(scrollX = TRUE), 
                    caption = htmltools::tags$caption(style = 'caption-side: top;
                                                text-align: left;
                                                color:black;
                                                font-size:20px;
                                                font-weight: bold;', "Tree Model: Summary"))
  
    treePlot <- plot(gbmFit)
  
    gbm_pred <- predict(gbmFit, newdata = modelTest)
    trResults <- postResample(gbm_pred, modelTest$pctGamesWon)
    metric_boosting <- data.frame(trResults) %>%
      round(3) 

    treeTest <- datatable(metric_boosting, caption = htmltools::tags$caption(style = 'caption-side: top;
                                                text-align: left;
                                                color:black;
                                                font-size:20px;
                                                font-weight: bold;', "Tree Model: Performance on Training Data"))
  
    list(treeResults = treeResults, treePlot = treePlot, treeTest = treeTest)

 })
  
  #output table for training data
  output$treeResultsTable <- renderDataTable({
    
    treeOutput()$treeResults
    
  }) 
  
  #output table for training data
  output$treeResultsPlot <- renderPlot({
    
    treeOutput()$treePlot
    
  })
  
  #output table for test data
  output$treeTest <- renderDataTable({
    
    treeOutput()$treeTest
    
  })  
  
  
  # Create and run tree model
  rfOutput <- eventReactive(input$runModel, {
    
    predictorVars <- paste(isolate(input$modelParams), collapse = "+")
    
    cvNumberRF <- ifelse(isolate(input$rf1) == "five", 5, 10)
    
    mtryRF <- ifelse(isolate(input$rfMtry) == "two", 2, 3)
    
    train.control = trainControl(method = "cv", number = cvNumberRF)
    
    rfFit <- train(as.formula(paste('pctGamesWon ~', predictorVars)),
                   data = modelTrain,
                   method = "rf",
                   trControl = train.control,
                   preProcess = c("center","scale"),
                   tuneGrid = data.frame(mtry = 1:mtryRF))
    
    rf_out = data.frame(rfFit$results)
    rf_out <- rf_out %>%
      mutate_if(is.numeric, round, 3)
    
    rfTrainTable <- datatable(rf_out, options = list(scrollX = TRUE), 
                              caption = htmltools::tags$caption(style = 'caption-side: top;
                                                text-align: left;
                                                color:black;
                                                font-size:20px;
                                                font-weight: bold;', "Random Forest Model: Performance on Training Data"))
    
    RandomForestFit <- randomForest(as.formula(paste('pctGamesWon ~', predictorVars)), 
                                    data = modelTrain, ntree = 500, importance = TRUE)
    
    RF_pred <- predict(rfFit, newdata = modelTest)
    metric_rf = postResample(RF_pred, modelTest$pctGamesWon)
    metric_rf <- data.frame(metric_rf) %>%
      mutate_if(is.numeric, round, 3)
    
    rfTestTable <- datatable(metric_rf, caption = htmltools::tags$caption(style = 'caption-side: top;
                                                text-align: left;
                                                color:black;
                                                font-size:20px;
                                                font-weight: bold;', "Tree Model: Performance on Test Data"))
    
    list(rfTrainTable = rfTrainTable, RandomForestFit = RandomForestFit, rfTestTable = rfTestTable)
    
  })
  
  #output table for training data
  output$rfResultsTable <- renderDataTable({
    
    rfOutput()$rfTrainTable
    
  }) 
  
  #output table for training data
  output$rfPlot <- renderPlot({
    
    varImpPlot(rfOutput()$RandomForestFit, type = 1, main = "Variable Importance")
    
  })
  
  #output table for test data
  output$rfTest <- renderDataTable({
    
    rfOutput()$rfTestTable
    
  })  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#The following code is used by the data output section

  userDataSet <- reactive({
   
    simpleStats <- c("year", "league", "division", "team", "pctGamesWon", "leagueWin", "worldSeriesWin")
    offStats <- c("avgRunsScored", "avgHits", "avgTotalBases", "avgHomeRuns", "avgWalks")
    defStats <- c("avgRunsAllowed", "ERA", "avgHitsAllowed", "avgHomeRunsAllowed", "avgWalksAllowed", 
                  "avgErrors", "fieldingPct")
    if(input$userStat == "allStat") {
      userDataAll <- userDataRaw %>%
        select(simpleStats, offStats, defStats)
     
      if(input$userTeamSelect == 'TRUE' & input$userYrSelect == 'TRUE') {
        userDataAll2 <- userDataAll %>%
          filter(team == input$userTeam & (year <= input$userYrSlider[2]) & (year >= input$userYrSlider[1]))
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
              filter(team == input$userTeam & (year <= input$userYrSlider[2]) & (year >= input$userYrSlider[1]))
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

 output$ex3 <- renderUI({
   withMathJax(
     helpText('The busy Cauchy distribution
               $$\\frac{1}{\\pi\\gamma\\,\\left[1 +
               \\left(\\frac{x-x_0}{\\gamma}\\right)^2\\right]}\\!$$'))
 })
 
 
 
})

