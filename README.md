# ST558 Final Project
## Kara Belknap, 12/12/22

## Purpose

The purpose of this R Shiny app is to investigate how different offensive and defensive factors affect the percentage of games an MLB team will win in a regular MLB season. The app contains four main pages: 1. An about page in which the data is explained in detail, 2. An exploratory data analysis page in which a user can look at a number of different factors and how those factors affect the success of a team, 3. A modeling page that allows a user to investigate different regression models that relate to the data, and 4. A data page in which the user can subset the data and download the raw data to a .csv file for their own exploration.

## Packages Used

The following packages were used in the development of this package:
tidyverse, knitr, shiny, shinydashboard, DT, Lahman, and mathjaxr.

Prior to running this app, you can load all of these packages using the following lines of code in your R Console:

packages <- c("tidyverse", "knitr", "shiny", "shinydashboard", "DT", "Lahman", "mathjaxr")
lapply(packages, library, character.only = TRUE)

## To Run This App

Once the packages are loaded, you can run this app using the following code:

shiny::runGitHub()





