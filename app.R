library(httr)
library(data.table)
library(DT)
library(tidyr)
library(dplyr)
library(shiny)
library(stringr)
library(openxlsx)
library(jsonlite)

source('www/src/get_db.R')
source('www/src/load_data.R')

source('ui.R')
source("server.R")

shinyApp(ui = ui, server = server)
