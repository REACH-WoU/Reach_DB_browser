library(httr)
library(purrr)
library(data.table)
library(DT)
library(tidyr)
library(dplyr)
library(shiny)
library(stringr)
library(openxlsx)
library(jsonlite)
library(mapview)
library(sf)
library(leaflet)
library(markdown)
library(plotly)
library(sf)
library(rmapshaper)
library(rhandsontable)

sf_use_s2(FALSE)

source('www/src/utils.R')
source('www/src/get_db.R')
source('www/src/load_data.R')


# UI

ui <- fluidPage(
  
  # Application title
  titlePanel("REACH Database browser"),
  tags$head(
    tags$style(HTML(
      "#table-container {
          overflow: visible !important;
        }
      #table-container {
            overflow: visible !important;
            }
      .leaflet-container {
        cursor: crosshair;
      }"
    )),
    HTML(
      '<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"/>'
    ),
    includeCSS("www/style.css"),
    HTML(
      '<a style="padding-left:10px;" class="app-title" href= "https://www.reach-initiative.org/" target="_blank"><img src="reach.jpg" height = "50"></a><span class="app-description" style="font-size: 16px; color: #FFFFFF"><strong>Database_test</strong></span>'
    ),
  ),
  
  tabsetPanel(id = "tabs",
              tabPanel('Read me',
                       includeMarkdown("README.md")
              ),
              tabPanel('Database request',
                       sidebarLayout(
                         sidebarPanel(
                           selectizeInput("project_search", "Select project:", choices = c(), multiple = FALSE, selected = 'Overall'),
                           selectizeInput("questions",
                                          "Select a question:",
                                          choices = '',
                                          multiple = TRUE),
                           
                           # Empty space with CSS styling
                           div(style = "height: 10px;"),
                           span('Once the options have been selected, click the button below to build the table of the available data',
                                style = "font-weight: bold;"),
                           div(style = "height: 10px;"),
                           actionButton("process", "Build table"),
                           div(style = "height: 10px;"),
                           uiOutput('button2'),
                           div(style = "height: 10px;"),
                           span('If you want to save the current selection of variables for the future, you can download an excel file with them',
                                style = "font-weight: bold;"),
                           div(style = "height: 10px;"),
                           downloadButton("excel", "Download Excel"),
                           div(style = 'height: 10px'),
                           tags$hr(style = "border: 1px solid black;"),
                           div(checkboxInput("checker", "I already have an excel file I'd like to use"),
                               style = "font-size: 18px !important"),
                           div(style = 'height: 10px'),
                           conditionalPanel(
                             condition = "input.checker == true",
                             fileInput("file", "Upload the excel file here", accept = ".xlsx")),
                           width = 4
                         ),
                         
                         mainPanel(
                           DTOutput("table"),
                           width = 8
                         )
                       )
              ),
              tabPanel('Geospatial request',
                       sidebarLayout(
                         sidebarPanel(
                           h5("Draw a polygon on the map to select the area of interest"),
                           selectizeInput("geo_admin_level", "Select admin level", choices = c("oblast", "raion", "hromada", "settlement"), multiple = FALSE, selected = "oblast"),
                           conditionalPanel(
                             condition = "input.geo_admin_level == 'hromada' | input.geo_admin_level == 'settlement'",
                             selectizeInput("geo_defined_oblast", "Specify oblasts", choices = c("Overall", oblasts$admin1Name_eng), multiple = TRUE, selected = c("Kyivska", "Kyiv city"))
                           ),
                           h5("Choose the topology relation:\n
                      - 'intersects' - the geometries have at least one point in common
                      - 'covers' -admin geometry is completely inside the drawn polygon"),
                           selectizeInput("geo_relation_type", "Select topology relation", choices = c("intersects", "covers"), multiple = FALSE, selected = "intersects"),
                           fluidRow(actionButton("geo_reset", "Reset Polygon"),
                                    actionButton("geo_get_info", "Get info"),
                                    conditionalPanel(
                                      condition = "output.geo_showGetButton",
                                      actionButton("geo_get_questions", "Get questions"),
                                    )
                           ),
                           tags$hr(style = "border: 1px solid black;"),
                           div(checkboxInput("checker_geo", "I already have an excel file I'd like to use"),
                               style = "font-size: 18px !important"),
                           div(style = 'height: 10px'),
                           conditionalPanel(
                             condition = "input.checker_geo == true",
                             fileInput("file_geo", "Upload the excel file here", accept = ".xlsx")),
                           width = 3
                         ),
                         mainPanel(
                           leafletOutput("geo_map", height = "520px"),
                           div(style = "height: 15px;"),
                           rHandsontableOutput("geo_table"),
                           width = 9
                         )
                       )
              ),
              tabPanel("Questions observer",
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput('geo_get_request'),
                           width = 2
                         ),
                         mainPanel(
                           DTOutput("geo_questions_table"),
                           width = 10
                         )
                       )
              ),
              tabPanel('Data columns explorer',
                       sidebarLayout(
                         sidebarPanel(
                           fluidRow(
                             column(3, selectizeInput("column_project_search_1", "Select project:", choices = c(""), multiple = FALSE, selected = "")),
                             column(3, selectizeInput("column_questions_1", "Select a question:", choices = c(""), multiple = FALSE, selected = "")),
                             column(3, selectizeInput("column_dissagr_1", "Select dissagr:", choices = c("Overall"), multiple = FALSE, selected = "Overall")),
                             column(3, selectizeInput("column_admins_1", "Select admins:", choices = c("Overall"), multiple = TRUE, selected = "Overall"))
                           ),
                           uiOutput("column_dynamic_fields"),
                           
                           actionButton("column_process", "Process"),
                           downloadButton("column_excel", "Download Excel"),
                           width = 11
                         ),
                         
                         mainPanel(
                         )
                       )
              ),
              tabPanel('Categorical',
                       sidebarLayout(
                         sidebarPanel(
                           style = "position:fixed;width:inherit;",
                           selectizeInput("project", "Select project:", choices = c(), multiple = FALSE),
                           selectizeInput("variable_orig", "Select variable:", choices = c(), multiple = FALSE),
                           selectizeInput("dissagr", "Select dissagr:", choices = c(), multiple = FALSE),
                           selectizeInput("option", "Select map z-col option:", choices = c(), multiple = FALSE),
                           width = 2
                         ),
                         
                         mainPanel(
                           # fluidRow(
                           #   column(6, plotly::plotlyOutput("graph_1_select")),
                           #   column(6, plotly::plotlyOutput("graph_2_select")),
                           # ),
                           plotly::plotlyOutput("graph_1_select", height = "800px"),
                           plotly::plotlyOutput("graph_2_select", height = "800px"),
                           # div(style = "height: 50px;"),
                           h4("Oblast map"),
                           leaflet::leafletOutput("map_oblast"),
                           div(style = "height: 30px;"),
                           h4("Raion map"),
                           leaflet::leafletOutput("map_raion"),
                           div(style = "height: 30px;"),
                           h4("Hromada map"),
                           leaflet::leafletOutput("map_hromada"),
                           width = 10
                         )
                       )
              ),
              tabPanel('Numeric',
                       sidebarLayout(
                         sidebarPanel(
                           style = "position:fixed;width:inherit;",
                           selectizeInput("project_numeric", "Select project:", choices = c(), multiple = FALSE),
                           selectizeInput("variable_orig_numeric", "Select variable:", choices = c(), multiple = FALSE),
                           selectizeInput("dissagr_numeric", "Select dissagr:", choices = c(), multiple = FALSE),
                           selectizeInput("option_numeric", "Select map z-col option:", choices = c(), multiple = FALSE),
                           width = 2
                         ),
                         mainPanel(
                           plotly::plotlyOutput("graph_1_numeric"),
                           div(style = "height: 50px;"),
                           h4("Oblast map"),
                           leaflet::leafletOutput("map_oblast_numeric"),
                           div(style = "height: 30px;"),
                           h4("Raion map"),
                           leaflet::leafletOutput("map_raion_numeric"),
                           div(style = "height: 30px;"),
                           h4("Hromada map"),
                           leaflet::leafletOutput("map_hromada_numeric"),
                           width = 10
                         )
                       )
              ),
              tabPanel('Timeline categorical',
                       sidebarLayout(
                         sidebarPanel(
                           style = "position:fixed;width:inherit;",
                           selectizeInput("variable_orig_m", "Timeline selector, select multiple questions:", choices = c(), multiple = TRUE),
                           width = 2
                         ),
                         mainPanel(
                           plotly::plotlyOutput("graph_3_select", height ='900px'),
                           width = 10
                         )
                       )
              ),
              tabPanel('Timeline numeric',
                       sidebarLayout(
                         sidebarPanel(
                           style = "position:fixed;width:inherit;",
                           selectizeInput("variable_orig_m_numeric", "Timeline selector, select multiple questions:", choices = c(), multiple = TRUE),
                           width = 2
                         ),
                         mainPanel(
                           plotly::plotlyOutput("graph_2_numeric", height ='900px'),
                           width = 10
                         )
                       )
              )
  )
  
)

