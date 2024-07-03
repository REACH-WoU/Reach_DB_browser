library(httr)
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


# UI

ui <- fluidPage(
  
  # Application title
  titlePanel("REACH Database browser"),
  tags$head(
    tags$style(HTML(
      "#table-container {
          overflow: visible !important;
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
  
  tabsetPanel(
    # tabPanel('Read me',
    #          # Sidebar layout with input and output definitions
    #          column(12, 
    #                 h3("General information"),
    #                 span(htmlOutput("text_row1"), style = "font-size:20px;")
    #          ),
    #          column(6, 
    #                 h3("Intra-project comparison"),
    #                 span(htmlOutput("text_column1"), style = "font-size:20px;")
    #          ),
    #          column(6, 
    #                 h3("Within-project comparison"),
    #                 span(htmlOutput("text_column2"), style = "font-size:20px;")
    #          ),
    #          column(12, 
    #                 h3("Available research cylces"),
    #                 DTOutput("table1")
    #          ),
    #          div(style = "height: 20px;")
    # ),
    tabPanel('Database request',
             # Sidebar layout with input and output definitions
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("project_search", "Select project:", choices = c(), multiple = FALSE, selected = 'Overall'),
                 selectizeInput("questions",
                                "Select a question:",
                                choices = '',
                                multiple = TRUE),
                 
                 # Empty space with CSS styling
                 div(style = "height: 10px;"),
                 actionButton("process", "Build table"),
                 div(style = "height: 10px;"),
                 uiOutput('button2'),
                 div(style = "height: 10px;"),
                 downloadButton("excel", "Download Excel"),
                 width = 4
               ),
               
               mainPanel(
                 DTOutput("table"),
                 width = 8
               )
             )
    ),
    tabPanel('Categorical',
             sidebarLayout(
               sidebarPanel(
                 style = "position:fixed;width:inherit;",
                 selectizeInput("project", "Select project:", choices = c(), multiple = FALSE),
                 selectizeInput("variable_orig", "Select variable:", choices = c(), multiple = FALSE),
                 selectizeInput("option", "Select map z-col option:", choices = c(), multiple = FALSE),
                 # div(style = "height: 30px;"),
                 # selectizeInput("variable_orig_m", "Timeline selector, select multiple questions:", choices = c(), multiple = TRUE),
                 width = 2
               ),
               
               # Main panel for displaying outputs
               mainPanel(
                 fluidRow(
                   column(6, plotly::plotlyOutput("graph_1_select")),
                   column(6, plotly::plotlyOutput("graph_2_select")),
                 ),
                 # plotlyOutput("graph_3_select", height ='900px'),
                 div(style = "height: 50px;"),
                 leaflet::leafletOutput("map_oblast"),
                 div(style = "height: 30px;"),
                 leaflet::leafletOutput("map_raion"),
                 div(style = "height: 30px;"),
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
                 selectizeInput("option_numeric", "Select map z-col option:", choices = c(), multiple = FALSE),
                 # div(style = "height: 30px;"),
                 # selectizeInput("variable_orig_m_numeric", "Timeline selector, select multiple questions:", choices = c(), multiple = TRUE),
                 width = 2
               ),
               mainPanel(
                 # div(htmlOutput("numeric_text_1")),
                 plotly::plotlyOutput("graph_1_numeric"),
                 # fluidRow(column(6, div(htmlOutput("numeric_text_1"), 
                 #                       style = "text-align: center; color: #000080; font-size: 20px;")),
                 #          column(6, plotly::plotlyOutput("graph_1_numeric"))),
                 # plotlyOutput("graph_2_numeric", height ='900px'),
                 div(style = "height: 50px;"),
                 leaflet::leafletOutput("map_oblast_numeric"),
                 div(style = "height: 30px;"),
                 leaflet::leafletOutput("map_raion_numeric"),
                 div(style = "height: 30px;"),
                 leaflet::leafletOutput("map_hromada_numeric")
               )
             )
    ),
    tabPanel('Timeline categorical view',
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
    tabPanel('Timeline numeric view',
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
    ),
    tabPanel('Read me',
             includeMarkdown("README.md")
    ),
  )
  
)

