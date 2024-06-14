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
               # Sidebar panel for inputs
               sidebarPanel(
                 selectizeInput("questions",
                                "Select an option:",
                                choices = '',
                                multiple = TRUE),
                 
                 # Empty space with CSS styling
                 div(style = "height: 10px;"),
                 actionButton("process", "Build table"),
                 div(style = "height: 10px;"),
                 uiOutput('button2'),
                 div(style = "height: 10px;"),
                 downloadButton("excel", "Download Excel"),
                 width =2
               ),
               
               # Main panel for displaying outputs
               mainPanel(
                 # Output: DataTable
                 DTOutput("table"),
                 width = 10
               )
             )
    ),
    tabPanel('Geoview',
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("project_id", "Select project_id:", choices = projects_data$project_id, multiple = FALSE),
                 selectizeInput("round", "Select round:", choices = c(), multiple = FALSE),
                 selectizeInput("survey_type", "Select survey_type:", choices = c(), multiple = FALSE),
                 actionButton("check_representation_levels", "Check representation levels"),
                 selectizeInput("representation_level", "Select representation level:", choices = c(), multiple = FALSE),
                 actionButton("draw_map", "Draw representation level map"),
               ),
               mainPanel(
                 leafletOutput("map")
               )
             )
    )
  )
  
)

