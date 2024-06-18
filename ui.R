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
                 width = 2
               ),
               
               mainPanel(
                 DTOutput("table")
               )
             )
    ),
    tabPanel('Select',
             sidebarLayout(
               sidebarPanel(
                 conditionalPanel(
                       condition = "input.process_request",
                       selectizeInput("project", "Select project:", choices = c(), multiple = FALSE),
                       selectizeInput("variable_orig", "Select variable:", choices = c(), multiple = FALSE),
                       selectizeInput("option", "Select option:", choices = c(), multiple = FALSE),
                     ),
                     width = 2
                   ),
                   
                   # Main panel for displaying outputs
                   mainPanel(
                     echarts4rOutput("perc_pie_chart"),
                     div(style = "height: 30px;"),
                     leafletOutput("map_oblast"),
                     div(style = "height: 30px;"),
                     leafletOutput("map_raion"),
                     div(style = "height: 30px;"),
                     leafletOutput("map_hromada"),
                     width = 10
                   )
                 )
    ),
    tabPanel('Numeric',
             sidebarLayout(
               sidebarPanel(
                 conditionalPanel(
                   condition = "input.process_request",
                   selectizeInput("project_numeric", "Select project:", choices = c(), multiple = FALSE),
                   selectizeInput("variable_orig_numeric", "Select variable:", choices = c(), multiple = FALSE),
                   selectizeInput("option_numeric", "Select option:", choices = c(), multiple = FALSE),
                   
                 ),
                 width = 2
               ),
               mainPanel(
                 leafletOutput("map_oblast_numeric"),
                 div(style = "height: 30px;"),
                 leafletOutput("map_raion_numeric"),
                 div(style = "height: 30px;"),
                 leafletOutput("map_hromada_numeric")
               )
             )
    )
  )
  
)

