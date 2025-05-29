# Helper function: Handle NULL inputs by returning NA; otherwise, unlist the input
unlist_with_na <- function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(unlist(x))
  }
}

# JavaScript code snippet for handling checkbox interactions in the UI, related to 'Database request' part
js <- c(
  "$('[id^=checkb]').on('click', function(){",
  "  var id = this.getAttribute('id');",
  "  var i = parseInt(/checkb(\\d+)/.exec(id)[1]);",
  "  var value = $(this).prop('checked');",
  "  var info = [{row: i, col: 16, value: value}];",
  "  Shiny.setInputValue('dtable_cell_edit:DT.cellInfo', info);",
  "})"
)

# JavaScript code snippet for handling checkbox interactions in the UI, related to 'Geospatial request' part
geo_js <- c(
  "$('[id^=chec]').on('click', function(){",
  "  var id = this.getAttribute('id');",
  "  var i = parseInt(/chec(\\d+)/.exec(id)[1]);",
  "  var value = $(this).prop('checked');",
  "  var info = [{row: i, col: 9, value: value}];",
  "  Shiny.setInputValue('geo_table_cell_edit:DT.cellInfo', info);",
  "})"
)

# Function to generate REACH color palettes for visualizations
palette_function <- colorRampPalette(c("#F4FBFE", "#DFECEF", "#BFDBE2", "#9FCACE", "#77B2BF", "#4096AA", "#27768A", "#0C596B", "#0C3842", "#0F2328"))

# Create Shiny inputs dynamically for each row in a data frame
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
  }
  inputs
}

server <- function(input, output, session) {
  
  
  # ----------------------------readme -------------------------------------------------
  
  # output$text_row1 <- renderText({})
  # 
  # 
  # output$text_column1 <- renderText({})
  # 
  # output$text_column2 <- renderText({})
  
  # ---------------------------the Database requests page --------------------------------
  
  # Update the project selection based on available projects in the database
  updateSelectInput(session, "project_search", choices = c("Overall", unique(unique_table$project_ID)))
  
  # Handle changes in the selected project and update questions dropdown accordingly, related to Database request part
  observeEvent(input$project_search,{
    if(input$project_search == "Overall"){
      updateSelectizeInput(
        session,
        inputId = "questions",
        label   = "Select an option:",
        choices = unique_questions,
        server  = TRUE )
    } else {
      projects_question <- unique_table %>% 
        filter(project_ID == input$project_search) %>% 
        pull(database_label_clean)
      
      updateSelectizeInput(
        session,
        inputId = "questions",
        label   = "Select an option:",
        choices = projects_question,
        server  = TRUE )
    }
  })
  
  
  
  # Reactive values to store various intermediate datasets
  datasetInput <- reactiveVal(NULL)
  Dat <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  numeric_data <- reactiveVal(NULL)
  select_data <- reactiveVal(NULL)
  
  # Reactive expression to fetch the selected project
  observeEvent(ignoreInit = TRUE, input$process,{
    req(input$questions)
    
    questions<- input$questions
    
    # Fetch relevant IDs and filter data
    get_true_IDs <- unique_table %>% 
      dplyr::filter(database_label_clean %in% questions) %>% 
      dplyr::pull(true_ID)
    
    
    needed_data <- database_project %>% 
      dplyr::filter(true_ID %in% get_true_IDs) %>% 
      dplyr::mutate(TABLE_ID = paste0(project_ID,'_R',round_ID,'_',survey_type)) %>% 
      inner_join(time_tbl) %>% 
      dplyr::rename(month_conducted = value)
    
    # Process data and prepare for rendering in the table
    df_choices_added <- needed_data %>%
      left_join(tool_survey %>%
                  dplyr::select(name, contains('label'), TABLE_ID) %>%
                  dplyr::rename(
                    english_question = `label::English` ,
                    ukrainian_question = `label::Ukrainian`,
                    russian_question = `label::Russian`
                  ) %>%
                  dplyr::mutate(name = str_squish(name))) %>% 
      left_join(rep_table) %>% 
      dplyr::select(project_ID,survey_type,round_ID,sector,TABLE_ID,q.type,list_name,datasheet,
             english_question,ukrainian_question,russian_question,representative_at,oblast,
             month_conducted,name)
    
    removeModal()
    
    # Set processed data to reactive value for rendering
    datasetInput(
      cbind(df_choices_added, check = shinyInput(checkboxInput, nrow(df_choices_added), "checkb")
    ))
    
    Dat(cbind(df_choices_added, bool = FALSE)) 
    
    # Render the data table
    output$table <- renderDT({
      df <- datasetInput()
      df <- df %>% 
        dplyr::mutate(representative_at = gsub('\\,',', ',representative_at))
      
      # remove every second space to have a wider bar in the table
      df$oblast <- vapply(df$oblast, function(x){
        ls <- unlist(str_split(x,','))
        for(i in 1:length(ls)){
          if(i%%2==0){
            ls[i] <- gsub(' ','',ls[i])
          }
        }
        output <- paste(ls,collapse = ',')
        return(output)
      },character(1)
      )
      
      
      if(is.null(df)){
        return(NULL)
      }else{
        datatable(
          df,
          filter = "top",
          class = list(stripe = FALSE),
          escape = FALSE,
          editable = list(target = "cell", disable = list(columns = 16)),
          selection = "none",
          callback = JS(js),
          options = list(
            dom = 'lfrtipB',
            # scrollX=T,
            # autoWidth = TRUE,
            pageLength = 100,
            columnDefs = list(
              # list(targets = c(which(names(df)=='oblast')-1), width = '200px'),
              list(visible=FALSE, targets=(which(names(df)%in% c('survey_type','round_ID',
                                                                 'TABLE_ID','list_name','datasheet','name'))-1)
              )
            )),
          rownames = FALSE,
        )%>% 
          formatStyle(0:ncol(df), `border-bottom` = "solid 2px #000")
        
      }
      
    }, server = FALSE)
    
  })
  
  observeEvent(ignoreInit = TRUE, input$dtable_cell_edit, { 
    info <- input$dtable_cell_edit # this input contains the info of the edit
    print(info)
    dato <- Dat() # read the frame
    dato$bool <- as.character(dato$bool)# convert to character (no warnings this way)
    Dat(editData(dato, info))# update the reactive
    
    if(any(Dat()$bool)){
      output$button2 <- renderUI({
        tagList(
        span("When you've selected all of the questions you want visualized, click the button below",
             style = "font-weight: bold;"),
        div(style = "height: 10px;"),
        actionButton("process_request", "Send the request to the server")
        )
      })
    }
    
  })
  
  observeEvent(ignoreInit = TRUE, input$process_request,{
    
    output$button2 <- renderUI({NULL})
    
    selected_frame <- Dat()
    print(colnames(selected_frame))
    selected_frame <- selected_frame %>% filter(bool == 'TRUE')
    
    if (nrow(selected_frame) == 0) {
      showModal(modalDialog(
        title = "Error",
        "Please select at least one row",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    rep_table_overview <-  dbGetQuery(my_connection , "SELECT TABLE_ID,datasheet_names,main_datasheet,representative_columns from data_representative_table")
    rep_table_overview_geo <- rep_table_overview %>% 
      mutate(representative_columns=ifelse(representative_columns == 'None','Overall',representative_columns)) %>% 
      select(TABLE_ID,representative_columns)
    
    rep_table_overview_sheets_dict <- rep_table_overview %>% 
      separate_rows(datasheet_names ,sep =';') %>% 
      mutate(datasheet_name_tool = ifelse(datasheet_names == main_datasheet,'main',datasheet_names)) %>% 
      select(TABLE_ID,datasheet_names,datasheet_name_tool)
    
    
    general_info <- selected_frame %>% 
      select(TABLE_ID,project_ID,round_ID,survey_type,month_conducted) %>% 
      distinct() %>% 
      left_join(rep_table_overview_sheets_dict %>% 
                  filter(datasheet_name_tool=='main') %>% 
                  mutate(main_sheet_name = paste0('data_',TABLE_ID,'_',datasheet_names,'_DCMPR'))) %>% 
      select(-c(datasheet_names ,datasheet_name_tool))
    
    # get the weights info
    weight_table <-  dbGetQuery(my_connection , paste0(
      "SELECT TABLE_NAME, COLUMN_NAME
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_NAME in ('",paste0(unique(general_info$main_sheet_name), collapse ="','")  ,"')"))
    
    weight_table <- weight_table %>% 
      filter(grepl('weight',COLUMN_NAME)) %>% 
      rename(main_sheet_name =TABLE_NAME,
             weight_column_name = COLUMN_NAME)
    
    if(nrow(weight_table)>0){
      general_info <- general_info %>% 
        left_join(weight_table)
    }else{
      general_info$weight_column_name <- NA
    }
    
    DAF_disaggs <- database_project %>% 
      mutate(TABLE_ID = paste0(project_ID,'_R',round_ID,'_',survey_type)) %>% 
      filter(TABLE_ID %in% unique(selected_frame$TABLE_ID),
             datasheet=='main',
             true_ID %in% c('226218a1-ae1a-4fb9-8419-888707dd20da','5e44e6ff-f9f9-4a8a-9d32-636bd51554db',
                            '94dfa0b4-902e-4a9c-9c30-4b4ff25637c0')) %>% 
      select(name,TABLE_ID)
    
    DAF_disaggs_full <- rbind(DAF_disaggs, DAF_disaggs %>% mutate(name=NA))
    
    DAF_template <- selected_frame %>% 
      left_join(
        rep_table_overview_geo %>% 
          mutate(representative_columns = ifelse(representative_columns!='Overall',
                                                 paste0(representative_columns,';','Overall'),
                                                 'Overall'))
      ) %>% 
      separate_rows(representative_columns,sep =';') %>% 
      left_join(DAF_disaggs_full %>% rename(disaggregations=name),relationship = "many-to-many") %>% 
      rename(admin = representative_columns,
             variable_label = english_question) %>% 
      mutate(disaggregations_label = ifelse(is.na(disaggregations),'Overall',disaggregations)) %>% 
      group_by(TABLE_ID) %>% 
      mutate(n_ = 1,
             ID = cumsum(n_)) %>% 
      select(-n_) %>% 
      ungroup() %>% 
      mutate(calculation = NA,
             func = ifelse(q.type %in% c('decimal','integer'),'numeric',q.type),
             join = NA) %>% 
      select(TABLE_ID,ID, name,variable_label,calculation,func,admin,
             disaggregations,disaggregations_label,join, q.type, datasheet) %>% 
      left_join(rep_table_overview_sheets_dict %>% rename(datasheet=datasheet_name_tool)) %>% 
      rename(variable = name) %>% 
      mutate(DB_table_name = paste0('data_',TABLE_ID,'_',datasheet_names,'_DCMPR')) %>% 
      select(-datasheet_names) 
    
    removeModal()
    showModal(
      modalDialog(
        title = "Processing",
        "Sending your request to the processing system.",
        footer = NULL,
        easyClose = TRUE
      )
    )
    
    DAF_template[is.na(DAF_template$calculation),]$calculation <- 'empty'
    DAF_template[is.na(DAF_template$disaggregations),]$disaggregations <- 'empty'
    
    if(nrow(general_info[is.na(general_info$weight_column_name),])>0){
      general_info[is.na(general_info$weight_column_name),]$weight_column_name <- 'empty'
    }
    
    write.xlsx(DAF_template, "daf.xlsx")
    write.xlsx(general_info, "gen_info.xlsx")

    json_body <- list(
      daf_file = DAF_template,
      info = general_info,
      filter = data.frame(
        TABLE_ID = as.character(),
        ID = as.character(),
        variable = as.character(),
        operation = as.character(),
        value = as.character()
      )
    )
    
    url <- Sys.getenv('url')
    
    response <- POST(url, body = json_body, encode = "json")
    print(status_code(response))
    
    char <- rawToChar(response$content)
    df <- fromJSON(char)
    
    df_final <- as.data.frame(do.call(cbind,df$result))
    
    # Convert the list to a dataframe 
    df <- purrr::map_dfc(df_final, ~ purrr::map(.x, unlist_with_na) %>% unlist())
    
    df <- df %>% 
      left_join(general_info %>% select(TABLE_ID,month_conducted))
    
    
    removeModal()
    processed_data(df)
    
    output$map_oblast <- renderLeaflet({NULL})
    output$map_raion <- renderLeaflet({NULL})
    output$map_hromada <- renderLeaflet({NULL})
    
    output$map_oblast_numeric <- renderLeaflet({NULL})
    output$map_raion_numeric <- renderLeaflet({NULL})
    output$map_hromada_numeric <- renderLeaflet({NULL})
  })
  
  # if the user has excel input let them use it here
  observeEvent(ignoreInit = TRUE, list(input$file,
                                       input$file_geo), {

    if (!is.null(input$file)){
      excel_input <- openxlsx::read.xlsx(input$file$datapath)
    }else if (!is.null(input$file_geo)){
      excel_input <- openxlsx::read.xlsx(input$file_geo$datapath)
    }
    # check that excel file is valid
    columns.set <- c("ID", "admin", "admin_category", "option", "variable",
                     "disaggregations_category_1",
                     "weighted_count", "unweighted_count", "perc", "general_count",
                     "full_count", "total_count_perc", "option_orig",
                     "admin_category_orig",
                     "variable_orig", "TABLE_ID", "mean",
                     "median", "min", "max", "month_conducted")
    
    if(!all(columns.set %in% colnames(excel_input))){
      showModal(modalDialog(
        title = "Error",
        "The excel file does not contain the correct columns. Please check the columns and try again.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    
    if (nrow(excel_input) == 0) {
      showModal(modalDialog(
        title = "Error",
        "The excel file is empty. Please check the file and try again.",
        easyClose = TRUE
      ))
      return(NULL)
    }

    processed_data(excel_input)
  })
  
  # split the processed data into numeric and character
  observeEvent(processed_data(),{
    # write.xlsx(processed_data(),'excel_input.xlsx')
    if(!is.null(processed_data())){
      
      df <- processed_data()
      if (!("disaggregations_1" %in% colnames(df))) {
        df$disaggregations_1 = " Overall"
      }
      if ("mean" %in% colnames(df)) {
        print("mean")
        numeric <- df %>%
          filter(!is.na(mean))
      } else {
        numeric <- data.frame(
          TABLE_ID = character(),
          ID = numeric(),
          variable = character(),
          admin = character(),
          disaggregations = character(),
          variable_orig = character(),
          disaggregations_category_1 = character(),
          option = character(),
          admin_category_orig = character()
        )
      }
      
      if ("perc" %in% colnames(df)) {
        print("perc")
        select <- df %>%
          filter(!is.na(perc))
        
        # testo<<-select
        
        full_frame <- select %>%
          select(variable,option,option_orig,TABLE_ID) %>%
          distinct() %>%
          full_join(select %>% 
                      select(variable,variable_orig,
                             month_conducted,TABLE_ID,
                             admin,admin_category,admin_category_orig,
                             full_count, total_count_perc) %>% 
                      distinct()) 
        
        select <- select %>%
          right_join(full_frame) %>% 
          mutate(across(any_of(c('disaggregations_1','disaggregations_1_orig',
                                 'disaggregations_category_1','disaggregations_category_1_orig')), ~ ifelse(is.na(.x),' Overall',.x)),
                 across(c(weighted_count, unweighted_count,perc, general_count), ~ ifelse(is.na(.x),0,.x))) 

        
      } else {
        select <- data.frame(
          TABLE_ID = character(),
          ID = numeric(),
          variable = character(),
          admin = character(),
          disaggregations = character(),
          variable_orig = character(),
          disaggregations_1 = character(),
          option = character(),
          admin_category_orig = character()
        )
      }
      
      numeric_data(numeric)
      select_data(select)
      
      projects_numeric <- unique(numeric$TABLE_ID)
      projects_select <- unique(select$TABLE_ID)
      
      updateSelectizeInput(session, "project", choices = c(""), selected = "")
      updateSelectizeInput(session, "project_numeric", choices = c(""), selected = "")
      
      updateSelectizeInput(session, "project", choices = projects_select)
      updateSelectizeInput(session, "project_numeric", choices = projects_numeric)
      
      output$map_oblast <- renderLeaflet({NULL})
      output$map_raion <- renderLeaflet({NULL})
      output$map_hromada <- renderLeaflet({NULL})
      
      output$map_oblast_numeric <- renderLeaflet({NULL})
      output$map_raion_numeric <- renderLeaflet({NULL})
      output$map_hromada_numeric <- renderLeaflet({NULL})
      
      output$graph_1_select <- renderPlotly({NULL})
      output$graph_2_select <- renderPlotly({NULL})
      output$graph_3_select <- renderPlotly({NULL})
      
      output$graph_1_numeric <- renderPlotly({NULL})
      output$graph_2_numeric <- renderPlotly({NULL})
      
      showNotification("The data has been processed. You can either download it as an excel or switch to the Categorical, Numerical and Timeline pages to view visuals",
                       type = 'message')
      
    }
  })
  
  # Download handler for exporting data to Excel, related to Database request
  output$excel <- downloadHandler(
    filename = function() {
      paste("data", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      
      excel_frame <- processed_data()
      # Create a workbook
      wb <- createWorkbook()
      
      # Add a worksheet
      addWorksheet(wb, "Data")
      
      # Write data to the worksheet
      writeData(wb, "Data", x = excel_frame, startCol = 1, startRow = 1, rowNames = FALSE)
      
      # Save the workbook
      saveWorkbook(wb, file)
    }
  )
  
  # Download handler for exporting data to Excel, related to Geospatial request
  output$excel_geo <- downloadHandler(
    filename = function() {
      paste("data", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      
      excel_frame <- processed_data()
      # Create a workbook
      wb <- createWorkbook()
      
      # Add a worksheet
      addWorksheet(wb, "Data")
      
      # Write data to the worksheet
      writeData(wb, "Data", x = excel_frame, startCol = 1, startRow = 1, rowNames = FALSE)
      
      # Save the workbook
      saveWorkbook(wb, file)
    }
  )
  
  
  
  
  # Event to handle when a project is selected, related to Categorical page
  observeEvent(ignoreInit = FALSE, input$project, {
    # Ensure the input project and selected data are not null and have rows
    if (!is.null(input$project) && !is.null(select_data()) && nrow(select_data())>0) {
      # Filter data based on the selected project
      filtered_df <- select_data() %>% filter(TABLE_ID == input$project)
      # Extract unique variables for the selected project
      variables <- unique(filtered_df$variable_orig)
      
      updateSelectizeInput(session, "variable_orig", choices = variables)
      
      # Update the multi-variable selection input
      variables_m <- unique(select_data()$variable_orig)
      updateSelectizeInput(session, "variable_orig_m", choices = variables_m, selected = variables_m[1])
      
    }
    
  })
  
  # Event to handle a variable is selected, related to Categorical page
  observeEvent(ignoreInit = FALSE,list(
    input$project, input$variable_orig
  ), {
    # Ensure variable selection and data are valid
    if (!is.null(input$variable_orig) && !is.null(select_data()) && nrow(select_data())>0) {
      data_processed <- select_data()
      
      # Filter data for the selected project and variable
      filtered_df <- data_processed %>%
        dplyr::filter(TABLE_ID %in% input$project & variable_orig %in% input$variable_orig,
               perc>0)
      # Update disaggregation input with unique values
      updateSelectizeInput(session, "dissagr", choices = unique(filtered_df$disaggregations_1), selected = NULL)
    }
  })
  
  # Event to handle a disaggregation is selected, related to Categorical page
  observeEvent(ignoreInit = FALSE,list(
    input$project, input$variable_orig, input$dissagr
  ), {
    if (!is.null(input$variable_orig) && !is.null(input$dissagr) && !is.null(select_data()) && nrow(select_data())>0) {
      # Filter data based on project, variable, and disaggregation
      data_processed <- select_data()
      filtered_df <- data_processed %>%
        dplyr::filter(TABLE_ID %in% input$project & variable_orig %in% input$variable_orig & disaggregations_1 %in% c(input$dissagr, " Overall"),
               perc>0)
      # Process data for overall admin
      overall_admin_data <- filtered_df %>%
        dplyr::filter(admin == "Overall" & disaggregations_category_1 %in% c('Overall',' Overall')) %>% 
        rowwise() %>% 
        dplyr::mutate(option = paste(strwrap(option, width = 35), collapse = "<br>")) %>% 
        ungroup() %>%
        distinct()
      
      # Update option input with unique options
      options <- unique(filtered_df$option)
      updateSelectizeInput(session, "option", choices = options)
      
      # Generate graph 1 based on overall admin data
      total_cnt <- round(sum(overall_admin_data$perc)*100,0)
      title_graph_1 <- paste0(unique(overall_admin_data$variable),'<br>')
      
      output$graph_1_select <- renderPlotly({
        if(total_cnt==100){
          # Pie chart if percentages add up to 100
          graph_1 <- plot_ly(overall_admin_data, labels = ~option, values = ~weighted_count, type = 'pie', hole = 0.6) %>%
            layout(title = list(text = paste0("<b>",title_graph_1 ,"</b>"),
                                font = list(color = '#000080', size = 12)),
                   showlegend = TRUE)
          
        }else{
          # Bar chart otherwise
          graph_1 <- plot_ly(overall_admin_data, x = ~perc*100, y = ~option,  type = 'bar',
                             text = ~paste0(round(perc*100,1),'%'),
                             textposition = 'outside') %>%
            layout(title = list(text=paste0("<b>",title_graph_1 ,"</b>"),
                                font = list(color = '#000080', size = 12)),
                   xaxis = list(ticksuffix = "%",title = "", range = c(0,110)),
                   yaxis = list(title = ""))
        }
        return(graph_1)
      })
      
      
      # Prepare data for disaggregation graph
      overall_disaggregation_data <- filtered_df %>%
        filter(admin == "Overall") %>% 
        rowwise() %>% 
        mutate(option = paste(strwrap(option, width = 35), collapse = "<br>")) %>% 
        ungroup()  %>%
        distinct()
      
      # Check categories and filter non-overall data if needed
      check_categories <- setdiff(unique(overall_disaggregation_data$disaggregations_category_1), c(' Overall','Overall'))
      if(length(check_categories)>0){
        overall_disaggregation_data <- overall_disaggregation_data %>% 
          filter(!disaggregations_category_1 %in% c(' Overall','Overall'))
      }
      
      # Generate disaggregation graph
      title_graph_2 <- paste0(unique(overall_disaggregation_data$variable),'<br>')
      title_graph_2 <- substr(title_graph_2, 1, 50)
      title_graph_2 <- paste0(title_graph_2, " by ", paste(unique(overall_disaggregation_data$disaggregations_1), collapse = ", "), '<br>')
      output$graph_2_select <- renderPlotly({
        plot_ly(overall_disaggregation_data, x = ~perc*100, y = ~disaggregations_category_1, color = ~option, type = 'bar',
                text = ~paste0(round(perc*100,1),'%'),
                textposition = 'auto',
                insidetextanchor = 'start',
                hoverinfo = 'text',
                marker = list(line = list(width = 1, color = "black"))) %>%
          layout(title = list(text = paste0("<b>", title_graph_2 , "</b>"),
                              font = list(color = '#000080', size = 12)),
                 xaxis = list(ticksuffix = "%", title = "", range = c(0,110),
                              automargin = TRUE),
                 yaxis = list(title = "", automargin = TRUE, standoff = 15),  # add standoff parameter
                 margin = list(l = 100, r = 20, t = 50, b = 50),
                 showlegend = TRUE)
      })
      
      
    }
    
    
  })
  
  # Event handler for the selection of variables in the multi-variable timeline graph, related to Timeline Categorical page
  observeEvent(ignoreInit = TRUE,input$variable_orig_m,{
    if(!is.null(input$variable_orig_m) && !is.null(select_data()) && nrow(select_data())>0){
      
      data_processed <- select_data()
      
      # Filter and preprocess data for the timeline graph
      graph_base3 <- data_processed %>% 
        filter(variable_orig %in% input$variable_orig_m,
               disaggregations_category_1 %in% c(' Overall','Overall'),
               admin %in% 'Overall') %>% 
        rowwise() %>% 
        mutate(variable = paste(strwrap(variable, width = 35), collapse = "<br>"),
               period_full = paste(month_conducted, TABLE_ID, variable,sep = '<br>'),
               option = paste(strwrap(option, width = 35), collapse = "<br>"))
      
      # Summarize percentages to check if they total 100%
      cnts <- graph_base3 %>% group_by(variable,TABLE_ID) %>% summarise(perc=sum(perc)) %>% pull(perc)
      cnts <- round(cnts*100,0)
      title <- graph_base3$variable
      
      # Prepare graph title
      title <- substr(title, 1, 50)
      title <- paste0(title, '...') # Truncate if too long
      title <- paste0(title,'<br>')
      
      # Render the timeline graph using Plotly
      output$graph_3_select <- renderPlotly({
        if(all(cnts==100)){
          
          plot_ly(graph_base3, x = ~period_full, y = ~perc*100 , color = ~option, type = 'bar',
                  text = ~paste0(round(perc*100,1),'%'),
                  # textposition = 'auto',
                  textfont =  list(size = 14,color = 'black')) %>%
            layout(barmode = 'stack', 
                   title = list(text=paste0("<b>",title ,"</b>"),
                                font = list(color = '#000080', size = 14)),
                   legend = list(x = 0, y = -0.2),
                   xaxis = list(title = ""),
                   yaxis = list(ticksuffix = "%",title = ""))
        }else{
          plot_ly(graph_base3, x = ~period_full, y = ~perc*100 , color = ~option, type = 'bar',
                  text = ~paste0(round(perc*100,1),'%'),
                  # textposition = 'auto',
                  textfont =  list(size = 14,color = 'black')) %>%
            layout(title = list(text=paste0("<b>",title ,"</b>"),
                                font = list(color = '#000080', size = 14)),
                   xaxis = list(title = ""),
                   yaxis = list(ticksuffix = "%",title = ""))
          
        }
      }) 
      
    }
  })
  
  # Event handler for option selection to display geographical maps, related to Categorical page
  observeEvent(ignoreInit = TRUE,input$option, {
    if (!is.null(input$option) && !is.null(select_data()) && nrow(select_data())>0) {
      
      # Filter the data based on the selected option and variable
      filtered_df <- select_data() %>%
        filter(TABLE_ID == input$project & variable_orig == input$variable_orig & disaggregations_category_1 %in% c('Overall',' Overall') & option == input$option)
      
      # Prepare and merge data for oblast-level map visualization
      oblast_map <- filtered_df %>%
        inner_join(oblast_json, by = c("admin_category_orig" = "ADM1_PCODE"))
      
      oblast_map <- st_as_sf(oblast_map)
      
      # Prepare and merge data for raion-level map visualization
      raion_map <- filtered_df %>%
        inner_join(raion_json, by = c("admin_category_orig" = "ADM2_PCODE"))
      
      raion_map <- st_as_sf(raion_map)
      
      # Prepare and merge data for hromada-level map visualization
      hromada_map <- filtered_df %>%
        inner_join(hromada_json, by = c("admin_category_orig" = "ADM3_PCODE"))
      
      hromada_map <- st_as_sf(hromada_map)
      
      # Render oblast-level map using Leaflet
      if (nrow(oblast_map) > 0) {
        output$map_oblast <- renderLeaflet({
          req(oblast_map)
          mapview(oblast_map, zcol = "perc", layer.name = paste("Oblast map -", input$option, "option"),
                  col.regions = palette_function(10),
                  map.types = c("CartoDB.Positron",
                                "OpenStreetMap",
                                "Esri.WorldImagery",
                                "OpenTopoMap"))@map
        })
      }
      # Render raion-level map using Leaflet
      if (nrow(raion_map) > 0) {
        output$map_raion <- renderLeaflet({
          req(raion_map)
          mapview(raion_map, zcol = "perc", layer.name = paste("Raion map -", input$option, "option"),
                  col.regions = palette_function(10),
                  map.types = c("CartoDB.Positron",
                                "OpenStreetMap",
                                "Esri.WorldImagery",
                                "OpenTopoMap"))@map
        })
      }
      # Render hromada-level map using Leaflet
      if (nrow(hromada_map) > 0) {
        output$map_hromada <- renderLeaflet({
          req(hromada_map)
          mapview(hromada_map, zcol = "perc", layer.name = paste("Hromada map -", input$option, "option"),
                  col.regions = palette_function(10),
                  map.types = c("CartoDB.Positron",
                                "OpenStreetMap",
                                "Esri.WorldImagery",
                                "OpenTopoMap"))@map
        })
      }
      
    }
    
  })
  
  ##### numeric
  # Event handler for project selection, updates the list of variables and multi-variable selection for numeric data
  # related to Numeric page
  observeEvent(ignoreInit = FALSE, input$project_numeric, {
    if (!is.null(input$project_numeric) && !is.null(numeric_data()) && nrow(numeric_data()) > 0) {
      # Filter the numeric data for the selected project
      filtered_df <- numeric_data() %>% filter(TABLE_ID == input$project_numeric)
      # Extract unique variable names and update the single-variable input
      variables <- unique(filtered_df$variable_orig)
      updateSelectizeInput(session, "variable_orig_numeric", choices = variables)
      # Extract unique variable names for multi-variable selection and set default selection
      variables_m <- unique(numeric_data()$variable_orig)
      updateSelectizeInput(session, "variable_orig_m_numeric", choices = variables_m, selected = variables_m[1])
      
    }
    
  })
  # Event handler for updating disaggregation and option selection based on selected project and variable
  # related to Numeric page
  observeEvent(ignoreInit = FALSE, list(
    input$project_numeric, input$variable_orig_numeric
  ), {
    if (!is.null(input$variable_orig_numeric) && !is.null(numeric_data()) && nrow(numeric_data()) > 0) {
      
      # Filter the data based on selected project and variable
      filtered_df <- numeric_data() %>% 
        filter(TABLE_ID == input$project_numeric & variable_orig == input$variable_orig_numeric)
      
      # Update disaggregation input with unique categories
      updateSelectizeInput(session, "dissagr_numeric", 
                           choices = unique(filtered_df$disaggregations_1), 
                           selected = " Overall")
      
      # Define and update options (e.g., mean, min, max, median)
      options <- c("mean", "min", "max", "median")
      updateSelectizeInput(session, "option_numeric", choices = options)
    }
  })
  
  # Event handler for processing numeric data and rendering visualizations, related to Numeric page
  observeEvent(ignoreInit = TRUE, list(
    input$project_numeric,
    input$variable_orig_numeric,
    input$option_numeric,
    input$dissagr_numeric), {
      if (!is.null(input$option_numeric) & !is.null(numeric_data()) & nrow(numeric_data())> 0 & input$option_numeric != '' &
          !is.null(input$dissagr_numeric)) {
        
        processed_numerics <- numeric_data()
        
        # Filter data based on project, variable, and disaggregation selection
        filtered_df <- processed_numerics %>%
          dplyr::filter(TABLE_ID == input$project_numeric & variable_orig == input$variable_orig_numeric & 
                          disaggregations_1 %in% c(input$dissagr_numeric, " Overall"))  %>%
          distinct()
        
        # Extract basic statistics based on the selected option
        basic_stat_number <- filtered_df %>% 
          dplyr::filter(admin_category%in%c(' Overall','Overall') & disaggregations_category_1 %in% c('Overall',' Overall')) %>% 
          dplyr::pull(!!sym(input$option_numeric))
        
        # Prepare data for disaggregation graph
        graph_base_n2 <- filtered_df %>% 
          dplyr::filter(variable_orig %in% input$variable_orig_numeric,
                 admin %in% 'Overall',
                 TABLE_ID %in% input$project_numeric) %>% 
          rowwise() %>% 
          dplyr::mutate(variable = paste(strwrap(variable, width = 35), collapse = "<br>"),
                 vis_variable  = !!sym(input$option_numeric))
        
        # Format graph title
        title <- paste0(unique(graph_base_n2$variable),'<br>')
        # cut off the title if it is too long
        title <- substr(title, 1, 50)
        title <- paste0(title, '...')
        title <- paste0(title,'<br>')
        
        # Check for additional categories and adjust data
        check_categories <- setdiff(unique(graph_base_n2$disaggregations_category_1), c(' Overall','Overall'))
        
        title_text = "Summary Statistics"
        if(length(check_categories)>0){
          title_text = "Summary Statistics"
          graph_base_n2 <- graph_base_n2 %>% 
            dplyr::filter(!disaggregations_category_1 %in% c(' Overall','Overall'))
        }
        # Reshape data for visualization
        graph_base_n2 <- graph_base_n2 %>%
          pivot_longer(cols = c('mean', 'min', 'max', 'median'), names_to = 'statistic', values_to = 'value')
        
        # Render bar chart for summary statistics
        output$graph_1_numeric <- renderPlotly({
          plot_ly(
            graph_base_n2, 
            x = ~statistic, 
            y = ~value, 
            type = 'bar', 
            color = ~disaggregations_category_1,
            text = ~value, 
            textposition = 'auto'
          ) %>%
            layout(
              barmode = 'group',
              title = list(
                text = paste(input$variable_orig_numeric, "summary statistics by", input$dissagr_numeric),
                font = list(size = 24)
              ),
              xaxis = list(
                titlefont = list(size = 18),
                tickfont = list(size = 14)
              ),
              yaxis = list(
                titlefont = list(size = 18),
                tickfont = list(size = 14)
              ),
              margin = list(t = 50, b = 50),
              paper_bgcolor = 'rgba(245, 246, 249, 1)',
              plot_bgcolor = 'rgba(245, 246, 249, 1)',
              hovermode = 'closest'
            )
        })
        
        # Create maps for oblast, raion, and hromada levels
        #### oblast plot
        oblast_map_numeric <- filtered_df %>%
          inner_join(oblast_json, by = c("admin_category_orig" = "ADM1_PCODE"))
        
        oblast_map_numeric <- st_as_sf(oblast_map_numeric)
        
        #### raion plot
        raion_map_numeric <- filtered_df %>%
          inner_join(raion_json, by = c("admin_category_orig" = "ADM2_PCODE"))
        
        raion_map_numeric <- st_as_sf(raion_map_numeric)
        
        #### hromada plot
        hromada_map_numeric <- filtered_df %>%
          inner_join(hromada_json, by = c("admin_category_orig" = "ADM3_PCODE"))
        
        hromada_map_numeric <- st_as_sf(hromada_map_numeric)
        
        # Render maps for each geographic level if data is available
        if (nrow(oblast_map_numeric) > 0) {
          output$map_oblast_numeric <- renderLeaflet({
            req(oblast_map_numeric)
            mapview(oblast_map_numeric, zcol = input$option_numeric, 
                    layer.name = paste("Oblast map -", input$option_numeric, "statistic"),
                    col.regions = palette_function(10), 
                    map.types = c("CartoDB.Positron",
                                  "OpenStreetMap",
                                  "Esri.WorldImagery",
                                  "OpenTopoMap"))@map
          })
        }
        
        if (nrow(raion_map_numeric) > 0) {
          output$map_raion_numeric <- renderLeaflet({
            req(raion_map_numeric)
            mapview(raion_map_numeric, zcol = input$option_numeric,
                    layer.name = paste("Raion map -", input$option_numeric, "statistic"),
                    col.region = palette_function(10),
                    map.types = c("CartoDB.Positron",
                                  "OpenStreetMap",
                                  "Esri.WorldImagery",
                                  "OpenTopoMap"))@map
          })
        }
        
        if (nrow(hromada_map_numeric) > 0) {
          output$map_hromada_numeric <- renderLeaflet({
            req(hromada_map_numeric)
            mapview(hromada_map_numeric, zcol = input$option_numeric, 
                    layer.name = paste("Hromada map -", input$option_numeric, "statistic"),
                    col.regions = palette_function(10),
                    msp.types = c("CartoDB.Positron",
                                  "OpenStreetMap",
                                  "Esri.WorldImagery",
                                  "OpenTopoMap"))@map
          })
        }
      }
      
    })
  
  # Event handler for the selection of variables in the multi-variable timeline graph, related to Timeline numeric page
  observeEvent(ignoreInit = TRUE, list(
    input$variable_orig_m_numeric,
    input$option_numeric),{
      if(!is.null(input$variable_orig_m_numeric) & !is.null(numeric_data()) & input$option_numeric!=""){
        
        processed_numerics <- numeric_data()
        
        # timeline graph
        graph_base3 <- processed_numerics %>% 
          filter(variable_orig %in% input$variable_orig_m_numeric,
                 disaggregations_category_1 %in% c(' Overall','Overall'),
                 admin %in% 'Overall')%>% 
          rowwise() %>% 
          mutate(variable = paste(strwrap(variable, width = 35), collapse = "<br>"),
                 period_full = paste(month_conducted,TABLE_ID,variable,sep = '<br>'),
                 viz_variable = !!sym(input$option_numeric))  %>%
          distinct()
        
        title <- paste0(unique(graph_base3$variable),'<br>')
        # cut off the title if it is too long
        title <- substr(title, 1, 50)
        title <- paste0(title, '...')
        title <- paste0(title,'<br>')
        
        graph_base3 <- graph_base3 %>%
          pivot_longer(cols = c('mean', 'min', 'max', 'median'),
                       names_to = 'statistic',
                       values_to = 'value')
        
        output$graph_2_numeric <- renderPlotly({
          plot <- plot_ly(
            graph_base3,
            x = ~period_full,
            y = ~value,
            color = ~statistic,
            colors = c('mean' = '#40AB5D',  # Soft blue
                       'max' = '#EE5859',   # Warm peach
                       'median' = '#E9C46A',# Soft yellow
                       'min' = '#4096AA'),   # Calm teal
            
            type = 'bar',
            text = ~paste(statistic, ": ", value),
            hoverinfo = "text"
          ) %>%
            layout(
              barmode = 'group',
              title = list(
                text = "Summary Statistics by Variable and Project",
                font = list(size = 24)
              ),
              xaxis = list(
                titlefont = list(size = 18),
                tickfont = list(size = 14)
              ),
              yaxis = list(
                titlefont = list(size = 18),
                tickfont = list(size = 14)
              ),
              margin = list(t = 50, b = 100),
              hovermode = 'closest'
            )
        })
        
      }
    })
  
  
  ################################## GEO PART ##################################
  # Define reactive variables for storing data
  points <- reactiveVal(data.frame(lng = numeric(0), lat = numeric(0)))
  admin_map = reactiveVal()
  info_table = reactiveVal()
  
  # Observe the geo_admin_level input, updates the map based on selected administrative level
  # related to Geospatial request page
  observeEvent(input$geo_admin_level,{
    admin_map(switch(input$geo_admin_level,
                     "oblast" = oblast_json,
                     "raion" = raion_json,
                     "hromada" = hromada_json,
                     "settlement" = settlement_json,
                     oblast_json))  # Assign map data based on selection
    # Update the map based on the selected oblast and admin level
    if (input$geo_admin_level == "hromada" | input$geo_admin_level == "settlement") {
      if ("Overall" %in% input$geo_defined_oblast) {
        if (input$geo_admin_level == "hromada") {
          admin_map(hromada_json)
        }
        else {
          admin_map(settlement_json)
        }
      } else {
        oblast_row <- oblasts[oblasts$admin1Name_eng %in% input$geo_defined_oblast, ]
        if (input$geo_admin_level == "hromada") {
          admin_map(hromada_json %>% dplyr::filter(ADM1_PCODE %in% oblast_row$admin1Pcode))
        } else {
          admin_map(settlement_json %>% dplyr::filter(ADM1_PCODE %in% oblast_row$admin1Pcode))
        }
      }
    }
    # Reset points and tables
    points(data.frame(lng = numeric(0), lat = numeric(0)))
    ### reset question table and info table
    output$geo_table <- renderRHandsontable({
      NULL
    })
    
    info_table(NULL)
    output$geo_questions_table <- renderDT({
      NULL
    })
  })
  # Reactive values from selectizers
  rv <- reactiveValues(selected_oblasts=c(),selected_raions=c(),selected_hromadas=c(),selected_settlements=c(),selected_units = c())
  rv_pcode <- reactiveValues(selected_oblasts=c(),selected_raions=c(),selected_hromadas=c(),selected_settlements=c())
  # Reactive selectizer for oblast
  output$geo_defined_oblast_ui <- renderUI({
    req(input$geo_admin_level)
    
    default_choices <- c("Overall", oblasts$admin1Name_eng)
    
    # If no reactive selection has been set yet, apply a default for certain levels
    selected_vals <- if (length(rv$selected_oblasts) == 0 && input$geo_admin_level %in% c("raion", "hromada", "settlement")) {
      c("Kyivska")
    } else {
      rv$selected_oblasts
    }
    selectizeInput("geo_defined_oblast", "Specify oblasts", 
                   choices = default_choices,
                   multiple = TRUE,
                   selected = selected_vals)
    
  })
  
  # Observe the geo_defined_oblast input, updates the raion selectizer
  observeEvent(input$geo_defined_oblast, {
    req(input$geo_defined_oblast)
    rv$selected_oblasts <- input$geo_defined_oblast
    rv_pcode$selected_oblasts <- oblasts %>% filter(admin1Name_eng %in% rv$selected_oblasts) %>% pull(admin1Pcode)
    
    # Filter raion names by selected oblasts
    filtered_raions <- data.frame(admin1Pcode = rv_pcode$selected_oblasts) %>%
      left_join(raions,by = "admin1Pcode") %>%
      pull(admin2Name_eng) %>%
      unique() %>%
      sort()
    ### Update map
    if (input$geo_admin_level != "oblast"){
    admin_map(eval(as.name(paste0(input$geo_admin_level,"_json"))) %>% dplyr::filter(ADM1_PCODE %in% rv_pcode$selected_oblasts))
    }
    output$geo_defined_raion_ui <- renderUI({
      selectizeInput("geo_defined_raion", "Specify raions", 
                     choices = filtered_raions, 
                     multiple = TRUE,
                     selected = rv$selected_raions)
    })
  })
  
  # Observe the geo_defined_raion input, updates the hromada selectizer
  observeEvent(input$geo_defined_raion, {
    req(input$geo_defined_raion)
    rv$selected_raions <- input$geo_defined_raion
    rv_pcode$selected_raions <- data.frame(admin1Pcode = rv_pcode$selected_oblasts) %>%
      left_join(raions,by = "admin1Pcode") %>% filter(admin2Name_eng %in% rv$selected_raions) %>% pull(admin2Pcode)
    # Filter hromada names by selected raions
    filtered_hromadas <- data.frame(admin2Pcode = rv_pcode$selected_raions) %>%
      left_join(hromadas,by = "admin2Pcode") %>%
      pull(admin3Name_eng) %>%
      unique() %>%
      sort()
    ### Update map
    if (input$geo_admin_level != "raion"){
    admin_map(eval(as.name(paste0(input$geo_admin_level,"_json"))) %>% dplyr::filter(ADM2_PCODE %in% rv_pcode$selected_raions))
    }
    output$geo_defined_hromada_ui <- renderUI({
      selectizeInput("geo_defined_hromada", "Specify hromadas", 
                     choices = filtered_hromadas, 
                     multiple = TRUE,
                     selected = rv$selected_hromadas)
    })
  })
  # Observe the geo_defined_hromada input, updates the settlement selectizer
  observeEvent(input$geo_defined_hromada, {
    req(input$geo_defined_hromada)
    rv$selected_hromadas <- input$geo_defined_hromada
    rv_pcode$selected_hromadas <- data.frame(admin2Pcode = rv_pcode$selected_raions) %>%
      left_join(hromadas,by = "admin2Pcode") %>% filter(admin3Name_eng %in% rv$selected_hromadas) %>% pull(admin3Pcode)
    # Filter hromada names by selected raions
    filtered_settlements <- data.frame(admin3Pcode = rv_pcode$selected_hromadas) %>%
      left_join(settlements,by = "admin3Pcode") %>%
      pull(admin4Name_eng) %>%
      unique() %>%
      sort()
    ### Update map
    if (input$geo_admin_level != "hromada"){
    admin_map(eval(as.name(paste0(input$geo_admin_level,"_json"))) %>% dplyr::filter(ADM3_PCODE %in% rv_pcode$selected_hromadas))
    }
    output$geo_defined_settlement_ui <- renderUI({
      selectizeInput("geo_defined_settlement", "Specify settlements", 
                     choices = filtered_settlements, 
                     multiple = TRUE,
                     selected = rv$selected_settlements)
    })
  })
  # Observe the geo_defined_settlemet input to get Pcodes
  observeEvent(input$geo_defined_settlement, {
    req(input$geo_defined_settlement)
    rv$selected_settlements <- input$geo_defined_settlement
    rv_pcode$selected_settlements <- data.frame(admin3Pcode = rv_pcode$selected_hromadas) %>%
      left_join(settlements,by = "admin3Pcode") %>% filter(admin4Name_eng %in% rv$selected_settlements) %>% pull(admin4Pcode)
  })
  # Observe the geo_defined_settlemet input to get Pcodes
  observeEvent(input[[paste0("geo_defined_",input$geo_admin_level)]], {
    #req(eval(as.name(paste0("input$geo_defined_",input$geo_admin_level))))
    rv$selected_units <- input[[paste0("geo_defined_",input$geo_admin_level)]]
    rv_pcode$selected_units <- rv_pcode[[paste0("selected_", input$geo_admin_level, "s")]]
  })

  # Observe the geo_defined_oblast input, updates the map based on selected oblast
  # plot only needed area, improve time performance
  # related to Geospatial request page
  observeEvent(input$geo_defined_oblast, {
    # Skip if admin level is "oblast" or "raion"
    if (input$geo_admin_level %in% c("oblast", "raion")) {
      return()
    }
    # Handle map updates for hromada and settlement based on the selected oblast
    if (input$geo_admin_level %in% c("hromada","settlement")) {
      if ("Overall" %in% input$geo_defined_oblast) {
        if (input$geo_admin_level == "hromada") {
          admin_map(hromada_json)
        }
        else {
          admin_map(settlement_json)
        }
      } else {
        oblast_row <- oblasts[oblasts$admin1Name_eng %in% input$geo_defined_oblast, ]
        if (input$geo_admin_level == "hromada") {
          admin_map(hromada_json %>% dplyr::filter(ADM1_PCODE %in% oblast_row$admin1Pcode))
        } else {
          admin_map(settlement_json %>% dplyr::filter(ADM1_PCODE %in% oblast_row$admin1Pcode))
        }
      }
    }
    
    # Reset points and tables
    points(data.frame(lng = numeric(0), lat = numeric(0)))
    ### reset question table and info table
    output$geo_table <- renderRHandsontable({
      NULL
    })
    
    info_table(NULL)
    output$geo_questions_table <- renderDT({
      NULL
    })
  })
  
  # Render the map with the current administrative level and data
  output$geo_map <- renderLeaflet({
    if (is.null(admin_map())) {
      return()
    }
    
    labelName <- switch(input$geo_admin_level,
                        "oblast" = "ADM1_EN",
                        "raion" = "ADM2_EN",
                        "hromada" = "ADM3_EN",
                        "settlement" = "ADM4_EN")
    
    labels <- admin_map()[[labelName]]
    
    leaflet() %>%
      addPolygons(data = admin_map(), color = "#4096AA", fill = TRUE, fillColor = "#4096AA", fillOpacity = 0.2, weight = 1, label = labels) %>%
      addTiles() %>%
      setView(lng = 31.1656, lat = 48.3794, zoom =6)
  })
  
  # Handle map clicks to add points and update the map with selected points, related to Geospatial request page
  observeEvent(input$geo_map_click, {
    click <- input$geo_map_click
    new_point <- data.frame(lng = click$lng, lat = click$lat)
    points(rbind(points(), new_point)) # Add new point to the points reactive variable
    
    current_points <- points()
    
    labelName <- switch(input$geo_admin_level,
                        "oblast" = "ADM1_EN",
                        "raion" = "ADM2_EN",
                        "hromada" = "ADM3_EN",
                        "settlement" = "ADM4_EN")
    
    labels <- admin_map()[[labelName]]
    
    leafletProxy("geo_map") %>%
      clearShapes() %>%
      addPolygons(lng = current_points$lng, lat = current_points$lat, color = "red", fill = TRUE, fillColor = "red", weight = 4) %>%
      addPolygons(data = admin_map(), color = "#4096AA", fill = TRUE, fillColor = "#4096AA", fillOpacity = 0.2, weight = 1, label = labels) %>%
      addMarkers(lng = current_points$lng, lat = current_points$lat)
  })
  
  # Reset points, map, and tables
  observeEvent(input$geo_reset, {
    points(data.frame(lng = numeric(0), lat = numeric(0)))
    
    labelName <- switch(input$geo_admin_level,
                        "oblast" = "ADM1_EN",
                        "raion" = "ADM2_EN",
                        "hromada" = "ADM3_EN",
                        "settlement" = "ADM4_EN")
    labels <- admin_map()[[labelName]]
    
    leafletProxy("geo_map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      addPolygons(data = admin_map(), color = "#4096AA", fill = TRUE, fillColor = "#4096AA", fillOpacity = 0.2, weight = 1, label = labels) %>%
      setView(lng = 31.1656, lat = 48.3794, zoom = 6)
    
    ### reset question table and info table
    output$geo_table <- renderRHandsontable({
      NULL
    })
    
    info_table(NULL)
    
    output$geo_questions_table <- renderDT({
      NULL
    })
    
    ### reset selectizer
    rv[[paste0("selected_",input$geo_admin_level,"s")]] <- c()
    rv_pcode[[paste0("selected_",input$geo_admin_level,"s")]] <- c()
    rv$selected_units <- c()
    rv_pcode$selected_units <- c()
    updateSelectizeInput(session, paste0("geo_defined_",input[["geo_admin_level"]]),
                         selected = rv$selected_units)

    
  })
  
  intersections <- reactiveVal()
  
  # Handle request to get information based on selected points, related to Geospatial request page
  observeEvent(input$geo_get_info, {
    # Handle geometry creation based on the number of points selected
    if (nrow(points()) > 2) {
      closed_points <- rbind(
        data.frame(lng = points()$lng, lat = points()$lat),
        data.frame(lng = points()$lng[1], lat = points()$lat[1])
      )
      geometry <- st_sfc(st_polygon(list(as.matrix(closed_points))), crs = 4326)
    }
    
    if (nrow(points()) == 2) {
      closed_points <- rbind(
        data.frame(lng = points()$lng, lat = points()$lat)
      )
      geometry <- st_sfc(st_linestring(as.matrix(closed_points)), crs = 4326)
    }
    
    if (nrow(points()) == 1) {
      closed_points <- rbind(
        data.frame(lng = points()$lng, lat = points()$lat)
      )
      geometry <- st_sfc(st_point(as.matrix(closed_points)), crs = 4326)
    }
    
    admin_name <- switch(input$geo_admin_level,
                         "oblast" = "admin1Name_eng",
                         "raion" = "admin2Name_eng",
                         "hromada" = "admin3Name_eng",
                         "settlement" = "admin4Name_eng")
    admin_pcode <- switch(input$geo_admin_level,
                          "oblast" = "admin1Pcode",
                          "raion" = "admin2Pcode",
                          "hromada" = "admin3Pcode",
                          "settlement" = "admin4Pcode")
    # Check if geometry is valid and find intersections with the admin map
    if (exists("geometry")){
      if (st_is_valid(geometry)) {
        admin_map_planar <- st_transform(admin_map(), 3857)
        geometry_planar <- st_transform(geometry, 3857)
        # Find intersections or coverage based on user selection
        if (input$geo_relation_type == "covers") {
          intersections_admin <- admin_map()[unlist(st_covers(geometry_planar, admin_map_planar)), ]
        } else if (input$geo_relation_type == "intersects") {
          intersections_admin <- admin_map()[unlist(st_intersects(geometry_planar, admin_map_planar)), ]
        }
        # Filter representation data based on intersections
        admin_column <- switch(input$geo_admin_level,
                               "oblast" = "ADM1_PCODE",
                               "raion" = "ADM2_PCODE",
                               "hromada" = "ADM3_PCODE",
                               "settlement" = "ADM4_PCODE")
        intersections(data.frame(
          "value" = intersections_admin[[admin_column]]
        ))
        
      }
      
      
      new_values <- eval(as.name(paste0(input$geo_admin_level,"s"))) %>% filter(eval(as.name(admin_pcode)) %in% intersections()$value) %>% pull(eval(as.name(admin_name)))
        
      # Combine with current selections and remove duplicates
      combined <- unique(c(rv$selected_units, new_values))
      combined_pcodes <- unique(c(rv_pcode$selected_units,intersections()$value))
      # Update reactive value
      rv$selected_units <- combined
      rv_pcode$selected_units <- combined_pcodes
      selected_pcodes <- combined_pcodes
      # Update the input in UI
      updateSelectizeInput(session, paste0("geo_defined_",input$geo_admin_level),
                           selected = combined)
      
    } else {
      selected_pcodes <- rv_pcode[[paste0("selected_",input$geo_admin_level,"s")]]
    }
    representation_data_filtered <- representation_data %>%
        dplyr::filter(map_lgl(representation_data[[input$geo_admin_level]], ~ any(. %in% selected_pcodes))) %>%
        dplyr::select(c("Name", "Project", "Round", "Type", "Interview_date")) %>%
        dplyr::mutate(Get_info = FALSE)
      
      
      
      # sort representation_data_filtered by Project
      representation_data_filtered <- representation_data_filtered[order(representation_data_filtered$Project),]
      representation_data_filtered <- representation_data_filtered %>%
        dplyr::arrange(Name, Interview_date)
      output$geo_table <- renderRHandsontable({
        rhandsontable(
          representation_data_filtered,
          rowHeaders = NULL,
          width = "1000px",
          readOnly = TRUE
        ) %>%
          hot_col("Get_info", readOnly = FALSE, valign = 'htCenter') %>%
          hot_col("Interview_date", valign = 'htCenter') %>%
          hot_col("Round", valign = 'htCenter') %>%
          hot_cols(colWidths = c(310, 160, 110, 160, 160, 100))
      })
    
  })
#  
#  # Follow the selections on the map and add them to the selectizer
#  observeEvent(input$geo_defined_oblast, {
#    rv$selected_oblasts <- input$geo_defined_oblast
#    rv_pcode$selected_oblasts <- oblasts %>% filter(admin1Name_eng %in% input$geo_defined_oblast) %>% pull(admin1Pcode)
#  })
#  observeEvent(input$geo_defined_raions, {
#    rv$selected_raions <- input$geo_defined_raions
#    rv_pcode$selected_raions <- data.frame(admin1Pcode = rv_pcode$selected_oblasts) %>%
#      left_join(raions,by = "admin1Pcode") %>% filter(admin2Name_eng %in% rv$selected_raions) %>% pull(admin2Pcode)
#  })
#
  
  
  # Handle table interactions and updating the geo_table table
  observeEvent(input$geo_table, {
    info_table(hot_to_r(input$geo_table))
  })
  
  # Check whether the 'Get_info' button should be shown(only if points or polygons were drawn)
  showGetButton <- reactive({
    if (!is.null(info_table())) {
      user_data <- info_table()
      if (sum(user_data$Get_info) > 0) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
    return(FALSE)
  })
  
  output$geo_showGetButton <- reactive({
    showGetButton()
  })
  
  outputOptions(output, "geo_showGetButton", suspendWhenHidden = FALSE)
  
  # Handle user selection for requesting data, realted to Question observer page
  observeEvent(input$geo_get_questions, {
    
    user_data <- as_tibble(hot_to_r(input$geo_table)) %>%
      dplyr::filter(Get_info == TRUE)
    user_data$TABLE_ID <- paste(user_data$Project, user_data$Round, user_data$Type, sep = "_")
    table_Ids <- unique(user_data$TABLE_ID)
    
    # Fetch questions based on selected table IDs
    question.table <- get.questions(table_Ids) %>%
      dplyr::rename(Project_Round_Type = TABLE_ID, "english_question_label" = english_question, "ukrainian_question_label" = ukrainian_question)
    
    # Add checkboxes for user selection of questions
    question.table <- cbind(question.table, check = shinyInput(checkboxInput, nrow(question.table), "chec"))
    
    # Render the question table
    output$geo_questions_table <- renderDT({
      datatable(question.table,
                filter = "top",
                selection = 'none',
                rownames = FALSE,
                escape = FALSE,
                editable = list(target = "cell", disable = list(columns = 8)),
                callback = JS(geo_js),
                options = list(
                  pageLength = nrow(question.table),
                  autoWidth = TRUE
                  )) %>%
        formatStyle(
          columns = names(question.table),
          valueColumns = NULL,
          target = 'cell',
          backgroundColor = styleEqual(NA, 'white'),
          border = '1px solid black'
        )
    }, server = FALSE)
    
    # Switch tab to "Questions observer"
    updateTabsetPanel(session, inputId = "tabs", selected = "Questions observer")
    
    question_table.reactive(cbind(question.table, bool = FALSE))
  })
  
  question_table.reactive <- reactiveVal()
  
  # Update table when cell is edited
  observeEvent(input$geo_table_cell_edit, {
    info <- input$geo_table_cell_edit
    question_table.reactive(editData(question_table.reactive(), info))
    
    # Show or hide the 'Get request' button based on user selection
    if (any(question_table.reactive()$bool)) {
      output$geo_get_request <- renderUI({
        tagList(
          actionButton("geo_get_request", "Get request"),
          div(style = "height: 10px;"),
          span('If you want to save the current selection of variables for the future, you can download an excel file with them',
               style = "font-weight: bold;"),
          div(style = "height: 10px;"),
          downloadButton("excel_geo", "Download Excel"),
        )
      })
    } else {
      output$geo_get_request <- renderUI({
        NULL
      })
    }
  })
  
  # Handle the 'Get request' action to send data for processing
  observeEvent(input$geo_get_request, {
    requested_data <- question_table.reactive() %>%
      dplyr::filter(bool == TRUE)
    
    admin_level <- input$geo_admin_level
    
    daf <- requested_data %>%
      left_join(tool_survey, by = c("Project_Round_Type" = "TABLE_ID", "english_question_label" = "label::English")) %>%
      dplyr::mutate(adm_class = admin_level, disaggregations = "Overall", disaggregations_label = "Overall",join = NA, 
                    ID = 1:nrow(.), func = case_when(
                      q.type.x %in% c('select_one','select_multiple') ~ 'freq',
                      q.type.x %in% c('integer','decimal') ~ 'numeric',
                      .default = 'freq'),
                    calculation = "empty",
                    DB_table_name = paste("data", Project_Round_Type, datasheet, "DCMPR", sep='_')) %>%
      dplyr::rename(TABLE_ID = Project_Round_Type, variable_label = english_question_label, variable = name, q.type = q.type.x) %>%
      dplyr::select(TABLE_ID, ID, variable, variable_label, calculation, func, adm_class, disaggregations, disaggregations_label, q.type, join, datasheet, DB_table_name)
      

    admin_names <- c('oblast','raion','hromada','settlement')
    
    
    # get the names of actual admin columns
    admin_names_map <- dbGetQuery(my_connection , "SELECT TABLE_ID,representative_columns from data_representative_table") %>% 
      mutate(representative_columns  = ifelse(representative_columns =='None',NA,representative_columns )) %>% 
      rowwise() %>% 
      mutate(adm_class = ifelse(!is.na(representative_columns),
                                paste0(admin_names[1:(str_count(representative_columns,';')+1)],collapse = ';'),
                                NA)) %>% 
      ungroup()%>%
      mutate(across(c(representative_columns, adm_class), ~strsplit(as.character(.), ";"))) %>%
      unnest(c(representative_columns, adm_class)) %>%
      filter(!is.na(representative_columns) & !is.na(adm_class)) %>% 
      rename(admin = representative_columns)
    
    # merge into the DAF
    daf <- daf %>% 
      left_join(admin_names_map) %>% 
      mutate(admin_var = admin)
  
    daf <- rbind(daf,daf %>% 
      distinct() %>% 
      mutate(disaggregations='empty',
             admin ='Overall')) %>% 
      mutate(ID = 1:nrow(.))
    

    rep_table_overview <-  dbGetQuery(my_connection , "SELECT TABLE_ID,datasheet_names,main_datasheet from data_representative_table")
    
    general_info <- requested_data %>%
      left_join(rep_table_overview, by = c("Project_Round_Type" = "TABLE_ID")) %>%
      dplyr::rename(TABLE_ID = Project_Round_Type) %>%
      dplyr::mutate(project_ID = get.project.name(TABLE_ID),
                    round_ID = rev(unlist(stringr::str_split(TABLE_ID, "_")))[[2]],
                    survey_type = rev(unlist(stringr::str_split(TABLE_ID, "_")))[[1]],
                    main_sheet_name = paste("data", TABLE_ID, main_datasheet, "DCMPR", sep='_'))
    
    
    time_table <- as_tibble(hot_to_r(input$geo_table)) %>% 
      mutate(TABLE_ID = paste0(Project,'_',Round,'_',Type))
      
    
    general_info <- general_info %>%
      left_join(time_table, by = c("TABLE_ID" = "TABLE_ID")) %>%
      dplyr::mutate(round_ID =  gsub("[^0-9]", "", round_ID)) %>%
      dplyr::rename(month_conducted = Interview_date) %>%
      select(TABLE_ID, project_ID, round_ID, survey_type, month_conducted, main_sheet_name)
    
    weight_table <-  dbGetQuery(my_connection , paste0(
      "SELECT TABLE_NAME, COLUMN_NAME
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_NAME in ('",paste0(unique(general_info$main_sheet_name), collapse ="','")  ,"')"))
    
    weight_table <- weight_table %>% 
      filter(grepl('weight',COLUMN_NAME)) %>% 
      rename(main_sheet_name =TABLE_NAME,
             weight_column_name = COLUMN_NAME)
    
    if (nrow(weight_table) > 0){
      general_info <- general_info %>% 
        left_join(weight_table)
    } else {
      general_info$weight_column_name <- NA
    }
    
    if(nrow(general_info[is.na(general_info$weight_column_name),])>0){
      general_info[is.na(general_info$weight_column_name),]$weight_column_name <- 'empty'
    }
    
    showModal(
      modalDialog(
        title = "Processing",
        "Sending your request to the processing system.",
        footer = NULL,
        easyClose = TRUE
      )
    )

    filter <- daf %>%
      dplyr::select(ID, TABLE_ID, admin_var) %>%
      cross_join( intersections()) %>% 
      mutate(operation = '==') %>% 
      rename(variable = admin_var)
    
    # write.xlsx(daf, "daf_geo.xlsx")
    # write.xlsx(general_info, "gen_info_geo.xlsx")
    # write.xlsx(filter, "filter_geo.xlsx")

    json_body <<- list(
      daf_file = daf,
      info = general_info,
      filter = filter
    )
    url <- Sys.getenv('url')
    
    response <- POST(url, body = json_body, encode = "json")
    print(status_code(response))
    
    char <- rawToChar(response$content)
    df <- fromJSON(char)
    
    df_final <- as.data.frame(do.call(cbind,df$result))
    
    df <- purrr::map_dfc(df_final, ~ purrr::map(.x, unlist_with_na) %>% unlist())
    
    df <- df %>% 
      left_join(general_info %>% select(TABLE_ID,month_conducted) %>% distinct())
    
    print(unique(df$admin_category))
    
    removeModal()
    processed_data(df)
  })
  
  
  ################### data columns explorer ########################
  
  # Reactive values and variables for managing dynamic inputs and their states
  column_field_count <- reactiveVal(1)
  column_values <- reactiveValues()
  column_choices <- reactiveValues()
  
  # Initialize the first selectize input with available projects from the database
  updateSelectizeInput(session, "column_project_search_1", choices = c("", sort(unique(projects_data$TABLE_ID))))
  
  # Observers for updating reactive column_values when column_questions_ have changes
  observe({
    n <- column_field_count()
    lapply(1:n, function(i) {
      local({
        j <- i
        # Update column_values based on changes in "column_questions_" inputs
        observeEvent(input[[paste0("column_questions_", j)]], {
          if (!is.null(input[[paste0("column_questions_", j)]]) && input[[paste0("column_questions_", j)]] != "") {
            column_values[[paste0("column_questions_", j)]] <- input[[paste0("column_questions_", j)]]
          }
          
        }, ignoreInit = TRUE)
      })
    })
  })
  
  # Observers for updating reactive column_values when column_project_search_ have changes
  observe({
    n <- column_field_count()
    lapply(1:n, function(i) {
      local({
        j <- i
        # Update column_values based on changes in "column_project_search_" inputs
        observeEvent(input[[paste0("column_project_search_", j)]], {
          if (!is.null(input[[paste0("column_project_search_", j)]]) && input[[paste0("column_project_search_", j)]] != "") {
            column_values[[paste0("column_project_search_", j)]] <- input[[paste0("column_project_search_", j)]]
          }
        }, ignoreInit = TRUE)
      })
    })
  })
  # Observers for updating reactive column_values when column_dissagr_ have changes
  observe({
    n <- column_field_count()
    lapply(1:n, function(i) {
      local({
        j <- i
        # Update column_values based on changes in "column_dissagr_" inputs
        observeEvent(input[[paste0("column_dissagr_", j)]], {
          if (!is.null(input[[paste0("column_dissagr_", j)]]) && input[[paste0("column_dissagr_", j)]][[1]] != "") {
            column_values[[paste0("column_dissagr_", j)]] <- input[[paste0("column_dissagr_", j)]]
          }
          
        }, ignoreInit = TRUE)
      })
    })
  })
  
  # Observers for updating reactive column_values when column_admins_ have changes
  observe({
    n <- column_field_count()
    lapply(1:n, function(i) {
      local({
        j <- i
        # Update column_values based on changes in "column_admins_" inputs
        observeEvent(input[[paste0("column_admins_", j)]], {
          if (!is.null(input[[paste0("column_admins_", j)]]) && input[[paste0("column_admins_", j)]][[1]] != "") {
            column_values[[paste0("column_admins_", j)]] <- input[[paste0("column_admins_", j)]]
          }
          
        }, ignoreInit = TRUE)
      })
    })
  })
  
  # observe new fields creation, change column_field_count
  observe({
    n <- column_field_count()  # Receive current number of fields(rows)
    
    if (!is.null(input[[paste0("column_questions_", n)]]) && input[[paste0("column_questions_", n)]] != "") {
      column_field_count(n + 1)
    }
  })
  
  #observe column_project_search_ and extract relevant questions
  observe({
    n <- column_field_count()
    
    lapply(1:n, function(i) {
      local({
        j <- i
        
        observeEvent(input[[paste0("column_project_search_", j)]], {
          if (!is.null(input[[paste0("column_project_search_", j)]]) && input[[paste0("column_project_search_", j)]] != "") {
            
            main_datasheet <- projects_data %>%
              dplyr::filter(TABLE_ID == input[[paste0("column_project_search_", j)]]) %>%
              dplyr::select(main_datasheet) %>%
              pull()
            
            columns <- tool_survey %>%
              dplyr::filter(TABLE_ID == input[[paste0("column_project_search_", j)]] & q.type %in% c("select_one", "select_multiple", "integer", "decimal")) %>%
              dplyr::select(name) %>%
              ungroup() %>%
              pull()
            
            dissagr_columns <- tool_survey %>%
              dplyr::filter(TABLE_ID == input[[paste0("column_project_search_", j)]] & q.type %in% c("select_one", "select_multiple", "integer", "decimal") &
                              datasheet %in% main_datasheet) %>%
              dplyr::select(name) %>%
              ungroup() %>%
              pull()
            
            admins_columns <- projects_data %>%
              dplyr::filter(TABLE_ID == input[[paste0("column_project_search_", j)]]) %>%
              dplyr::select(representative_columns) %>%
              pull()
            
            if (admins_columns == "None") {
              admins_columns <- c()
            } else {
              admins_columns <- unlist(strsplit(admins_columns, ';'))
              admins_columns <- admins_columns[!grepl("settlemen", admins_columns)]
            }
            # Update column choices
            column_choices[[paste0("column_questions_", j)]] <- columns
            column_choices[[paste0("column_dissagr_", j)]] <- dissagr_columns
            column_choices[[paste0("column_admins_", j)]] <- admins_columns
            
            column_values[[paste0("column_project_search_", j)]] <- input[[paste0("column_project_search_", j)]]
            
            # Update selectize inputs
            updateSelectizeInput(session, paste0("column_questions_", j), choices = c("", columns))
            updateSelectizeInput(session, paste0("column_dissagr_", j), choices = c("Overall", dissagr_columns))
            updateSelectizeInput(session, paste0("column_admins_", j), choices = c("Overall", admins_columns))
          }
        }, ignoreInit = TRUE)
      })
    })
  })
  
  # Render dynamic UI for project and question selection
  output$column_dynamic_fields <- renderUI({
    n <- column_field_count()
    
    if (n >= 2) {
      fields <- lapply(2:n, function(i) {
        fluidRow(
          column(3, selectizeInput(paste0("column_project_search_", i), "Select project:", 
                                   choices = c("", sort(unique(projects_data$TABLE_ID))), 
                                   multiple = FALSE, 
                                   selected = column_values[[paste0("column_project_search_", i)]])),
          column(3, selectizeInput(paste0("column_questions_", i), "Select a question:", 
                                   choices = c(column_choices[[paste0("column_questions_", i)]]), 
                                   multiple = FALSE, 
                                   selected = column_values[[paste0("column_questions_", i)]])),
          column(3, selectizeInput(paste0("column_dissagr_", i), "Select dissagr:", 
                                   choices = c("Overall", column_choices[[paste0("column_dissagr_", i)]]), 
                                   multiple = FALSE, 
                                   selected = column_values[[paste0("column_dissagr_", i)]])),
          column(3, selectizeInput(paste0("column_admins_", i), "Select admins:", 
                                   choices = c("Overall", column_choices[[paste0("column_admins_", i)]]), 
                                   multiple = TRUE, 
                                   selected = column_values[[paste0("column_admins_", i)]]))
        )
      })
      
      do.call(tagList, fields)
    }
  })
  
  # Process the form and generate outputs when the "column_process" button is clicked
  observeEvent(ignoreInit = TRUE, input$column_process,{
    # Check if the first question is filled
    if (is.null(input[["column_questions_1"]]) | input[["column_questions_1"]] == "") {
      return()
    }
    n <- column_field_count()
    res_text <- ""
    DAF <- data.frame()
    gen_info <- data.frame()
    # Loop through all dynamic fields to collect inputs and process data
    for(i in 1:n) {
        column <- input[[paste0("column_questions_", i)]]
        if (column == "") {
          next
        }
        table_name  <- columns_info %>%
          dplyr::filter(grepl(input[[paste0("column_project_search_", i)]], table_name) & ColumnName == column) %>%
          dplyr::select(table_name) %>%
          unique() %>%
          pull()
        
        func <- tool_survey %>%
          dplyr::filter(TABLE_ID == input[[paste0("column_project_search_", i)]] & name == column) %>%
          dplyr::select(`q.type`) %>%
          unique() %>%
          pull()
        
        if (length(func) == 0 | !(func %in% c("select_one", "select_multiple"))) {
          func <- "mean"
        }
        
        column_datasheet <- tool_survey %>%
          dplyr::filter(TABLE_ID == input[[paste0("column_project_search_", i)]] & name == column) %>%
          dplyr::select(datasheet) %>%
          pull()
        
        dissagr <- ifelse(input[[paste0("column_dissagr_", i)]] == "Overall", "empty", paste0(input[[paste0("column_dissagr_", i)]], collapse = ","))
        dissagr_label <- paste0(input[[paste0("column_dissagr_", i)]], collapse = ",")
        
        if (is.null(input[[paste0("column_admins_", i)]])) {
          admins <- c("Overall")
        } else {
          admins <- c("Overall", input[[paste0("column_admins_", i)]])
        }
        admins <- unique(admins)
        if (dissagr != "empty") {
          dissagr <- c(dissagr, "empty")
          dissagr_label <- c(dissagr_label, "Overall")
        }
        for (diss in dissagr) {
          for (admin in admins) {
            DAF <- rbind(DAF, data.frame(
              "TABLE_ID" = c(input[[paste0("column_project_search_", i)]]),
              "variable" = c(column),
              "variable_label" = c(column),
              "calculation" = c("empty"),
              "func" = func,
              "admin" = admin,
              "disaggregations" = diss,
              "disaggregations_label" = dissagr_label[which(dissagr == diss)],
              "join" = NA,
              "q.type" = c(func),
              "datasheet" = c(column_datasheet),
              "DB_table_name" = paste0("data_", table_name, "_DCMPR")
            ))
          }
        }
        
        main_datasheet <- projects_data %>%
          dplyr::filter(TABLE_ID == input[[paste0("column_project_search_", i)]]) %>%
          dplyr::select(main_datasheet) %>%
          pull()
        
        project_ID <- get.project.name(input[[paste0("column_project_search_", i)]])
        round_ID <- rev(unlist(stringr::str_split(input[[paste0("column_project_search_", i)]], "_")))[[2]]
        survey_type <- rev(unlist(stringr::str_split(input[[paste0("column_project_search_", i)]], "_")))[[1]]
        
        month_conducted <- time_tbl %>%
          dplyr::filter(TABLE_ID %in% input[[paste0("column_project_search_", i)]]) %>%
          dplyr::select(Interview_date) %>%
          pull()
        
        gen_info <- rbind(gen_info, data.frame(
          "TABLE_ID" = c(input[[paste0("column_project_search_", i)]]),
          "project_ID" = c(project_ID),
          "round_ID" = c(round_ID),
          "survey_type" = c(survey_type),
          "month_conducted" = c(month_conducted[[1]]),
          "main_sheet_name" = c(paste0("data_", input[[paste0("column_project_search_", i)]], "_", main_datasheet, "_DCMPR")),
          "weight_column_name" = c("empty")
        ))
    }
    DAF <- DAF %>% distinct()
    
    DAF$ID <- seq_len(nrow(DAF))
    # write.xlsx(DAF, "daf_column.xlsx")
    # write.xlsx(gen_info, "gen_info_column.xlsx")
    
    json_body <- list(
      daf_file = DAF,
      info = gen_info,
      filter = data.frame(
        TABLE_ID = as.character(),
        ID = as.character(),
        variable = as.character(),
        operation = as.character(),
        value = as.character()
      )
    )
    
    # write.xlsx(data.frame(
    #   TABLE_ID = as.character(),
    #   ID = as.character(),
    #   variable = as.character(),
    #   operation = as.character(),
    #   value = as.character()
    # ), "filter.xlsx")
    
    showModal(
      modalDialog(
        title = "Processing",
        "Sending your request to the processing system.",
        footer = NULL,
        easyClose = TRUE
      )
    )
    
    # Send data to Azure http_trigger to process analysis
    url <- Sys.getenv('url')
    response <- POST(url, body = json_body, encode = "json")
    # Process output
    char <- rawToChar(response$content)
    df <- fromJSON(char)
    df_final <- as.data.frame(do.call(cbind,df$result))

    # Convert the list to a dataframe
    df <- purrr::map_dfc(df_final, ~ purrr::map(.x, unlist_with_na) %>% unlist())

    df <- df %>%
      left_join(gen_info %>% select(TABLE_ID,month_conducted))
    
    removeModal()
    processed_data(df)
  })
  
  # Download the processed data as an Excel file
  output$column_excel <- downloadHandler(
    filename = function() {
      paste("data", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      
      excel_frame <- processed_data()
      # Create a workbook
      wb <- createWorkbook()
      
      # Add a worksheet
      addWorksheet(wb, "Data")
      
      # Write data to the worksheet
      writeData(wb, "Data", x = excel_frame, startCol = 1, startRow = 1, rowNames = FALSE)
      
      # Save the workbook
      saveWorkbook(wb, file)
    }
  )
}
