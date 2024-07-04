unlist_with_na <- function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(unlist(x))
  }
}

js <- c(
  "$('[id^=checkb]').on('click', function(){",
  "  var id = this.getAttribute('id');",
  "  var i = parseInt(/checkb(\\d+)/.exec(id)[1]);",
  "  var value = $(this).prop('checked');",
  "  var info = [{row: i, col: 16, value: value}];",
  "  Shiny.setInputValue('dtable_cell_edit:DT.cellInfo', info);",
  "})"
)


shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
  }
  inputs
}

source('www/src/get_db.R')
source('www/src/load_data.R')

server <- function(input, output, session) {
  
  
  # ----------------------------readme -------------------------------------------------
  
  # output$text_row1 <- renderText({})
  # 
  # 
  # output$text_column1 <- renderText({})
  # 
  # output$text_column2 <- renderText({})
  
  # ---------------------------the requests page --------------------------------
  
  updateSelectInput(session, "project_search", choices = c("Overall", unique(unique_table$project_ID)))
  
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
  
  
  
  
  datasetInput <- reactiveVal(NULL)
  Dat <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  numeric_data <- reactiveVal(NULL)
  select_data <- reactiveVal(NULL)
  
  # # Reactive expression to fetch the selected project
  observeEvent(ignoreInit = TRUE, input$process,{
    req(input$questions)
    
    questions<- input$questions
    
    get_true_IDs <- unique_table %>% 
      filter(database_label_clean %in% questions) %>% 
      pull(true_ID)
    
    
    needed_data <- database_project %>% 
      filter(true_ID %in% get_true_IDs) %>% 
      mutate(TABLE_ID = paste0(project_ID,'_R',round_ID,'_',survey_type)) %>% 
      inner_join(time_tbl) %>% 
      rename(month_conducted = value)
    
    
    df_choices_added <- needed_data %>%
      #   left_join(tool_choices %>% 
      #               select(-c(order,load_time)) %>% 
      #               rename(choice_name =name,
      #                      english_choices = `Label::English` ,
      #                      ukrainian_choices = `Label::Ukrainian`,
      #                      russian_choices = `Label::Russian`
      #               ) %>% 
      #               group_by(TABLE_ID,list_name) %>% 
      #               summarise(across(everything(), ~ paste0(.x, collapse=',\n'))) %>% 
      #               ungroup() %>% 
      #               mutate(list_name = str_squish(list_name))
      #   ) %>% 
      left_join(tool_survey %>%
                  select(name, contains('Label'), TABLE_ID) %>%
                  rename(
                    english_question = `Label::English` ,
                    ukrainian_question = `Label::Ukrainian`,
                    russian_question = `Label::Russian`
                  ) %>%
                  mutate(name = str_squish(name))) %>% 
      left_join(rep_table) %>% 
      select(project_ID,survey_type,round_ID,sector,TABLE_ID,q.type,list_name,datasheet,
             english_question,ukrainian_question,russian_question,representative_at,oblast,
             month_conducted,name)
    
    removeModal()
    
    datasetInput(
      cbind(df_choices_added, check = shinyInput(checkboxInput, nrow(df_choices_added), "checkb")
      ))
    
    Dat(cbind(df_choices_added, bool = FALSE)) 
    
    output$table <- renderDT({
      df <- datasetInput()
      df <- df %>% 
        mutate(representative_at = gsub('\\,',', ',representative_at))
      
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
    
    # output$table <- renderDT({
    #   return(NULL)
    # })
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
    # DAF_template<<-DAF_template
    # general_info<<-general_info
    
    # save DAF_template and general_info as xlsx
    
    DAF_template[is.na(DAF_template$calculation),]$calculation <- 'empty'
    DAF_template[is.na(DAF_template$disaggregations),]$disaggregations <- 'empty'
    
    if(nrow(general_info[is.na(general_info$weight_column_name),])>0){
      general_info[is.na(general_info$weight_column_name),]$weight_column_name <- 'empty'
    }
    
    # write.xlsx(DAF_template,'DAF_template.xlsx')
    # write.xlsx(general_info,'general_info.xlsx')
    
    json_body <- list(
      daf_file = DAF_template,
      info = general_info
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
    
  })
  
  # if the user has excel input let them use it here
  observeEvent(input$file, {
    excel_input <- readxl::read_xlsx(input$file$datapath)
    
    # check that excel file is valid
    columns.set <- c("ID", "admin", "admin_category", "option", "variable",
                     "disaggregations_category_1", "disaggregations_1",
                     "weighted_count", "unweighted_count", "perc", "general_count",
                     "full_count", "total_count_perc", "option_orig",
                     "disaggregations_category_1_orig", "admin_category_orig",
                     "variable_orig", "disaggregations_1_orig", "TABLE_ID", "mean",
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
    write.xlsx(processed_data(),'excel_input.xlsx')
    if(!is.null(processed_data())){
      
      df <- processed_data()

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
      print(unique(numeric$variable_orig))
      
      updateSelectizeInput(session, "project", choices = projects_select)
      updateSelectizeInput(session, "project_numeric", choices = projects_numeric)
      
      showNotification("The data has been processed. You can either download it as an excel or switch to the next page to view visuals",
                       type = 'message')
      
    }
  })
  
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
  
  
  
  ################################ Geoview part ################################
  
  observeEvent(ignoreInit = TRUE,input$project, {
    if (!is.null(input$project) & nrow(select_data())>0) {
      
      filtered_df <- select_data() %>% filter(TABLE_ID == input$project)
      
      variables <- unique(filtered_df$variable_orig)
      
      updateSelectizeInput(session, "variable_orig", choices = variables)
      
      
      variables_m <- unique(select_data()$variable_orig)
      
      updateSelectizeInput(session, "variable_orig_m", choices = variables_m, selected = variables_m[1])
      
    }
    
  })
  
  observeEvent(ignoreInit = TRUE,list(
    input$variable_orig,
    input$project
  ), {
    if (!is.null(input$variable_orig)& !is.null(input$project) & nrow(select_data())>0) {
      
      data_processed <- select_data()
      
      
      filtered_df <- data_processed %>%
        filter(TABLE_ID %in% input$project & variable_orig %in% input$variable_orig,
               perc>0)

      overall_admin_data <- filtered_df %>%
        filter(admin == "Overall" & disaggregations_category_1 %in% c('Overall',' Overall')) %>% 
        rowwise() %>% 
        mutate(option = paste(strwrap(option, width = 35), collapse = "<br>")) %>% 
        ungroup()
      

      
      options <- unique(filtered_df$option)
      
      updateSelectizeInput(session, "option", choices = options)
      
      
      # basic chart
      
      total_cnt <- round(sum(overall_admin_data$perc)*100,0)
      title <- unique(overall_admin_data$variable)
      
      
      output$graph_1_select <- renderPlotly({
        if(total_cnt==100){
          graph_1 <- plot_ly(overall_admin_data, labels = ~option, values = ~weighted_count, type = 'pie', hole = 0.6) %>%
            layout(title = list(text = paste0("<b>",title ,"</b>"),
                                font = list(color = '#000080', size = 12)),
                   showlegend = TRUE)
          
        }else{
          graph_1 <- plot_ly(overall_admin_data, x = ~perc*100, y = ~option,  type = 'bar',
                             text = ~paste0(round(perc*100,1),'%'),
                             textposition = 'outside') %>%
            layout(title = list(text=paste0("<b>",title ,"</b>"),
                                font = list(color = '#000080', size = 12)),
                   xaxis = list(ticksuffix = "%",title = "", range = c(0,110)),
                   yaxis = list(title = ""))
        }
        return(graph_1)
      })
      
      
      
      
      # disaggregation graph
      overall_disaggregation_data <- filtered_df %>%
        filter(admin == "Overall") %>% 
        rowwise() %>% 
        mutate(option = paste(strwrap(option, width = 35), collapse = "<br>")) %>% 
        ungroup()
      
      check_categories <- setdiff(unique(overall_disaggregation_data$disaggregations_category_1), c(' Overall','Overall'))
      
      # check if we even have gender data for this
      if(length(check_categories)>0){
        overall_disaggregation_data <- overall_disaggregation_data %>% 
          filter(!disaggregations_category_1 %in% c(' Overall','Overall'))
      }
      
      title <- paste0(unique(overall_disaggregation_data$variable),'<br>')
      # cut off the title if it is too long
      title <- substr(title, 1, 50)
      title <- paste0(title, '...')
      title <- paste0(title,'<br>')
      
      output$graph_2_select <- renderPlotly({
        plot_ly(overall_disaggregation_data, x = ~perc*100, y = ~option, color = ~disaggregations_category_1, type = 'bar',
                text = ~paste0(round(perc*100,1),'%'),
                textposition = 'auto',
                insidetextanchor = 'start',
                hoverinfo = 'text',
                marker = list(line = list(width = 1, color = "black"))) %>%
          layout(title = list(text = paste0("<b>", title , "</b>"),
                              font = list(color = '#000080', size = 12)),
                 xaxis = list(ticksuffix = "%", title = "", range = c(0,110),
                              automargin = TRUE),
                 yaxis = list(title = "", automargin = TRUE),
                 margin = list(l = 100, r = 20, t = 50, b = 50),
                 height = 400,
                 showlegend = TRUE)
      })
      
    }
    
    
  })
  
  
  observeEvent(ignoreInit = TRUE,input$variable_orig_m,{
    if(!is.null(input$variable_orig_m) & nrow(select_data())>0){
      
      data_processed <- select_data()
      
      # timeline graph
      graph_base3 <- data_processed %>% 
        filter(variable_orig %in% input$variable_orig_m,
               disaggregations_category_1 %in% c(' Overall','Overall'),
               admin %in% 'Overall',
               perc>0) %>% 
        rowwise() %>% 
        mutate(variable = paste(strwrap(variable, width = 35), collapse = "<br>"),
               period_full = paste(month_conducted, TABLE_ID, variable,sep = '<br>'),
               option = paste(strwrap(option, width = 35), collapse = "<br>"))
      
      cnts <- graph_base3 %>% group_by(variable,TABLE_ID) %>% summarise(perc=sum(perc)) %>% pull(perc)
      cnts <- round(cnts*100,0)
      title <- paste0(graph_base3$variable, '<br>')
      # cut off the title if it is too long
      title <- substr(title, 1, 50)
      title <- paste0(title, '...')
      title <- paste0(title,'<br>')
      
      
      output$graph_3_select <- renderPlotly({
        if(all(cnts==100)){
          
          plot_ly(graph_base3, x = ~period_full, y = ~perc*100 , color = ~option, type = 'bar',
                  text = ~paste0(round(perc*100,1),'%'),
                  textposition = 'outside',
                  textfont =  list(size = 12,color = 'black')) %>%
            layout(barmode = 'stack', 
                   title = list(text=paste0("<b>",title ,"</b>"),
                                font = list(color = '#000080', size = 12)),
                   legend = list(x = 0, y = -0.2),
                   xaxis = list(title = ""),
                   yaxis = list(ticksuffix = "%",title = ""))
        }else{
          plot_ly(graph_base3, x = ~period_full, y = ~perc*100 , color = ~option, type = 'bar',
                  text = ~paste0(round(perc*100,1),'%'),
                  textposition = 'outside',
                  textfont =  list(size = 12,color = 'black')) %>%
            layout(title = list(text=paste0("<b>",title ,"</b>"),
                                font = list(color = '#000080', size = 12)),
                   xaxis = list(title = ""),
                   yaxis = list(ticksuffix = "%",title = ""))
          
        }
      }) 
      
    }
  })
  
  
  observeEvent(ignoreInit = TRUE,input$option, {
    if (!is.null(input$option) & nrow(select_data())>0) {
      
      filtered_df <- select_data() %>%
        filter(TABLE_ID == input$project & variable_orig == input$variable_orig & disaggregations_category_1 %in% c('Overall',' Overall') & option == input$option)
      
      #### oblast plot
      
      oblast_map <- filtered_df %>%
        inner_join(oblast_json, by = c("admin_category_orig" = "ADM1_PCODE"))
      
      oblast_map <- st_as_sf(oblast_map)
      
      #### raion plot
      raion_map <- filtered_df %>%
        inner_join(raion_json, by = c("admin_category_orig" = "ADM2_PCODE"))
      
      raion_map <- st_as_sf(raion_map)
      
      #### hromada plot
      hromada_map <- filtered_df %>%
        inner_join(hromada_json, by = c("admin_category_orig" = "ADM3_PCODE"))
      
      hromada_map <- st_as_sf(hromada_map)
      
      if (nrow(oblast_map) > 0) {
        output$map_oblast <- renderLeaflet({
          req(oblast_map)
          mapview(oblast_map, zcol = "perc", map.types = c("CartoDB.Positron",
                                                           "OpenStreetMap",
                                                           "Esri.WorldImagery",
                                                           "OpenTopoMap"))@map
        })
      }
      
      if (nrow(raion_map) > 0) {
        output$map_raion <- renderLeaflet({
          req(raion_map)
          mapview(raion_map, zcol = "perc", map.types = c("CartoDB.Positron",
                                                          "OpenStreetMap",
                                                          "Esri.WorldImagery",
                                                          "OpenTopoMap"))@map
        })
      }
      
      if (nrow(hromada_map) > 0) {
        output$map_hromada <- renderLeaflet({
          req(hromada_map)
          mapview(hromada_map, zcol = "perc", map.types = c("CartoDB.Positron",
                                                            "OpenStreetMap",
                                                            "Esri.WorldImagery",
                                                            "OpenTopoMap"))@map
        })
      }
      
    }
    
  })
  ##### numeric
  
  observeEvent(ignoreInit = TRUE, input$project_numeric, {
    if (!is.null(input$project_numeric) & nrow(numeric_data()) > 0) {
      
      filtered_df <- numeric_data() %>% filter(TABLE_ID == input$project_numeric)
      
      variables <- unique(filtered_df$variable_orig)
      
      updateSelectizeInput(session, "variable_orig_numeric", choices = variables)
      
      variables_m <- unique(numeric_data()$variable_orig)
      
      updateSelectizeInput(session, "variable_orig_m_numeric", choices = variables_m, selected = variables_m[1])
      
    }
    
  })
  
  observeEvent(ignoreInit = TRUE, input$variable_orig_numeric, {
    if (!is.null(input$variable_orig_numeric) & nrow(numeric_data()) > 0) {
      
      filtered_df <- numeric_data() %>% filter(TABLE_ID == input$project_numeric & variable_orig == input$variable_orig_numeric)
      
      options <- c("mean", "min", "max", "median")
      
      updateSelectizeInput(session, "option_numeric", choices = options)
    }
    
  })
  
  
  observeEvent(ignoreInit = TRUE, list(
    input$variable_orig_numeric,
    input$option_numeric,
    input$project_numeric), {
      if (!is.null(input$option_numeric) & nrow(numeric_data())> 0 & input$option_numeric != '' &
          !is.null(input$project_numeric)) {
        
        processed_numerics <- numeric_data()
        
        filtered_df <- processed_numerics %>%
          filter(TABLE_ID == input$project_numeric & variable_orig == input$variable_orig_numeric & 
                   disaggregations_category_1 %in% c(" Overall",'Overall'))
        
        # basic stats
        basic_stat_number <- filtered_df %>% 
          filter(admin_category%in%c(' Overall','Overall')) %>% 
          pull(!!sym(input$option_numeric))
        
        
        # output$numeric_text_1 <- renderText({paste0(
        #   unique(filtered_df$variable),
        #   '<br>',input$option_numeric,' of ',round(basic_stat_number,2)
        # )
        # })
        
        # disaggregation graph
        graph_base_n2 <- processed_numerics %>% 
          filter(variable_orig %in% input$variable_orig_numeric,
                 admin %in% 'Overall',
                 TABLE_ID %in% input$project_numeric) %>% 
          rowwise() %>% 
          mutate(variable = paste(strwrap(variable, width = 35), collapse = "<br>"),
                 vis_variable  = !!sym(input$option_numeric))
        
        title <- paste0(unique(graph_base_n2$variable),'<br>')
        # cut off the title if it is too long
        title <- substr(title, 1, 50)
        title <- paste0(title, '...')
        title <- paste0(title,'<br>')
        
        check_categories <- setdiff(unique(graph_base_n2$disaggregations_category_1), c(' Overall','Overall'))
        
        if(length(check_categories)>0){
          graph_base_n2 <- graph_base_n2 %>% 
            filter(!disaggregations_category_1 %in% c(' Overall','Overall'))
        }
        
        graph_base_n2 <- graph_base_n2 %>%
          pivot_longer(cols = c('mean', 'min', 'max', 'median'), names_to = 'statistic', values_to = 'value')
        
        # output$graph_1_numeric <- renderPlotly({
        #   plot_ly(graph_base_n2, x = ~variable, y = ~vis_variable, color = ~disaggregations_category_1, type = 'bar',
        #           text = ~round(vis_variable,1),
        #           textposition = 'outside') %>%
        #     layout(title = list(text=paste0("<b>",title ,"</b>"),
        #                         font = list(color = '#000080', size = 12)),
        #            xaxis = list(title = ""),
        #            yaxis = list(title = ""))
        # })
        
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
                text = "Summary Statistics by Gender",
                font = list(size = 24)
              ),
              xaxis = list(
                title = "Statistic",
                titlefont = list(size = 18),
                tickfont = list(size = 14)
              ),
              yaxis = list(
                title = "Value",
                titlefont = list(size = 18),
                tickfont = list(size = 14)
              ),
              margin = list(t = 50, b = 50),
              paper_bgcolor = 'rgba(245, 246, 249, 1)',
              plot_bgcolor = 'rgba(245, 246, 249, 1)',
              hovermode = 'closest'
            )
        })
        
        
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
        
        if (nrow(oblast_map_numeric) > 0) {
          output$map_oblast_numeric <- renderLeaflet({
            req(oblast_map_numeric)
            mapview(oblast_map_numeric, zcol = input$option_numeric, map.types = c("CartoDB.Positron",
                                                                                   "OpenStreetMap",
                                                                                   "Esri.WorldImagery",
                                                                                   "OpenTopoMap"))@map
          })
        }
        
        if (nrow(raion_map_numeric) > 0) {
          output$map_raion_numeric <- renderLeaflet({
            req(raion_map_numeric)
            mapview(raion_map_numeric, zcol = input$option_numeric, map.types = c("CartoDB.Positron",
                                                                                  "OpenStreetMap",
                                                                                  "Esri.WorldImagery",
                                                                                  "OpenTopoMap"))@map
          })
        }
        
        if (nrow(hromada_map_numeric) > 0) {
          output$map_hromada_numeric <- renderLeaflet({
            req(hromada_map_numeric)
            mapview(hromada_map_numeric, zcol = input$option_numeric, map.types = c("CartoDB.Positron",
                                                                                    "OpenStreetMap",
                                                                                    "Esri.WorldImagery",
                                                                                    "OpenTopoMap"))@map
          })
        }
      }
      
    })
  
  
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
          mutate(variable =paste(strwrap(variable, width = 35), collapse = "<br>"),
                 period_full = paste(month_conducted,TABLE_ID,variable,sep = '<br>'),
                 viz_variable = !!sym(input$option_numeric))
        
        title <- paste0(unique(graph_base3$variable),'<br>')
        # cut off the title if it is too long
        title <- substr(title, 1, 50)
        title <- paste0(title, '...')
        title <- paste0(title,'<br>')
        
        
        # output$graph_2_numeric <- renderPlotly({
        #   plot_ly(graph_base3, x = ~period_full, y = ~viz_variable , type = 'bar',
        #           text = ~round(viz_variable,1),
        #           textposition = 'outside',
        #           textfont =  list(size = 12,color = 'black')) %>%
        #     layout(title = list(text=paste0("<b>",title ,"</b>"),
        #                         font = list(color = '#000080', size = 1)),
        #            legend = list(x = 0, y = -0.2),
        #            xaxis = list(title = ""),
        #            yaxis = list(title = ""))
        #   
        # })
        
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
            colors = c('mean' = 'green', 'max' = 'red', 
                       'median' = 'orange', 'min' = 'blue'),
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
                title = "Variable",
                titlefont = list(size = 18),
                tickfont = list(size = 14)
              ),
              yaxis = list(
                title = "Value",
                titlefont = list(size = 18),
                tickfont = list(size = 14)
              ),
              margin = list(t = 50, b = 100),
              hovermode = 'closest'
            )
        })
        
      }
    })
  
  
  # observeEvent(input$project_id, {
  #   project_rounds <- projects_data %>% 
  #     filter(project_id == input$project_id) %>% 
  #     pull(round)
  #   
  #   updateSelectizeInput(session, "round", choices = project_rounds)
  # })
  # 
  # observeEvent(input$round, {
  #   project_types <- projects_data %>% 
  #     filter(project_id == input$project_id,
  #            round == input$round) %>% 
  #     pull(survey_type)
  #   
  #   updateSelectizeInput(session, "survey_type", choices = project_types)
  # })
  # 
  # observeEvent(input$check_representation_levels, {
  # 
  #   if (!is.null(input$project_id) & !is.null(input$round) & !is.null(input$survey_type)) {
  #     
  #     updateSelectizeInput(session, "representation_level", choices = c())
  #     
  #     representation_levels <- projects_data %>% 
  #       filter(project_id == input$project_id,
  #              round == input$round,
  #              survey_type == input$survey_type) %>% 
  #       dplyr::select(oblast, raion, hromada, settlement)
  #     
  #     if (nrow(representation_levels) > 1) {
  #       1
  #     }
  #     representative_choices <- c()
  #     if (!is.na(representation_levels$oblast[1]) & representation_levels$oblast[1] != "") {
  #       representative_choices <- c(representative_choices, "oblast")
  #     }
  #     
  #     if (!is.na(representation_levels$raion[1]) & representation_levels$raion[1] != "") {
  #       representative_choices <- c(representative_choices, "raion")
  #     }
  #     
  #     if (!is.na(representation_levels$hromada[1]) & representation_levels$hromada[1] != "") {
  #       representative_choices <- c(representative_choices, "hromada")
  #     }
  #     
  #     if (!is.na(representation_levels$settlement[1]) & representation_levels$settlement[1] != "") {
  #       representative_choices <- c(representative_choices, "settlement")
  #     }
  #     
  #     if (length(representative_choices) == 0) {
  #       updateSelectizeInput(session, "representation_level", choices = c("No representative levels"))
  #     } else {
  #       updateSelectizeInput(session, "representation_level", choices = representative_choices)
  #     }
  #   }
  # 
  # })
  # 
  # observeEvent(input$draw_map, {
  #   if (!is.null(input$representation_level)) {
  #     
  #     if (input$representation_level == "No representative levels") {
  #       output$map <- renderLeaflet({
  #         leaflet() %>%
  #           addProviderTiles(providers$CartoDB.Positron) %>%
  #           setView(lng = 31.1656, lat = 48.3794, zoom = 6) %>%
  #           addLabelOnlyMarkers(
  #             lng = 31.1656, lat = 48.3794, 
  #             label = "NO DATA", 
  #             labelOptions = labelOptions(
  #               noHide = TRUE, 
  #               direction = 'top', 
  #               textOnly = TRUE,
  #               style = list(
  #                 "color" = "red", 
  #                 "font-size" = "16px",
  #                 "font-weight" = "bold"
  #               )
  #             )
  #           )
  #       })
  #       return()
  #     }
  #     
  #     geodata <- projects_data %>% 
  #       dplyr::filter(project_id == input$project_id,
  #              round == input$round,
  #              survey_type == input$survey_type) %>%
  #       pull(!!sym(input$representation_level)) %>%
  #       strsplit(";") %>%
  #       unlist()
  #     
  #     if (input$representation_level == "oblast") {
  # map_data <- oblast_json %>%
  #   dplyr::filter(ADM1_PCODE %in% geodata)
  #     } else if (input$representation_level == "raion") {
  #       map_data <- raion_json %>%
  #         filter(ADM2_PCODE %in% geodata)
  #     } else if (input$representation_level == "hromada") {
  #       map_data <- hromada_json %>%
  #         filter(ADM3_PCODE %in% geodata)
  #     } else if (input$representation_level == "settlement") {
  #       map_data <- settlement_json %>%
  #         filter(ADM4_PCODE %in% geodata)
  #     }
  #     
  # output$map <- renderLeaflet({
  #   req(map_data)
  #   mapview(map_data)@map
  # })
  #     
  #   }
  # })
  # 
  # observeEvent(input$addMaps, {
  #   output$mapsUI <- renderUI({
  #     numMaps <- input$numMaps
  #     map_outputs <- lapply(1:numMaps, function(i) {
  #       leafletOutput(paste0("map", i))
  #     })
  #     do.call(tagList, map_outputs)
  #   })
  #   
  #   lapply(1:input$numMaps, function(i) {
  #     output[[paste0("map", i)]] <- renderLeaflet({
  #       leaflet() %>%
  #         addTiles() %>%
  #         setView(lng = -93.65, lat = 42.0285, zoom = 4)
  #     })
  #   })
  # })
  
}


