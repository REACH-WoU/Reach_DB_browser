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


server <- function(input, output, session) {
  # ----------------------------readme -------------------------------------------------
  
  # output$text_row1 <- renderText({})
  # 
  # 
  # output$text_column1 <- renderText({})
  # 
  # output$text_column2 <- renderText({})
  
  # ---------------------------the requests page --------------------------------
  
  
  updateSelectizeInput(
    session,
    inputId = "questions",
    label   = "Select an option:",
    choices = unique_questions,
    server  = TRUE )
  
  
  datasetInput <- reactiveVal(NULL)
  Dat <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  
  # # Reactive expression to fetch the selected project
  observeEvent(input$process,{
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
      select(project_ID,survey_type,round_ID,sector,TABLE_ID,q.type,list_name,datasheet,english_question,ukrainian_question,russian_question,representative_at,oblast,
             month_conducted,name)
    
    removeModal()
    
    datasetInput(
      cbind(df_choices_added, check = shinyInput(checkboxInput, nrow(df_choices_added), "checkb")
      ))
    
    Dat(cbind(df_choices_added, bool = FALSE))  
    
  })
  
  
  # Render the DataTable
  output$table <- renderDT({
    df <- datasetInput()
    if(is.null(df)){
      return(NULL)
    }else{
      tbl <- datatable(
        df,
        filter = "top",
        class = list(stripe = FALSE),
        escape = FALSE,
        editable = list(target = "cell", disable = list(columns = 16)),
        selection = "none",
        callback = JS(js),
        options = list(
          dom = 'lfrtipB',
          pageLength = 100,
          columnDefs = list(
            list(targets = "_all", width = '15px')
            ,
            list(visible=FALSE, targets=(which(names(df)%in% c('survey_type','round_ID',
                                                               'TABLE_ID','list_name','datasheet','name'))-1)
            )
          )),
        rownames = FALSE,
      )
      
      return(tbl)
      
    }
    
  }, server = FALSE)
  
  observeEvent(input$dtable_cell_edit, { 
    info <- input$dtable_cell_edit # this input contains the info of the edit
    
    dato <- Dat() # read the frame
    dato$bool <- as.character(dato$bool)# convert to character (no warnings this way)
    Dat(editData(dato, info))# update the reactive
    
    # if(any(Dat()$bool)){
    output$button2 <- renderUI({
      actionButton("process_request", "Send the request to the server")
    })
    # }
    
  })
  
  observeEvent(input$process_request,{
    

    output$table <- NULL
    selected_frame <- Dat()
    
    selected_frame <- selected_frame %>% filter(bool =='TRUE')
    
    rep_table_overview <-  dbGetQuery(my_connection , "SELECT TABLE_ID,datasheet_names,main_datasheet,representative_columns from data_representative_table")
    rep_table_overview_geo <- rep_table_overview %>% 
      mutate(representative_columns=ifelse(representative_columns=='None','Overall',representative_columns)) %>% 
      select(TABLE_ID,representative_columns)
    
    rep_table_overview_sheets_dict <- rep_table_overview %>% 
      separate_rows(datasheet_names ,sep =';') %>% 
      mutate(datasheet_name_tool = ifelse(datasheet_names==main_datasheet,'main',datasheet_names)) %>% 
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

    DAF_template<-DAF_template
    general_info<-general_info
    
    DAF_template[is.na(DAF_template$calculation),]$calculation <- 'empty'
    general_info[is.na(general_info$weight_column_name),]$weight_column_name <- 'empty'
    
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
      left_join(DAF_template %>% 
                  select(ID, TABLE_ID, variable, admin,disaggregations) %>% 
                  rename(variable_orig = variable,
                         admin_orig = admin,
                         disaggregations_orig = disaggregations)) %>% 
      left_join(general_info %>% select(TABLE_ID,month_conducted))
    
    
    removeModal()

    processed_data(df)
    
    projects <- unique(df$TABLE_ID)
    updateSelectizeInput(session, "project", choices = projects, selected = NULL)
    updateSelectizeInput(session, "project_numeric", choices = projects, selected = NULL)
    
    showNotification("The data has been processed. You can either download it as an excel or switch to the next page to view visuals",
                     type = 'message')
    
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
  
  observeEvent(input$project, {
    if (!is.null(input$project) & !is.null(processed_data())) {
      
      filtered_df <- processed_data() %>% filter(TABLE_ID == input$project)
      
      variables <- unique(filtered_df$variable_orig)
      
      updateSelectizeInput(session, "variable_orig", choices = variables)
    }
    
  })
  
  observeEvent(input$variable_orig, {
    if (!is.null(input$variable_orig) & !is.null(processed_data())) {
      
      filtered_df <- processed_data() %>% filter(TABLE_ID == input$project & variable_orig == input$variable_orig)
      
      options <- unique(filtered_df$option)
      
      updateSelectizeInput(session, "option", choices = options)
    }
    
  })
  
  observeEvent(input$option, {
    if (!is.null(input$option) & !is.null(processed_data())) {
      
      filtered_df <- processed_data() %>%
        filter(TABLE_ID == input$project & variable_orig == input$variable_orig & disaggregations_1 == " Overall" & option == input$option)

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
    
    ##### numeric
    
    observeEvent(input$project_numeric, {
      if (!is.null(input$project_numeric) & !is.null(processed_data())) {
        
        filtered_df <- processed_data() %>% filter(TABLE_ID == input$project_numeric)
        
        variables <- unique(filtered_df$variable_orig)
        
        updateSelectizeInput(session, "variable_orig_numeric", choices = variables)
      }
      
    })
    
    observeEvent(input$variable_orig_numeric, {
      if (!is.null(input$variable_orig_numeric) & !is.null(processed_data())) {
        
        filtered_df <- processed_data() %>% filter(TABLE_ID == input$project_numeric & variable_orig == input$variable_orig_numeric)
        
        options <- unique(filtered_df$option)
        
        updateSelectizeInput(session, "option_numeric", choices = options)
      }
      
    })
    
  })
  
  observeEvent(input$option_numeric, {
    if (!is.null(input$option_numeric) & !is.null(processed_data())) {
      
      filtered_df <- processed_data() %>%
        filter(TABLE_ID == input$project_numeric & variable_orig == input$variable_orig_numeric & disaggregations_1 == " Overall" & option == input$option_numeric)

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
          mapview(oblast_map_numeric, zcol = "perc", map.types = c("CartoDB.Positron",
                                                           "OpenStreetMap",
                                                           "Esri.WorldImagery",
                                                           "OpenTopoMap"))@map
        })
      }
      
      if (nrow(raion_map_numeric) > 0) {
        output$map_raion_numeric <- renderLeaflet({
          req(raion_map_numeric)
          mapview(raion_map_numeric, zcol = "perc", map.types = c("CartoDB.Positron",
                                                          "OpenStreetMap",
                                                          "Esri.WorldImagery",
                                                          "OpenTopoMap"))@map
        })
      }
      
      if (nrow(hromada_map_numeric) > 0) {
        output$map_hromada_numeric <- renderLeaflet({
          req(hromada_map_numeric)
          mapview(hromada_map_numeric, zcol = "perc", map.types = c("CartoDB.Positron",
                                                            "OpenStreetMap",
                                                            "Esri.WorldImagery",
                                                            "OpenTopoMap"))@map
        })
      }
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


