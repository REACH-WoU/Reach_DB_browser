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

    
  })
  
  # output$excel <- downloadHandler(
  #   filename = function() {
  #     paste("data", Sys.Date(), ".xlsx", sep = "")
  #   },
  #   content = function(file) {
  #     excel_frame <-datasetInput()
  #     # Create a workbook
  #     wb <- createWorkbook()
  #     
  #     # Add a worksheet
  #     addWorksheet(wb, "Data")
  #     
  #     if(input$table_type == 'Multiple comparison'){
  #       old_names <- names(excel_frame)
  #       new_row <- gsub("\\_set_.*","",old_names)
  #       excel_frame <- rbind(new_row,excel_frame)
  #       
  #       new_names <- gsub(".*\\_set_","",old_names)
  #       names(excel_frame) <- new_names
  #     }else{
  #       excel_frame <- excel_frame %>% select(-any_of(c('true_ID')))
  #     }
  #     
  #     # Write data to the worksheet
  #     writeData(wb, "Data", x = excel_frame, startCol = 1, startRow = 1, rowNames = FALSE)
  #     
  #     # create a new style for this thing
  #     custom_style <- createStyle(wrapText =TRUE)
  #     grey_rows <- createStyle(fgFill = 'lightgrey',wrapText =TRUE,border ='TopBottom')
  #     white_rows <- createStyle(fgFill = 'white',wrapText =TRUE,border ='TopBottom')
  #     
  #     
  #     addStyle(wb, sheet = "Data", custom_style, rows = 1:(nrow(excel_frame)+1), 
  #              cols = 1:ncol(excel_frame), gridExpand = TRUE)
  #     
  #     if(input$table_type == 'Single comparison'){
  #       true_id_list <- unique(excel_frame$true_ID)
  #       true_id_even <- true_id_list[seq(1, length(true_id_list), 2)]
  #       rows_to_format <- which(excel_frame$true_ID %in% true_id_even)+1
  #       
  #       addStyle(wb, sheet = "Data", grey_rows, rows = rows_to_format, 
  #                cols = 1:ncol(excel_frame), gridExpand = TRUE)
  #     }else{
  #       ind_beginnings <- which(grepl('sector',excel_frame[1,]))
  #       ind_endings <- which(grepl('russian_choices',excel_frame[1,]))
  #       
  #       ind_beginnings <- ind_beginnings[seq(1,length(ind_beginnings),2)]
  #       ind_endings <- ind_endings[seq(1,length(ind_endings),2)]
  #       
  #       columns_to_format <- lapply(1:length(ind_beginnings),function(x){ind_beginnings[x]:ind_endings[x]}) %>% 
  #         unlist()
  #       
  #       columns_to_format2 <- setdiff(1:ncol(excel_frame),columns_to_format)
  #       
  #       addStyle(wb, sheet = "Data", grey_rows, rows = 1:(nrow(excel_frame)+1), 
  #                cols = columns_to_format, gridExpand = TRUE)
  #       
  #       addStyle(wb, sheet = "Data", white_rows, rows = 1:(nrow(excel_frame)+1), 
  #                cols = columns_to_format2, gridExpand = TRUE)
  #       
  #       # merge the cells with identical TABLE_ID
  #       ls_names <- unique(names(excel_frame))
  #       
  #       centered_style_grey <- createStyle(halign = "center", valign = "center",fgFill = 'lightgrey')
  #       centered_style_white <- createStyle(halign = "center", valign = "center",fgFill = 'white')
  #       
  #       for(name in ls_names){
  #         range <- which(names(excel_frame) %in% name)
  #         mergeCells(wb, "Data", cols = range, rows = 1)
  #         if(all(range %in% columns_to_format)){
  #           addStyle(wb, sheet = "Data", style = centered_style_grey,
  #                    cols = range, rows = 1, gridExpand = TRUE)
  #         }else{
  #           addStyle(wb, sheet = "Data", style = centered_style_white,
  #                    cols = range, rows = 1, gridExpand = TRUE)
  #         }
  #       }
  #     }
  #     
  #     border_style <- createStyle(border ='Left')
  #     
  #     addStyle(wb, sheet = "Data", border_style, rows = 1:(nrow(excel_frame)+1), 
  #              cols = ncol(excel_frame)+1, gridExpand = TRUE)
  #     
  #     setColWidths(wb,'Data',1:ncol(excel_frame),widths =30)
  #     
  #     # Write merged data to the worksheet
  #     writeData(wb, "Data", excel_frame,
  #               startCol = 1, startRow = 1, rowNames = FALSE)
  #     
  #     # Save the workbook
  #     saveWorkbook(wb, file)
  #   }
  # )
  
  
  
  
  
}
