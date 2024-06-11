server <- function(input, output, session) {
  # ----------------------------readme -------------------------------------------------
  
  # output$text_row1 <- renderText({
  #   'This app allows the user to browse the questionnaire bank of the Reach Database. The user can select
  #   different projects to view or view the entirety of the database. The pages also allow for different 
  #   types of view of the matching tables, if that was desired.
  #   The tables that are produced by this programme are browseable. Each table has a filter pane on the top.
  #   The user can filter the output table by the question of interest, variable type and type of survey that
  #   was conducted among others. The user can  also download the resulting table as an Excel file if they
  #   click the `Download Excel button`
  #   <br>
  #   <br>'
  # })
  # 
  # 
  # output$text_column1 <- renderText({
  #   '<strong>Single comparison.</strong><br>
  #   This page allows the user to select the project of interest and browse the database for
  #   all of the questions that match the questions of the selected project. For example, selecting UKR2308
  #   will show the user all of the questions that match between UKR2308 and the rest of our questionnaires.
  #   The user can select multiple projects or leave the field blank to view the entire database.
  #   <br>
  #   <strong>Multiple comparison.</strong><br>
  #   To use this capability of the browser the user has to select 2 or more projects in the dropdown menu.
  #   Selecting this type of view will build a wide table for the matching questions of the selected projects.
  #   Each row will represent a question with it\'s details (question type, sector, survey_type, etc.) with
  #   columns being grouped by the projects where the question in the row has a match.
  #   '
  # })
  # 
  # # Render text in the second column
  # output$text_column2 <- renderText({
  #   '<strong>General information.</strong><br>
  #   This page allows the user to view all of the matching questions that match across multiple rounds
  #   within a project. So, if the question `a` was asked in rounds 1,3,10 of the project, the user will
  #   be able to see this. If there are multiple types of surveying within the project, the user will have to
  #   specify exactly what kind of survey they\'d like to browse (e.g. Household survey vs Key informant survey
  #   under the same project ID).The matching questions are color coded for easy browsing.
  #   '
  # })
  
  # ---------------------------the requests page --------------------------------
  
  
  updateSelectizeInput(
    session,
    inputId = "questions",
    label   = "Select an option:",
    choices = unique_questions,
    server  = TRUE )
  
  
  datasetInput <- reactiveVal(NULL)
  
  
  # # Reactive expression to fetch the selected project
  observeEvent(input$process,{
    req(input$questions)
    
    showModal(
      modalDialog(
        title = "Processing",
        "Fetching your request from the DB.",
        footer = NULL,
        easyClose = TRUE
      )
    )
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
      select(project_ID,sector,TABLE_ID,english_question,ukrainian_question,russian_question,representative_at,oblast,
             month_conducted)
    
    removeModal()
    datasetInput(df_choices_added)
    
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
        options = list(
          dom = 'lfrtipB',
          pageLength = 100,
          columnDefs = list(
            list(targets = "_all", width = '15px',
                 targets = "oblast", width = '30px')  # Adjust the width as needed
          )),
        rownames = FALSE,
      )
      
      
      
      return(tbl)
      
      
      
    }
    
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
