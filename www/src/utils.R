
get.project.name <- function(TABLE_ID) {
  TABLE_ID.splitted <- unlist(stringr::str_split(TABLE_ID, "_"))
  if (length(TABLE_ID.splitted) == 3) {
    return(TABLE_ID.splitted[[1]])
  } else {
    return(paste(TABLE_ID.splitted[1], TABLE_ID.splitted[2], sep = "_"))
  }
}

get.questions <- function(table_IDs) {
  
  tool_choices <- dbGetQuery(my_connection , 
                             paste0("SELECT * from Choices_DB where TABLE_ID IN ('",
                                    paste0(table_IDs, collapse = "', '"),"')")) %>%
    dplyr::select(-c("order")) %>%
    rename(choice_name = name,
           english_choices = `label::English` ,
           ukrainian_choices = `label::Ukrainian`,
           russian_choices = `label::Russian`
    )  %>% 
    group_by(TABLE_ID, list_name) %>%
    dplyr::filter(n() <= 20) %>% 
    summarise(across(everything(), ~ paste0(.x, collapse=',\n\n'))) %>% 
    ungroup()
  
  tool_survey <- dbGetQuery(my_connection , 
                            paste0("SELECT * from Survey_DB where TABLE_ID IN ('",
                                   paste0(table_IDs, collapse = "', '"),"')")) %>%
    dplyr::filter(!name %in% c("start", "end", "status", NA, "deviceid", "oblast",
                               "audit", "instance_name", "metadata", "date_survey",
                               "raion", "hromada", "settlement", "rectangles", "distance")) %>%
    dplyr::filter(q.type %in% c("select_one", "select_multiple", "integer", "decimal")) %>%
    dplyr::arrange(order) %>%
    dplyr::select(-order)
  
  questions.table <- tool_survey %>%
    dplyr::left_join(tool_choices, by=c("list_name" = "list_name", "TABLE_ID" = "TABLE_ID")) %>%
    dplyr::select(TABLE_ID, sector, q.type, `label::English`, `label::Ukrainian`, `english_choices`, `ukrainian_choices`) %>%
    dplyr::rename(english_question = `label::English`,
                  ukrainian_question = `label::Ukrainian`)
  
  return(questions.table)
}

# define func for daf
is.perc.func <- function(table_name, column_name) {
  query <- paste0(
    "SELECT COUNT(*) AS cannot_cast_to_float_count ",
    "FROM ", table_name, " ",
    "WHERE ", column_name, " IS NOT NULL ",
    "AND ", column_name, " != 'nan' ",
    "AND TRY_CAST(", column_name, " AS FLOAT) IS NULL;"
  )
  
  res <- dbGetQuery(my_connection, query)
  return(res$cannot_cast_to_float_count > 0)
}

# is.perc.func("data_UKR2203_R23_Customers_main_DCMPR", "b11_gas_heating_price")
