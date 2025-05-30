
oblasts <- read.xlsx('www/oblast_names.xlsx')
raions <- read.xlsx('www/raion_names.xlsx')
hromadas <- read.xlsx('www/hromada_names.xlsx')
settlements <- read.xlsx('www/settlement_names.xlsx')

# Step 1: Get TABLE_ID and main_datasheet for decompressed rows
table_info <- dbGetQuery(my_connection, "
  SELECT TABLE_ID, main_datasheet
  FROM data_representative_table
  WHERE status = 'decompressed'
")

# Step 2: Get all table names in the database
available_tables <- dbGetQuery(my_connection, "SELECT name FROM sys.tables")$name

# Step 3: Loop over each row to construct table name and query start date
results <- lapply(1:nrow(table_info), function(i) {
  tbl_id <- table_info$TABLE_ID[i]
  sheet  <- table_info$main_datasheet[i]
  
  table_name <- paste0("data_", tbl_id, "_", sheet, "_DCMPR")
  
  # Check if table exists in the database
  if (table_name %in% available_tables) {
    query <- sprintf("SELECT TOP 1 '%s' AS TABLE_ID, [start] FROM [%s]", tbl_id, table_name)
    res <- tryCatch(dbGetQuery(my_connection, query), error = function(e) NULL)
    return(res)
  } else {
    return(NULL)
  }
})

# Step 4: Combine and transform
time_tbl <- do.call(rbind, results)

if (!is.null(time_tbl) && "start" %in% colnames(time_tbl)) {
  time_tbl <- time_tbl %>%
    rename(start_date = "start") %>%
    mutate(
      value = substr(start_date, 1, 10),
      value = as.Date(value, format = "%Y-%m-%d"),
      Interview_date = format(value, "%Y-%m")
    )
} else {
  warning("No valid start dates found.")
  print(colnames(time_tbl))
}


database_project <- dbGetQuery(my_connection , "SELECT * from Reach_QDB;")
print(unique(database_project$project_ID))

unique_table <- database_project %>% 
  filter(paste0(project_ID,'_R',round_ID,'_',survey_type) %in% time_tbl$TABLE_ID) %>%
  select(database_label_clean,true_ID, project_ID) %>% 
  distinct()

unique_questions <- unique(unique_table$database_label_clean)

tool_survey <- dbGetQuery(my_connection , "SELECT * from Survey_DB;")

rep_table <-  dbGetQuery(my_connection , "SELECT * from representative_columns_table;")

rep_table$representative_at <- apply(rep_table[,c("oblast","raion","hromada","settlement")], 1, 
                 function(i) paste(colnames(rep_table[,c("oblast","raion","hromada","settlement")])[ !is.na(i) & i != "" ], collapse = ","))

rep_table <- rep_table %>% 
  select(TABLE_ID,oblast,representative_at) %>% 
  separate_rows(oblast, sep = ';') %>% 
  mutate(oblast = plyr::mapvalues(oblast,
                                  from = oblasts$admin1Pcode,
                                  to = oblasts$admin1Name_eng,
                                  warn_missing = F)) %>% 
  arrange(oblast) %>% 
  group_by(TABLE_ID,representative_at) %>% 
  summarise(oblast = paste0(oblast, collapse = ', ')) %>% 
  ungroup() %>% 
  mutate(oblast = ifelse(oblast=="NA",'none',oblast),
         representative_at = ifelse(representative_at=="",'none',representative_at))


############## load projects, rounds and survey types and geodata ##############

# create sf dataframe with 1 point in Kiyv and lable "No Data"

representation_data <- dbGetQuery(my_connection , "SELECT * FROM [representative_columns_table]") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(oblast = stringr::str_split(oblast, ";"),
                raion = stringr::str_split(raion, ";"),
                hromada = stringr::str_split(hromada, ";"),
                settlement = stringr::str_split(settlement, ";")) %>%
  dplyr::mutate(Project = get.project.name(TABLE_ID),
                Round = rev(unlist(stringr::str_split(TABLE_ID, "_")))[[2]],
                Type = rev(unlist(stringr::str_split(TABLE_ID, "_")))[[1]]) %>%
  ungroup()

research_cycles <- dbGetQuery(my_connection , "SELECT Research_cycle_ID, Name FROM [Research_cycle_db];")

representation_data <- representation_data %>%
  dplyr::left_join(research_cycles, by = c("Project" = "Research_cycle_ID")) %>%
  dplyr::left_join(time_tbl, by = c("TABLE_ID" = "TABLE_ID"))

oblast_json <- st_read("www/geodata/Oblasts.geojson")
raion_json <- st_read("www/geodata/Raions_simplified.geojson")
hromada_json <- st_cast(st_read("www/geodata/Hromadas.geojson"), "MULTIPOLYGON")
settlement_json <- st_cast(st_read("www/geodata/Settlements_simplified.geojson"), "MULTIPOLYGON")

hromada_json <- ms_simplify(hromada_json, keep = 0.2, keep_shapes = TRUE)
# settlement_json <- ms_simplify(settlement_json, keep = 0.08, keep_shapes = TRUE)

# write settlements to file
# st_write(settlement_json, "www/geodata/Settlements_simplified.geojson")

query <- "SELECT * FROM [dbo].[data_representative_table];"

projects_data <- dbGetQuery(my_connection, query)

# split TABLE_ID with '_", extract project_id, round and survey_type as first, second and third element

projects_data <- projects_data %>%
  dplyr::mutate(
    project_id = sapply(strsplit(TABLE_ID, "_"), "[[", 1),
    round = sapply(strsplit(TABLE_ID, "_"), "[[", 2),
    survey_type = sapply(strsplit(TABLE_ID, "_"), "[[", 3),
    TABLE_ID_main_sheet = paste(TABLE_ID, main_datasheet, sep = "_")
  )


columns_info <- dbGetQuery(my_connection, "
SELECT c.name AS ColumnName, t.name as table_name
FROM sys.columns c
JOIN sys.tables t ON c.object_id = t.object_id;") %>%
  dplyr::mutate(table_name = sapply(strsplit(table_name, "_"), function(x) paste(x[-c(1, length(x))], collapse = "_")))
