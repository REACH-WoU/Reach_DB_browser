
oblasts <- read.xlsx('www/oblast_names.xlsx')

time_tbl <- dbGetQuery(my_connection , "
DECLARE @sql NVARCHAR(MAX);
-- Construct the dynamic SQL
SET @sql = STUFF(
    (
        SELECT ' UNION ALL SELECT top 1 ''' + TABLE_ID + ''' as TABLE_ID, [start] AS value FROM data_' + TABLE_ID+'_'+main_datasheet+'_DCMPR' 
        FROM data_representative_table
		where status = 'decompressed' 
		AND 'data_' + TABLE_ID + '_' + main_datasheet + '_DCMPR' IN (SELECT name FROM sys.tables)
        FOR XML PATH(''), TYPE
    ).value('.', 'NVARCHAR(MAX)')
, 1, 11, '');

exec sp_executesql @sql
") %>% 
  mutate(value = substr(value,1,10),
         value = as.Date(value,format="%Y-%m-%d"),
         value = format(value, "%Y-%m"))


database_project <- dbGetQuery(my_connection , "SELECT * from Reach_QDB;")
print(unique(database_project$project_ID))

unique_table <- database_project %>% 
  filter(paste0(project_ID,'_R',round_ID,'_',survey_type) %in% time_tbl$TABLE_ID) %>% 
  select(database_label_clean,true_ID) %>% 
  distinct()

unique_questions <- unique(unique_table$database_label_clean)

tool_survey <- dbGetQuery(my_connection , "SELECT * from Survey_DB;")

rep_table <-  dbGetQuery(my_connection , "SELECT * from representative_columns_table;")

rep_table$representative_at <- apply(rep_table[,c("oblast","raion","hromada","settlement")], 1, 
                 function(i) paste(colnames(rep_table[,c("oblast","raion","hromada","settlement")])[ !is.na(i) ], collapse = ","))

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

oblast_json <- st_read("www/geodata/Oblasts.geojson")
raion_json <- st_read("www/geodata/Raions.json")
hromada_json <- st_read("www/geodata/Hromadas.json")

query <- "SELECT * FROM [dbo].[representative_columns_table];"

projects_data <- dbGetQuery(my_connection, query)

# split TABLE_ID with '_", extract project_id, round and survey_type as first, second and third element

projects_data <- projects_data %>%
  dplyr::mutate(
    project_id = sapply(strsplit(projects_data$TABLE_ID, "_"), "[[", 1),
    round = sapply(strsplit(projects_data$TABLE_ID, "_"), "[[", 2),
    survey_type = sapply(strsplit(projects_data$TABLE_ID, "_"), "[[", 3))


