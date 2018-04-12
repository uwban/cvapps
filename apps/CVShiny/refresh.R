library(data.table)
library(magrittr)
library(pool)
library(RPostgreSQL)
library (feather)


cvponl_write <- dbPool(drv      = RPostgreSQL::PostgreSQL(),
                       host     = "shiny.hc.local",
                       dbname   = "cvponl",
                       user     = "",
                       password = "")



#make sure this runs after a database and/or meddra update
write_feather_files <- function() {
  
  max_date <- meddra_and_date %>%
    `[[`(1)
  
  max_meddra <- meddra_and_date %>%
    `[[`(2) 
  
  
  cv_reports                  <- tbl(cvponl_write, in_schema("current2", "reports_table"))
  cv_report_drug              <- tbl(cvponl_write, in_schema("current2", "report_drug" )) #%>%
    #select(report_id, drug_product_id, drugname, druginvolv_eng, indication_name_eng)
  cv_drug_product_ingredients <- tbl(cvponl_write, in_schema("current2", "drug_product_ingredients")) #%>%
    #select(active_ingredient_name, drugname, drug_product_id)
  cv_reactions                <- tbl(cvponl_write, in_schema("meddra", gsub('\\.', '_', max_meddra)))
  
  
  cv_reports_temp <- cv_reports %>%
    select(report_id, seriousness_eng, death)
  
  cv_report_drug %<>% left_join(cv_reports_temp, "report_id" = "report_id")
  cv_reactions %<>% left_join(cv_reports_temp, "report_id" = "report_id")
  
  #following Queries are used to generate autocomplete lists
  topbrands <- cv_report_drug %>%
    distinct(drugname) %>%
    as.data.frame() %>%
    `[[`(1) %>%
    sort() %>%
    `[`(-c(1,2))%>% #dropping +ARTHRI-PLUS\u0099 which is problematic
    as.data.frame() 
  
  topings_cv <- cv_drug_product_ingredients %>%
    distinct(active_ingredient_name) %>%
    as.data.frame() %>%
    `[[`(1) %>%
    sort() %>% 
    as.data.frame()
  
  smq_choices <- cv_reactions %>%
    distinct(smq_name) %>%
    as.data.frame() %>%
    filter(!is.na(smq_name)) %>%
    `[[`(1) %>%
    sort()
  
  pt_choices <- cv_reactions %>%
    distinct(pt_name_eng) %>% 
    as.data.frame() %>%
    `[[`(1) %>%
    c(smq_choices) %>%
    sort() %>% 
    as.data.frame()
  
  smq_choices %<>% as.data.frame()
  
  soc_choices <- cv_reactions %>%
    distinct(soc_name_eng) %>%
    as.data.frame() %>%
    `[[`(1) %>%
    sort() %>% 
    as.data.frame()
  
  
  directory <- getwd()
  
  topbrands_path <- paste0(directory, '/apps/CVShiny/feather_files/topbrands.feather')
  topings_cv_path <- paste0(directory, '/apps/CVShiny/feather_files/topings_cv.feather')
  smq_choices_path <- paste0(directory, '/apps/CVShiny/feather_files/smq_choices.feather')
  pt_choices_path <- paste0(directory, '/apps/CVShiny/feather_files/pt_choices.feather')
  soc_choices_path <- paste0(directory, '/apps/CVShiny/feather_files/soc_choices.feather')
  
  dir.create(file.path(directory, 'apps/CVShiny/feather_files'))
  
  file.create(topbrands_path)
  file.create(topings_cv_path)
  file.create(smq_choices_path)
  file.create(pt_choices_path)
  file.create(soc_choices_path)
  
  write_feather(topbrands, topbrands_path)
  write_feather(topings_cv, topings_cv_path)
  write_feather(smq_choices, smq_choices_path)
  write_feather(pt_choices, pt_choices_path)
  write_feather(soc_choices, soc_choices_path)
}

#categorizes into age groups, can't use what is in the reports table as is because it has a lot of NULL values
#INPUT: cv_reports: table 
age_group_clean <- function(cv_reports){
  cv_reports %<>%
    mutate(#age_group_clean = NA,
           age_group_clean = ifelse(is.na(age_y), "Unknown",
           ifelse(age_y <= 25/365, "Neonate",
           ifelse(age_y > 25/365 & age_y < 1, "Infant",
           ifelse(age_y >= 1 & age_y < 13, "Child",
           ifelse(age_y >= 13 & age_y < 18, "Adolescent",
           ifelse(age_y >= 18 & age_y <= 65, "Adult",
           ifelse(age_y > 65, "Elderly", age_group_eng ))))))))
}



#get the file name of most recent meddra folder. Should be in the form /home/shared/MedDRA/meddra_20_1_english
#parses the version number v.20.1 from the filename
#RETURN: c(meddra_version, meddra_path)
meddra_parse <- function(){

  meddra_file <- max(list.files(path='/home/shared/MedDRA'))
  meddra_name <- meddra_file %>% 
    gsub('meddra', 'v', .) %>%
    gsub('_english', '', .) 
  meddra_version <- meddra_name %>% gsub('_', '.', .)
  #finds the maximum file in this list and uses it
  meddra_path <- paste0('/home/shared/MedDRA/', meddra_file)
  
  return(c(meddra_version, meddra_file, meddra_path, meddra_name))
}

#creates new meddra into the new schema that was made
#INPUT: meddra_list: list of three objects where the first is the version and the second is the file and third is path
meddra_make <- function(meddra_list, con){
  
  dbGetQuery(con, "CREATE SCHEMA IF NOT EXISTS meddra")
  
  # get tables from postgresql db. current is the schema used, use format: schema.tablename to access tables
  cv_reports <- dbGetQuery(con, "SELECT * FROM remote.reports")
  
  #as per specifications in dist_file_format_20_1.pdf (tablename: filename), Select only columns necessary
  #meddra_hlt_pref_comp: hlt_pt.asc
  meddra_hlt_pref_comp <- fread(paste0(meddra_list[3], '/MedAscii/hlt_pt.asc'), sep = '$') %>%
    select('V1','V2') %>%
    plyr::rename(c('V1' = 'hlt_code', 'V2' = 'pt_code'))
  
  meddra_hlt_pref_term <- fread(paste0(meddra_list[3], '/MedAscii/hlt.asc'), sep = '$') %>%
    select('V1','V2') %>%
    plyr::rename(c('V1' = 'hlt_code', 'V2' = 'hlt_name'))
  
  #meddra_pref_term: pt.asc
  meddra_pref_term <- fread(paste0(meddra_list[3], '/MedAscii/pt.asc'), sep = '$') %>%
    select('V1','V4') %>%
    plyr::rename(c('V1' = 'pt_code', 'V4' = 'pt_soc_code'))
  
  #meddra_smq_content: smq_content.asc TAKE EXTRA CARE WHEN JOINING SMQ_CONTENT TO OTHER THINGS
  meddra_smq_content <- fread(paste0(meddra_list[3], '/MedAscii/smq_content.asc'), sep = '$') %>%
    select('V1', 'V2') %>%
    plyr::rename(c('V1' = 'smq_code', 'V2' = 'term_code'))
  
  #meddra_smq_list: smq_list.asc
  meddra_smq_list <- fread(paste0(meddra_list[3], '/MedAscii/smq_list.asc'), sep = '$') %>%
    select('V1', 'V2') %>%
    plyr::rename(c('V1' = 'smq_code', 'V2' = 'smq_name'))
  
  #map hlt_name and smq_name to pt_soc_code which we can join in reactions table by: soc_code = pt_soc_code
  final_table <- left_join(meddra_hlt_pref_term, meddra_hlt_pref_comp, by = "hlt_code") %>%
    left_join(meddra_pref_term, by = "pt_code") %>%  
    left_join(meddra_smq_content, by = c("pt_code" = "term_code")) %>%
    left_join(meddra_smq_list, by = "smq_code")
  

  #get table to with soc_code to join with final_table (complete map)
  reactions_soc <- dbGetQuery(cvponl_write, "SELECT reaction_id, report_id, pt_code, pt_name_eng, pt_name_fr, soc_code, soc_name_fr, soc_name_eng FROM remote.reactions") %>%
    left_join(final_table, na_matches = 'never', by = "pt_code") 
  
  #upload table (recently changed from reactions_soc to final_table)
  dbWriteTable(cvponl_write,  c("meddra", meddra_list[4]), final_table, overwrite = FALSE, temporary = FALSE, row.names = FALSE)
  #create indices for values used later: this might not be a complete list
  dbGetQuery(con, paste0("CREATE INDEX ON  meddra.", meddra_list[4], " (report_id)"))
  dbGetQuery(con, paste0("CREATE INDEX ON meddra.", meddra_list[4], " (smq_name)"))
  dbGetQuery(con, paste0("CREATE INDEX ON meddra.", meddra_list[4], " (pt_name_eng)"))
  dbGetQuery(con, paste0("CREATE INDEX ON meddra.", meddra_list[4], " (soc_name_eng)"))
}

#updates database table with the maximum date and current meddra version
#INPUT: max_date; max date of a report in the remote table
#     : con; a connection/pool
date_update <- function(max_date, con){

  
  schema_name <- paste0("refresh_", gsub("-", "_", toString(max_date)))
  meddra_version <- meddra_parse() %>%
    `[`(1)
  
  dbGetQuery(con, "CREATE SCHEMA IF NOT EXISTS date_refresh")
  
  history_table <- data.frame(datintreceivede=max_date,
                              schema=schema_name,
                              meddra_version=meddra_version,
                              stringsAsFactors = FALSE)
  
  dbWriteTable(con, c("date_refresh", "history"), 
               history_table, overwrite = FALSE, temporary = FALSE, row.names = FALSE, append=TRUE)
  
  return(schema_name)
}


#get the most recent date of a report published in remote schema, used in check function for reactiveTimer
dateCheck <- function() {

  #get the most recent date of a report published in remote schema
  remote_date <- dbGetQuery(cvponl_pool, "SELECT * FROM remote.reports") %>%
    dplyr::summarize(max_date = max(datintreceived)) %>%
    `[[`(1)
  
  current_date <- dbGetQuery(cvponl_pool, "SELECT * FROM date_refresh.history") %>%
    dplyr::summarize(max_date = max(datintreceived)) %>%
    `[[`(1) 
  
  if (current_date >= remote_date){
    return(FALSE)
  }
  else{
    return(TRUE)
  }

}


#useful for development
close_all_con <- function() {
  all_cons <- dbListConnections(RPostgreSQL::PostgreSQL())
  for(con in all_cons)
    +  dbDisconnect(con)
}

#could break this down into smaller functions, but it only has one use case
#this function is the main function that calls if the check function fails (I GUESS), need to move the if statements
#therefore it is only called if date in remote has changed! Calling refresh() should update 
refresh <- function() {
  
  
  #TODO: getting date from here would be the fastest way to find out if the current schema is out of date
  
  #get the date from the refresh tracking schema
  current_date <- dbGetQuery(cvponl_write, "SELECT * FROM date_refresh.history") %>%
    dplyr::summarize(max_date = max(datintreceived)) %>%
    `[[`(1) 
  
  #get the most recent date of a report published in remote schema
  remote_date <- dbGetQuery(cvponl_write, "SELECT * FROM remote.reports") %>%
    dplyr::summarize(max_date = max(datintreceived)) %>%
    `[[`(1) 
  
  #add indexes queries to this list for meddra and for current
  index_list <- c()
  
  
  #if there has been an update to remote schema
  if(current_date != remote_date){
    
    
    schema_new <- date_update(current_date, cvponl_write)
    
    dbGetQuery(cvponl_write, paste0("ALTER SCHEMA current2 RENAME TO ", schema_new))
    
    schema_name <- "current2"
                         
    #get a list of all tables from remote schema to be copied
    remote_table_list <- dbGetQuery(cvponl_write, "SELECT DISTINCT table_name 
      FROM information_schema.tables WHERE table_schema = 'remote'") %>%
      `[[`(1)
    
  
    dbGetQuery(cvponl_write, paste0("CREATE SCHEMA IF NOT EXISTS", schema_name))
    
    query_list <- lapply(remote_table_list, function(x) paste0("CREATE TABLE ",  schema_name, ".", x, " AS SELECT * FROM remote.", x))
    
    #applies each query
    lapply(query_list, dbGetQuery, con=cvponl_write)
    
    #Edit reports table
    reports <- dbGetQuery(cvponl_write, "SELECT * FROM remote.reports") %>% 
      
    #repoorts <- reports %>% mutate(milli_time = as.integer(as.POSIXct(datintreceived))*1000)
    updated_reports <- age_group_clean(reports)
    
  
    #add the age_group_clean column to the reports table, this is a work around and should be done upstream to save time, but for now this works
    #this means that there is an extra table called reports_table within the schema at the moment, ideally reports would just have an extra column
    dbWriteTable(cvponl_write, c(schema_name, "reports_table"), value = updated_reports, append = FALSE, row.names = FALSE)
    
    dbGetQuery(cvponl_write, paste0("ALTER TABLE ", schema_name, ".reports_table ALTER COLUMN datintreceived TYPE date"))
    
    #get all column names for each table that is used for creating indices
    index_list <- c(index_list, dbGetQuery(cvponl_write, paste0("SELECT DISTINCT column_name
    FROM information_schema.columns WHERE table_schema = '", schema_name, "' AND table_name = 'reports_table'")) %>%
                      `[[`(1) %>%
                      lapply(function(x) paste0('CREATE INDEX ON ', schema_name, '.reports_table', ' (', x, ')')))
    
    index_list <- c(index_list, dbGetQuery(cvponl_write, paste0("SELECT DISTINCT column_name
    FROM information_schema.columns WHERE table_schema =  '", schema_name, "' AND table_name = 'report_drug'")) %>%
                      `[[`(1) %>%
                      lapply(function(x) paste0('CREATE INDEX ON ', schema_name, '.report_drug', ' (', x, ')')))
    
    index_list <- c(index_list, dbGetQuery(cvponl_write, paste0("SELECT DISTINCT column_name
    FROM information_schema.columns WHERE table_schema =  '", schema_name, "' AND table_name = 'drug_product_ingredients'")) %>%
                      `[[`(1) %>%
                      lapply(function(x) paste0('CREATE INDEX ON ', schema_name, '.drug_product_ingredients', ' (', x, ')')))
  }
  
  current_meddra <- dbGetQuery(cvponl_write, "SELECT * FROM date_refresh.history") %>%
    dplyr::summarize(max_med = max(meddra_version)) %>%
    `[[`(1) 
  
  meddra <- meddra_parse()
  most_recent_meddra <- meddra[1]
  
  if (most_recent_meddra > current_meddra) {
    
    meddra_make(meddra, cvponl_write)
    
    index_list <- c(index_list, dbGetQuery(cvponl_write, paste0("SELECT DISTINCT column_name
    FROM information_schema.columns WHERE table_schema = 'meddra' AND table_name = '",meddra[4],"'")) %>%
                      `[[`(1) %>%
                      lapply(function(x) paste0('CREATE INDEX ON meddra.', meddra[4], ' (', x, ')'))) 
  }
  
 

  
  if (!is.null(index_list)){
    #create indices on all the columns (overkill, but whatever)
    lapply(index_list, dbGetQuery, con=cvponl_write)
  }
  

  #finish up by creating autocomplete lists
  write_feather_files()
  
}

close_all_con <- function() {
   all_cons <- dbListConnections(RPostgreSQL::PostgreSQL())
   for(con in all_cons)
       +  dbDisconnect(con)
}


refresh()


