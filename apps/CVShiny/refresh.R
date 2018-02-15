library(data.table)
library(magrittr)
library(dplyr)
library(plyr)
library(pool)
library(RPostgreSQL)
#library(feather)
#library(foreach)
#library(doParallel)

cvponl_write <- dbPool(drv      = RPostgreSQL::PostgreSQL(),
                       host     = "shiny.hc.local",
                       dbname   = "cvponl",
                       user     = "******",
                       password = "******")

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
  reactions_soc <- dbGetQuery(cvponl_write, "SELECT reaction_id, report_id, pt_code, pt_name_eng, pt_name_fr, soc_code, soc_name_fr, soc_name_eng FROM current.reactions") %>%
    left_join(final_table, na_matches = 'never', by = "pt_code") 
  
  #upload table
  dbWriteTable(cvponl_write,  c("meddra", meddra_list[4]), reactions_soc, overwrite = FALSE, temporary = FALSE, row.names = FALSE)
  #create indices for values used later: this might not be a complete list
  dbGetQuery(con, paste0("CREATE INDEX report_id ON  meddra.", meddra_list[4], " (report_id)"))
  dbGetQuery(con, paste0("CREATE INDEX smq_name ON meddra.", meddra_list[4], " (smq_name)"))
  dbGetQuery(con, paste0("CREATE INDEX pt_name_eng ON meddra.", meddra_list[4], " (pt_name_eng)"))
  dbGetQuery(con, paste0("CREATE INDEX soc_name_eng ON meddra.", meddra_list[4], " (soc_name_eng)"))
  
  #originally I intended to uploade the tables individually and join them together, however joins are expensive so I joined them to the reaction table,
  #probably doesn't take up that much extra space and this is the only usecase right now so it works, however in the future it might be
  #valuable to use the all the tables (and include all their columns that I filtered out above)
  #dbWriteTable(cvponl_write, c("current", "meddra_hlt_pref_comp"), value = meddra_hlt_pref_comp, append = TRUE, row.names = FALSE)
  #dbWriteTable(cvponl_write, c("current", "meddra_hlt_pref_term"), value = meddra_hlt_pref_term, append = TRUE, row.names = FALSE)
  #dbWriteTable(cvponl_write, c("current", "meddra_pref_term"), value = meddra_pref_term, append = TRUE, row.names = FALSE)
  #dbWriteTable(cvponl_write, c("current", "meddra_smq_content"), value = meddra_smq_content, append = TRUE, row.names = FALSE)
  #dbWriteTable(cvponl_write, c("current", "meddra_smq_list"), value = meddra_smq_list, append = TRUE, row.names = FALSE)
  
}

#updates database table with the maximum date and current meddra version
#INPUT: max_date; max date of a report in the remote table
#     : con; a connection/pool
date_update <- function(max_date, con){

  
  schema_name <- paste0("refresh_", gsub("-", "_", toString(max_date)))
  meddra_version <- meddra_parse() %>%
    `[`(1)
  
  dbGetQuery(con, "CREATE SCHEMA IF NOT EXISTS date_refresh")
  
  history_table <- data.frame(ref_date=max_date,
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
    dplyr::summarize(max_date = max(ref_date)) %>%
    `[[`(1) 
  
  if (current_date >= remote_date){
    return(FALSE)
  }
  else{
    return(TRUE)
  }

}

#could break this down into smaller functions, but it only has one use case
#this function is the main function that calls if the check function fails (I GUESS), need to move the if statements
#therefore it is only called if date in remote has changed!
refresh <- function() {
  
  
  #TODO: getting date from here would be the fastest way to find out if the current schema is out of date
  
  #get the date from the refresh tracking schema

  
  #get the most recent date of a report published in remote schema
  remote_date <- dbGetQuery(cvponl_write, "SELECT * FROM remote.reports") %>%
    dplyr::summarize(max_date = max(datintreceived)) %>%
    `[[`(1) 
  
  current_meddra <- dbGetQuery(cvponl_write, "SELECT * FROM date_refresh.history") %>%
    dplyr::summarize(max_med = max(meddra_version)) %>%
    `[[`(1) 
  
  meddra <- meddra_parse()
  most_recent_meddra <- meddra[1]


  
  schema_new <- date_update(remote_date, cvponl_write)
  
  dbGetQuery(cvponl_write, paste0("ALTER SCHEMA current2 RENAME TO ", schema_new))
  
  schema_name <- "current2"
                       
  #get a list of all tables from remote schema to be copied
  remote_table_list <- dbGetQuery(cvponl_write, "SELECT DISTINCT table_name 
    FROM information_schema.tables WHERE table_schema = 'remote'") %>%
    `[[`(1)
  
  dbGetQuery(cvponl_write, "CREATE SCHEMA IF NOT EXISTS current2")
  
  query_list <- lapply(remote_table_list, function(x) paste0("CREATE MATERIALIZED VIEW ",  schema_name, ".", x, " AS SELECT * FROM remote.", x))
  
  #applies each query
  lapply(query_list, dbGetQuery, con=cvponl_write)
  
  reports <- dbGetQuery(cvponl_write, "SELECT * FROM remote.reports")
  updated_reports <-age_group_clean(reports)
  
  #add the age_group_clean column to the reports table, this is a work around and should be done upstream to save time, but for now this works
  #this means that there is an extra table called reports_table within the schema at the moment, ideally reports would just have an extra column
  dbWriteTable(cvponl_write, c(schema_name, "reports_table"), value = updated_reports, append = FALSE, row.names = FALSE)
  
  
  if(most_recent_meddra > current_meddra) {
    
    meddra_make(meddra, cvponl_write)
  }
  #this doesn't work properly!
  return(schema_name)
  
}

#Getting cv_substances table: This table has the mapping from active ingredients to drugnames?
#drug_product_ingredients <- dbGetQuery(cvponl_write, "SELECT * FROM current.drug_product_ingredients")
#active_ingredients <- dbGetQuery(cvponl_write, "SELECT * FROM current.active_ingredients")

#cv_substances <- left_join(drug_product_ingredients, active_ingredients, "active_ingredient_id")%>%
#  group_by(primary_ingredient_id)



close_all_con <- function() {
  all_cons <- dbListConnections(RPostgreSQL::PostgreSQL())
  for(con in all_cons)
    +  dbDisconnect(con)
}

