library(data.table)
library(magrittr)
library(plyr)
library(dplyr)
library(pool)

library(RPostgreSQL)
library(feather)



#could break this down into smaller functions, but it only has one use case
refresh <- function() {
  cvponl_pool <- dbPool(drv      = RPostgreSQL::PostgreSQL(),
                        host     = "shiny.hc.local",
                        dbname   = "cvponl",
                        user     = "hcwriter",
                        password = "canada2")
  
  cv_reports <- dbGetQuery(cvponl_pool, "SELECT * FROM current.reports")
  
  max_date <- cv_reports %>%
    summarize(max_date = max(datintreceived)) %>%
    `[[`(1) 
  
  schema_name <- paste0("refresh_",gsub("-", "_", max_date))
  
  dbGetQuery(cvponl_pool, paste0("CREATE SCHEMA IF NOT EXISTS ", schema_name))
                       
  #get a list of all tables from remote schema to be copied
  remote_table_list <- dbGetQuery(cvponl_pool, "SELECT DISTINCT table_name 
    FROM information_schema.tables WHERE table_schema = 'remote'") %>%
    `[[`(1)

  #applies each query
  lapply(query_list, dbGetQuery, con=cvponl_pool)
  
  #create a schema for dates of refreshes to be stored if there isn't one already
  dbGetQuery(cvponl_pool, "CREATE SCHEMA IF NOT EXISTS date_refresh")
  
  #create dataframe to insert into schema tracking table

  history_table <- data.frame(ref_date=max_date,
                   schema=schema_name,
                   stringsAsFactors = FALSE)
  
  #TODO: getting date from here would be the fastest way to find out if the current schema is out of date
  dbWriteTable(cvponl_pool, c("date_refresh", "history"), history_table, overwrite = FALSE, temporary = FALSE, row.names = FALSE)
  
}




# get tables from postgresql db. current is the schema used, use format: schema.tablename to access tables
cv_reports <- dbGetQuery(cvponl_pool, "SELECT * FROM current.reports")

#as per specifications in dist_file_format_20_1.pdf (tablename: filename), Select only columns necessary
#meddra_hlt_pref_comp: hlt_pt.asc
meddra_hlt_pref_comp <- fread('/home/shared/MedDRA/meddra_20_1_english/MedAscii/hlt_pt.asc', sep = '$') %>%
  select('V1','V2') %>%
  rename(c('V1' = 'hlt_code', 'V2' = 'pt_code'))


meddra_hlt_pref_term <- fread('/home/shared/MedDRA/meddra_20_1_english/MedAscii/hlt.asc', sep = '$') %>%
  select('V1','V2') %>%
  rename(c('V1' = 'hlt_code', 'V2' = 'hlt_name'))

#meddra_pref_term: pt.asc
meddra_pref_term <- fread('/home/shared/MedDRA/meddra_20_1_english/MedAscii/pt.asc', sep = '$') %>%
  select('V1','V4') %>%
  rename(c('V1' = 'pt_code', 'V4' = 'pt_soc_code'))

#meddra_smq_content: smq_content.asc TAKE EXTRA CARE WHEN JOINING SMQ_CONTENT TO OTHER THINGS
meddra_smq_content <- fread('/home/shared/MedDRA/meddra_20_1_english/MedAscii/smq_content.asc', sep = '$') %>%
  select('V1', 'V2') %>%
  rename(c('V1' = 'smq_code', 'V2' = 'term_code'))


#meddra_smq_list: smq_list.asc
meddra_smq_list <- fread('/home/shared/MedDRA/meddra_20_1_english/MedAscii/smq_list.asc', sep = '$') %>%
  select('V1', 'V2') %>%
  rename(c('V1' = 'smq_code', 'V2' = 'smq_name'))

#map hlt_name and smq_name to pt_soc_code which we can join in reactions table by: soc_code = pt_soc_code
final_table <- left_join(meddra_hlt_pref_term, meddra_hlt_pref_comp, by = "hlt_code") %>%
  left_join(meddra_pref_term, by = "pt_code") %>%  
  left_join(meddra_smq_content, by = c("pt_code" = "term_code")) %>%
  left_join(meddra_smq_list, by = "smq_code")


cvponl_pool <- dbPool(drv      = RPostgreSQL::PostgreSQL(),
                      host     = "shiny.hc.local",
                      dbname   = "cvponl",
                      user     = "****",
                      password = "****")


#Getting cv_substances table: This table has the mapping from active ingredients to drugnames?
drug_product_ingredients <- dbGetQuery(cvponl_pool, "SELECT * FROM current.drug_product_ingredients")
active_ingredients <- dbGetQuery(cvponl_pool, "SELECT * FROM current.active_ingredients")

cv_substances <- left_join(drug_product_ingredients, active_ingredients, "active_ingredient_id")%>%
  group_by(primary_ingredient_id)

View(cv_substances)

#get table to with soc_code to join with final_table (complete map)
reactions_soc <- dbGetQuery(cvponl_pool, "SELECT reaction_id, report_id, pt_code, pt_name_eng, pt_name_fr, soc_code, soc_name_fr, soc_name_eng FROM current.reactions") %>%
  left_join(final_table, na_matches = 'never', by = "pt_code") 



#upload table
dbWriteTable(cvponl_pool,  c("current", "meddra_hlt_soc_smq"), reactions_soc, overwrite = FALSE, temporary = FALSE, row.names = FALSE)
#create indices for values used later: this might not be a complete list
dbGetQuery(cvponl_pool, "CREATE INDEX report_id ON current.meddra_hlt_soc_smq (report_id)")
dbGetQuery(cvponl_pool, "CREATE INDEX smq_name ON current.meddra_hlt_soc_smq (smq_name)")
dbGetQuery(cvponl_pool, "CREATE INDEX pt_name_eng ON current.meddra_hlt_soc_smq (pt_name_eng)")
dbGetQuery(cvponl_pool, "CREATE INDEX soc_name_eng ON current.meddra_hlt_soc_smq (soc_name_eng)")


#originally I intended to uploade the tables individually and join them together, however joins are expensive so I joined them to the reaction table,
#probably doesn't take up that much extra space and this is the only usecase right now so it works, however in the future it might be
#valuable to use the all the tables (and include all their columns that I filtered out above)
#dbWriteTable(cvponl_pool, c("current", "meddra_hlt_pref_comp"), value = meddra_hlt_pref_comp, append = TRUE, row.names = FALSE)
#dbWriteTable(cvponl_pool, c("current", "meddra_hlt_pref_term"), value = meddra_hlt_pref_term, append = TRUE, row.names = FALSE)
#dbWriteTable(cvponl_pool, c("current", "meddra_pref_term"), value = meddra_pref_term, append = TRUE, row.names = FALSE)
#dbWriteTable(cvponl_pool, c("current", "meddra_smq_content"), value = meddra_smq_content, append = TRUE, row.names = FALSE)
#dbWriteTable(cvponl_pool, c("current", "meddra_smq_list"), value = meddra_smq_list, append = TRUE, row.names = FALSE)

all_cons <- dbListConnections(RPostgreSQL::PostgreSQL())
for(con in all_cons)
  +  dbDisconnect(con)
