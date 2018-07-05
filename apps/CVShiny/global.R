# data manip + utils
library(dbplyr)
library(Hmisc)
library(magrittr)
library(utils)
#library(zoo)
library(pool)

# data visualizations
library(plotly)
library(ggplot2)
library(googleVis)

# Shiny libraries
library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)
library(dplyr)
library(lubridate)
library(feather)
library(RPostgreSQL)


source("common_ui.R")
source("linechart.R")
source("pieTableUtil.R")
source("barTableUtil.R")




options(shiny.trace=TRUE)
# -----------------------------------------------------------------------------

########## Codes to fetch top 1000 specific results to be used in dropdown menu ############### 
# Temperary solution: fetch all tables to local and run functions on them

# print('start of global')
# print(Sys.time())


#create a cvponl_poolection pool: insert relevant password and username
cvponl_pool <- dbPool(drv      = RPostgreSQL::PostgreSQL(),
                      host     = "shiny.hc.local",
                      dbname   = "cvponl",
                      user     = "",
                      password = "" )



drv <- dbDriver("PostgreSQL")
#cvponl <- dbcvponl_poolect(drv, dbname='cvponl', host='shiny.hc.local', port=80, user='', password ='')
#get max date and meddra within our current schema
meddra_and_date <- dbGetQuery(cvponl_pool, "SELECT  MAX(datintreceived) AS max_date, MAX(meddra_version) AS med_version FROM date_refresh.history") 

max_date <- meddra_and_date %>%
  `[[`(1)

# print('max_date')
# print(max_date)

max_meddra <- meddra_and_date %>%
  `[[`(2) 

cv_reports                  <- tbl(cvponl_pool, in_schema("current2", "reports_table")) %>% collect()

cv_report_drug              <- tbl(cvponl_pool, in_schema("current2", "report_drug" ))   %>% collect()

cv_drug_product_ingredients <- tbl(cvponl_pool, in_schema("current2", "drug_product_ingredients")) %>% collect()

cv_meddra                   <- tbl(cvponl_pool, in_schema("meddra", gsub('\\.', '_', max_meddra))) %>% collect()

cv_reactions                <- tbl(cvponl_pool, in_schema("current2", "reactions")) %>% collect() %>% left_join(cv_meddra, na_matches = 'never', by = "pt_code")



cv_reports_temp <- cv_reports %>%
  select(report_id, seriousness_eng, death)

cv_report_drug %<>% left_join(cv_reports_temp, "report_id" = "report_id")
cv_reactions %<>% left_join(cv_reports_temp, "report_id" = "report_id") 



#following Queries are used to generate autocomplete lists

topbrands <- cv_report_drug %>%
  distinct(drugname) %>%
  `[[`(1) %>%
  sort() %>%
  `[`(-c(1,2)) # dropping +ARTHRI-PLUS\u0099 which is problematic


topings_cv <- cv_drug_product_ingredients %>%
  distinct(active_ingredient_name) %>%
  `[[`(1) %>%
  sort()


smq_choices <- cv_reactions %>%
  distinct(smq_name) %>%
  filter(!is.na(smq_name)) %>%
  `[[`(1) %>%
  sort()

pt_choices <- cv_reactions %>%
  distinct(pt_name_eng) %>% 
  `[[`(1) %>%
  c(smq_choices) %>%
  sort()

soc_choices <- cv_reactions %>%
  distinct(soc_name_eng) %>%
  `[[`(1) %>%
  sort()

# cv_reports                  <- tbl(cvponl_pool, in_schema("current2", "reports_table"))
# cv_report_drug              <- tbl(cvponl_pool, in_schema("current2", "report_drug" ))
# cv_drug_product_ingredients <- tbl(cvponl_pool, in_schema("current2", "drug_product_ingredients"))
# cv_meddra                   <- tbl(cvponl_pool, in_schema("meddra", gsub('\\.', '_', max_meddra))) 


# Grabbing column names from the tbl metadata.
# Used for selecting columns in the downloads tab.
cv_report_drug_names <- colnames(cv_report_drug) 
cv_reaction_names   <- colnames(cv_reactions) 
cv_reports_names     <- colnames(cv_reports)


