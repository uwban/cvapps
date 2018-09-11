# NANCY: The remaining changes that need to be made: 3 graphs and the download section. 
#An array_list called reactions_soc,  similar to reactions_pt, needs to be created in elasticsearch.
#all of the functions that interact with the api are within utilities.R
#I have already written the code that uses it.
#finally, you might encounter a problem with escaping characters, lucene query strings don't seem to like a long list of these (they are documented in the function remove_spaces within utilities.R)
#Thank you and good luck with the rest of your time here! - James
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
library(tidyr)
library(lubridate)
library(feather)
library(RPostgreSQL)
library(httr)
library(jsonlite)
library(elastic)


source("common_ui.R")
source("linechart.R")
source("pieTableUtil.R")
source("barTableUtil.R")
source("utilities.R")




options(shiny.trace=TRUE)
# -----------------------------------------------------------------------------
#The api key isn't necessary right now, but if in the future it is created place here (include full syntax, for example: '&key=a78asdfkad78')
uri <- 'https://node.hres.ca/drug/event?'
api_key <- ''

#connect to elastic gate for meddra data:
connect(es_host = "elastic-gate.hc.local", es_port = 80,errors = "complete")


#auto list for brands
concomitants <- counter(uri, 'report_drugname_concomitant.keyword&limit=1000000', api_key)
suspects <- counter(uri, 'report_drugname_suspect.keyword&limit=100000', api_key)
topbrands <- rbind(concomitants, suspects)[,1] %>% unique() %>% sort()

#autolist for ingredients
concomitants <- counter(uri, 'report_ingredient_concomitant.keyword&limit=1000000', api_key)
suspects <- counter(uri, 'report_ingredient_suspect.keyword&limit=100000', api_key)
topings_cv <- rbind(concomitants, suspects)[,1] %>% unique() %>% sort()

#auto lists for both soc and pt (right now there is no soc in elastic - Dan needs to add before soc_choices works)
soc_choices <- counter(uri, 'reaction_soc.keyword&limit=10000', api_key)[,1]%>%sort()
pt_choices <- counter(uri, 'reaction_pt.keyword&limit=100000', api_key)[,1] %>% sort()

max_date<-as.Date(Sys.time(),format='%Y-%m-%d')
max_meddra<-'v.21.0'

#populate pt_hlt relationship table:


########## Codes to fetch top 1000 specific results to be used in dropdown menu ###############
# Temperary solution: fetch all tables to local and run functions on them

# print('start of global')
# print(Sys.time())

#
#create a connection pool: insert relevant password and usernamev (populates lists)
# cvponl_pool <- dbPool(drv      = RPostgreSQL::PostgreSQL(),
#                       host     = "shiny.hc.local",
#                       dbname   = "cvponl",
#                       user     = "",
#                       password = "" )



#get max date and meddra within our current schema

# max_meddra <- dbGetQuery(cvponl_pool, "SELECT  MAX(meddra_version) FROM current2.reactions") %>%
#   `[[`(1)
# 
# 
# 
# max_date <- dbGetQuery(cvponl_pool, "SELECT  MAX(datintreceived) FROM current2.reports_table") %>%
#   `[[`(1)
# 
# 
# 
# cv_reports                  <- tbl(cvponl_pool, in_schema("current2", "reports_table"))
# cv_report_drug              <- tbl(cvponl_pool, in_schema("current2", "report_drug" ))
# cv_drug_product_ingredients <- tbl(cvponl_pool, in_schema("current2", "drug_product_ingredients"))
# cv_meddra                   <- tbl(cvponl_pool, in_schema("meddra", gsub('\\.', '_', max_meddra)))
# cv_reactions                <- tbl(cvponl_pool, in_schema("current2", "reactions ")) %>% left_join(cv_meddra, na_matches = 'never', by = "pt_code")

#cvapps <- tbl(cvponl_pool, in_schema("current2", "cvapps"))

# 
# cv_reports_temp <- cv_reports %>%
#   select(report_id, seriousness_eng, death)
# 
# cv_report_drug %<>% left_join(cv_reports_temp, "report_id" = "report_id")
# cv_reactions %<>% left_join(cv_reports_temp, "report_id" = "report_id")
# 
# 
# NANCY
# #following Queries are used to generate autocomplete lists using the database. It is also how the downloads get the column names
# 
# # 
#
# 
# topbrands <- cv_report_drug %>%
#   distinct(drugname) %>%
#   as.data.frame() %>%
#   `[[`(1) %>%
#     sort() %>%
#    `[`(-c(1,2)) # dropping +ARTHRI-PLUS\u0099 which is problematic
# 
# 
# topings_cv <- cv_drug_product_ingredients %>%
#   distinct(active_ingredient_name) %>%
#   as.data.frame() %>%
#   `[[`(1) %>%
#   sort()                                                                            
# 
# 
# smq_choices <- cv_reactions %>%
#   distinct(smq_name) %>%
#   as.data.frame() %>%
#   filter(!is.na(smq_name)) %>%
#   `[[`(1) %>%
#   sort()
# 
# pt_choices <- cv_reactions %>%
#   distinct(pt_name_eng) %>%
#   as.data.frame() %>%
#   `[[`(1) %>%
#   c(smq_choices) %>%
#   sort()
# 
# soc_choices <- cv_reactions %>%
#   distinct(soc_name_eng) %>%
#   as.data.frame() %>%
#   `[[`(1) %>%
#   sort()
# 



# Grabbing column names from the tbl metadata.
# Used for selecting columns in the downloads tab.
# cv_report_drug_names <- cv_report_drug$ops$args$vars$alias
# cv_reaction_names   <- cv_reactions$ops$args$vars$alias
# cv_reports_names     <- cv_reports$ops$vars