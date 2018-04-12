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


source("common_ui.R")
source("linechart.R")
source("pieTableUtil.R")
source("barTableUtil.R")




options(shiny.trace=TRUE)
# -----------------------------------------------------------------------------

########## Codes to fetch top 1000 specific results to be used in dropdown menu ############### 
# Temperary solution: fetch all tables to local and run functions on them




#create a connection pool: insert relevant password and username
cvponl_pool <- dbPool(drv      = RPostgreSQL::PostgreSQL(),
                      host     = "shiny.hc.local",
                      dbname   = "cvponl",
                      user     = "",
                      password = "")



#get max date and meddra within our current schema
meddra_and_date <- dbGetQuery(cvponl_pool, "SELECT  MAX(datintreceived) AS max_date, MAX(meddra_version) AS med_version FROM date_refresh.history") 
  
max_date <- meddra_and_date %>%
  `[[`(1)

max_meddra <- meddra_and_date %>%
  `[[`(2) 


cv_reports                  <- tbl(cvponl_pool, in_schema("current2", "reports_table"))
cv_report_drug              <- tbl(cvponl_pool, in_schema("current2", "report_drug" ))
cv_drug_product_ingredients <- tbl(cvponl_pool, in_schema("current2", "drug_product_ingredients"))
cv_meddra                <- tbl(cvponl_pool, in_schema("meddra", gsub('\\.', '_', max_meddra)))
cv_reactions <- tbl(cvponl_pool, in_schema("current2", "reactions")) %>% left_join(cv_meddra, na_matches = 'never', by = "pt_code")


cv_reports_temp <- cv_reports %>%
  select(report_id, seriousness_eng, death)

cv_report_drug %<>% left_join(cv_reports_temp, "report_id" = "report_id")
cv_reactions %<>% left_join(cv_reports_temp, "report_id" = "report_id")



#following Queries are used to generate autocomplete lists


directory <- getwd()

#read feather files for autocomplete lists
topbrands <- read_feather(paste0(directory, '/feather_files/topbrands.feather')) %>%
  `[[`(1)
topings_cv <- read_feather(paste0(directory, '/feather_files/topings_cv.feather'))%>%
  `[[`(1)
smq_choices <- read_feather(paste0(directory, '/feather_files/smq_choices.feather'))%>%
  `[[`(1)
pt_choices <- read_feather(paste0(directory, '/feather_files/pt_choices.feather'))%>%
  `[[`(1)
soc_choices <- read_feather(paste0(directory, '/feather_files/soc_choices.feather'))%>%
  `[[`(1)


# Grabbing column names from the tbl metadata.
# Used for selecting columns in the downloads tab.
cv_report_drug_names <- cv_report_drug$ops$args$vars$alias
cv_reaction_names   <- cv_reactions$ops$args$vars$alias
cv_reports_names     <- cv_reports$ops$vars