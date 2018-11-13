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


#connect to elastic gate for meddra data:
connect(es_host = "elastic-gate.hc.local", es_port = 80,errors = "complete")


#auto list for brands
topbrands<-'{ 
  "aggs": {"brandname": {"nested": {"path": "report_drug_detail"},
  "aggs": {"brand": {"terms": {
            "field": "report_drug_detail.drugname.keyword",
            "size": 1000000
          }}}}}}'

topbrands <- Search(index='drug_event',body=topbrands,size=0,raw=T)%>%fromJSON()
topbrands<-topbrands$aggregations$brandname$brand$buckets$key%>%sort()

#autolist for ingredients
topings_cv<-'{ 
  "aggs": {"ing": {"nested": {"path": "report_drug_detail"},
  "aggs": {"ing": {"terms": {
            "field": "report_drug_detail.ingredients.keyword",
            "size": 1000000
          }}}}}}'

topings_cv <- Search(index='drug_event',body=topings_cv,size=0,raw=T) %>% fromJSON()
topings_cv<-topings_cv$aggregations$ing$ing$buckets$key%>%sort()

#auto lists for both soc and pt (right now there is no soc in elastic - Dan needs to add before soc_choices works)
soc_choices<-'{
  "aggs": {"soc": {"terms": {
        "field": "reaction_soc.keyword",
         "size": 100000
      }}}}'
soc_choices<-Search(index='drug_event',body=soc_choices,size=0,raw=T) %>% fromJSON()
soc_choices<-soc_choices$aggregations$soc$buckets$key%>%sort()
  
  
smq_body<-'{ "aggs" : {"smq_term" : {
            "terms" : {
            "field" : "smq_name.keyword",
            "size":10000
            }}}}'
  
smq_list<-Search(index='meddra_pt',body=smq_body,size=0)$aggregations$smq_term$buckets
smq<-sapply(smq_list,'[[',1)%>%sort()


pt<-'{
  "aggs": {"pt": {"terms": {
        "field": "reaction_pt.keyword",
         "size": 100000
      }}}}'
pt<-Search(index='drug_event',body=pt,size=0,raw=T) %>% fromJSON()
pt<-pt$aggregations$pt$buckets$key%>%sort()

pt_choices<-c(pt,smq)


#from elasticsearch, take the maxiumn receivedate from all reports:
body_date<-'{
    "aggs" : {
        "max_date" : { "max" : { "field" : "datreceived" } }
    }
}'

max_date_res<-Search(index='drug_event',body=body_date,size=0)$aggregations$max_date[[2]]
max_date<-as.Date(max_date_res,format='%Y-%m-%d')

body_med<-'{
    "aggs" : {
        "max_version" : { "max" : { "field" : "meddra_version" } }
    }
}'

max_version<-Search(index='meddra_pt',body=body_med,size=0)$aggregations$max_version[[1]]
max_meddra<-paste0('v.',max_version)

