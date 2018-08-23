library(magrittr)
library(jsonlite)
library(lubridate)
library(tidyr)
library(dplyr)
library(utils)
library(zoo)
library(data.table)
# data visualizations
library(plotly)
library(ggplot2)
library(googleVis)

# Shiny libraries
library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)
library(markdown)

#BCPNN library
library(PhViD)
library(MCMCpack) #Monte carlo simulation

source("common_ui.R")
source("stats_functions.R")
#### Data pre-processing ####

#function to query data:
base_url<-'https://node.hres.ca/drug/event'
hc_result<-function(url,summation=TRUE){
  
  result = httr::GET(url)
  httr::stop_for_status(result)
  
  if(summation){
  hc_result<-fromJSON(httr::content(result, as='text'))$total
  }else{
  hc_result<-fromJSON(httr::content(result, as='text'))$results
  }
  
  return(hc_result)
}

# drug and adverse event dropdown menu choices
#drug_choices <-hc_result(paste0(base_url,'?count=report_drugname_suspect.keyword'))%>%pull(key)%>%sort()
ing_choices<-hc_result(paste0(base_url,'?count=report_ingredient_suspect.keyword'),F)%>%pull(key)%>%sort()
pt_choices<-hc_result(paste0(base_url,'?count=reaction_pt.keyword'),F)%>%pull(key)%>%sort()

