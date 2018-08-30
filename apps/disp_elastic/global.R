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

#change point analysis:
library(changepoint)
library(bcp)
library(ggfortify)



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

add_api_key<-function(url){
  #url<-paste0(url,'&key=40e40966014eb7ac')
  url<-paste0(url,'&limit=50000')
  url<-gsub(' ','%20',url)
  return(url)
}

# drug and adverse event dropdown menu choices
#drug_choices <-hc_result(paste0(base_url,'?count=report_drugname_suspect.keyword'))%>%pull(key)%>%sort()
ing_choices<-paste0(base_url,'?count=report_ingredient_suspect.keyword')%>%
             add_api_key()%>%
             hc_result(F)%>%
             dplyr::pull(key)%>%
             sort()

pt_choices<-paste0(base_url,'?count=reaction_pt.keyword')%>%
  add_api_key()%>%
  hc_result(F)%>%
  dplyr::pull(key)%>%
  sort()

aboutAuthors <- function() {list(
  tags$strong("Authors:"),
  fluidRow(
    box(
      "Daniel Buijs, MSc", br(),
      "Data Science Manager, Health Products and Food Branch", br(),
      "Health Canada / Government of Canada", br(),
      "daniel.buijs@canada.ca",
      width = 3
    ),
    box(
      "Nanqing zhu, MSc", br(),
      "Data Scientist, Health Products and Food Branch", br(),
      "Health Canada / Government of Canada", br(),
      "nanqing.zhu@canada.ca",
      width = 3
    ),
    box(
      "Sophia He, BSc (in progress)", br(),
      "Jr. Data Scientist Co-op, Health Products and Food Branch", br(),
      "Health Canada / Government of Canada", br(),
      "yunqingh@sfu.ca",
      width = 3
    ),
    box(
      "Kevin Thai, BSc (in progress)", br(),
      "Jr. Data Scientist Co-op, Health Products and Food Branch", br(),
      "Health Canada / Government of Canada", br(),
      "kthai@uwaterloo.ca",
      width = 3
    ),
    box(
      "Bryce Claughton, BMath (in progress)", br(),
      "Jr. Data Scientist Co-op, Health Products and Food Branch", br(),
      "Health Canada / Government of Canada", br(),
      "bclaught@uwaterloo.ca",
      width = 3
    )
  )
)}
