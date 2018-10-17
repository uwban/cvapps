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


source('common_ui.R')
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
  #url<-gsub(' ','%20',url)
  return(url)
}

# drug and adverse event dropdown menu choices
#drug_choices <-hc_result(paste0(base_url,'?count=report_drugname_suspect.keyword'))%>%pull(key)%>%sort()
ing_choices<-paste0(base_url,'?count=report_ingredient_suspect.keyword')%>%
             add_api_key()%>%
             hc_result(F)%>%
             dplyr::pull(key)

pt_choices<-paste0(base_url,'?count=reaction_pt.keyword')%>%
  add_api_key()%>%
  hc_result(F)%>%
  dplyr::pull(key)

#updated date:
date<-paste0(base_url,'?count=datintreceived')%>%
  add_api_key()%>%
  hc_result(F)

max_date<-max(date$key_as_string)%>%as.Date(format='%Y-%m-%d')
  

# query functions ####
clean_string<-function(string){
  string <- gsub("%", "%20", string)
  string <- gsub("'", "%27", string)
  string <- gsub("!", "", string)
  string <- gsub("&", "", string)
  string <- gsub(' ', '%20', string)
  
  string<-paste0('\"',string,'\"')

  string
}

#for now set startDate and EndDate
startDate<-as.Date('1965-01-01','%Y-%m-%d')
endDate<-max_date

create_uri<-function(startDate,endDate,gender='All',rxn=NULL,drug_ing=NULL,count_term=NULL,limit=NULL,skip=NULL){
  search_uri<-paste0(base_url,'?search=','datintreceived:[', toString(startDate), '+TO+', toString(endDate), ']')
  
  if(gender!="All"){
    search_uri<-paste0(search_uri,'+AND+patient_gender:',gender)
  }
  
  if(!is.null(rxn)){
    search_uri <- paste0(search_uri, '+AND+reaction_pt.keyword:', clean_string(rxn))
  }
  
  if(!is.null(drug_ing)){
    search_uri <- paste0(search_uri, '+AND+report_ingredient_suspect.keyword:', clean_string(drug_ing))
  }
  
  if(!is.null(count_term)){
    search_uri <- paste0(search_uri, '&count=',count_term)
  }
  
  if(!is.null(limit)){
    search_uri <- paste0(search_uri, '&limit=',limit)
  }
  
  if(!is.null(skip)){
    search_uri<-paste0(search_uri,'&skip=',skip)
  }
  
  return(search_uri)
}


#parse all returns:
parse_all<-function(startDate,endDate,gender,rxn,drug_ing){
  response<-create_uri(startDate,endDate,gender='All',rxn,drug_ing,count_term=NULL,limit=NULL,skip=NULL)%>%
           add_api_key()%>%
           hc_result(F)
  
  total<-create_uri(startDate,endDate,gender='All',rxn,drug_ing,count_term=NULL,limit=NULL,skip=NULL)%>%
         add_api_key()%>%
         hc_result(T)
  
  if (total>1000){
    niter<-floor(total/1000)
    result_more<-list()
    for (i in 1:niter){
      result_more[[i]]<-create_uri(startDate,endDate,gender='All',rxn,drug_ing,count_term=NULL,limit=1000,skip=1000*i)%>%
                        add_api_key()%>%
                        hc_result(F)
    }
    response_more<-rbind_pages(result_more)
    response_final<-rbind_pages(list(response,response_more))
    
  }else{
    
    response_final<-response
  }
  
  return(response_final)
 }

report_col<-c('report_id','report_no','disability',
              'death','mah_no','source','outcome',
              'version_no','datreceived','report_type','seriousness',
              'hosp_required','reporter_type','datintreceived','patient_gender',
              'patient_weight','life_threatening','congenital_anomaly','patient_weight_unit',
              'report_drugname_suspect','report_ingredient_suspect','n_drugs_per_report',
              'patient_age','patient_age_y','patient_age_unit','patient_height','patient_height_unit','patient_age_group')