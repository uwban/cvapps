library(lubridate)

#Params: term_list - list of strings
#Return: or - a single string of all in term_list seperated by +OR+
# or_together <- function(term_list) {
#   
#   term_list<-paste0('\"',term_list,'\"')
#   or<-''
#   for (i in 1:(length(term_list) - 1)) {
#     or <- paste0(or,term_list[i],'+OR+')
#   }
#   or <- paste0(or, term_list[length(term_list)])
#   
#   or<-sub('^.','',or)
#   or<-sub('.$','',or)
#   
#   return(or)
# }

or_together <- function(term_list) {
  or <- "("
  term_list<-paste0('\"',term_list,'\"')
  
  for (i in 1:(length(term_list) - 1)) {
    or <- paste0(or, term_list[i], '+OR+')
  }
  or <- paste0(or, term_list[length(term_list)], ')')
  
  return(or)
}


#escapes the following characters+ - && || ! ( ) { } [ ] ^ " ~ * ? : \ replaces spaces with %20%
#Params: string - a field that may contain a space "childrens advil"
#Return: string - same string with spaces replaced by %20% (used in lucene query string) "childrens%20%advil"
remove_spaces <- function(string) {
  string <- gsub("%", "%20", string)
  string <- gsub("'", "%27", string)
  #string <- gsub(",", "%20", string)
  #string <- gsub("-", "%20", string)
  #string <- gsub("\\/", "", string)
  string <- gsub("!", "", string)
  string <- gsub("&", "", string)
  string <- gsub(' ', '%20', string)
  
  first_char<-substr(string,1,1)
  
  if(first_char=="("){
    string <- string
  }else{
    string<-paste0('\"',string,'\"')
  }
  
  # # string <- gsub("\}", "\\}", string)
  # # string <- gsub("\[", "\\[", string)
  # # string <- gsub("\]", "\\]", string)
  # string <- gsub("^", "\\^", string)
  # string <- gsub("~", "\\~", string)
  # string <- gsub("*", "\\*", string)
  # string <- gsub("?", "\\?", string)
  # string <- gsub(":", "\\:", string)
  # string <- gsub("\\", "\\\\", string)
  
  string
  
}

#used for main plot of CVShiny
#Params: uri - a full uri for a lucene 
#Return: terms - a vector with three uris seperated by seriousness
add_term <- function(uri) {
  
  terms <- c(paste0(uri,"+AND+!death:true+AND+seriousness:Yes"), paste0(uri, "+AND+death:true"),  paste0(uri, "+AND+seriousness:No"))
  return (terms)
  
}

request <- function(search_uri) {
  
  
  r <- GET(search_uri)
  response <- content(r, "parsed")
  total <- response$total
  
  return(total)
}


request_listed <- function(uri_list) {
  
  totals <- lapply(uri_list, request)
  return(unlist(totals))
}

#Params:
#Return:
counter <- function(uri, count_term, key=''){
  search_uri <- paste0(uri, '&count=', count_term, key)
  r <- GET(search_uri)
  results<- content(r, 'parsed')$results
  
  df <- data.frame(category=sapply(results,`[[`,1),doc_count=sapply(results,`[[`,2), stringsAsFactors = FALSE)
  
  return(df)
}


#defaults are added so that function is reusible for counting (piecharts)
#Params:
#Return:
create_uri <- function(startDate, endDate, gender='All', age=c(0, 125), rxn=NULL, soc=NULL, drug_inv='Any', drugname=NULL, seriousness=NULL, search_type='', ...) {

  search_uri <- 'https://node.hres.ca/drug/event?search='
  
  if(length(drugname) > 1) {
    drugname <- or_together(drugname)
  }
  
  if(length(rxn) > 1 ) {
    rxn <- or_together(rxn)
  }
  
  if(length(soc) > 1){
    soc <- or_together(soc)
  }

  
  #TODO add options for months
  # search_uri <- paste0(search_uri, 'datintreceived:[', toString(dateSequence), '+TO+', toString(dateSequence %m+% years(1)), ']')
  #if(age[1] != 0 & age[2] != 125) {
  search_uri <- paste0(search_uri, 'datintreceived:[', toString(startDate), '+TO+', toString(endDate), ']')
  

  if(age[1] != 0 & age[2] != 125) {
    search_uri <- paste0(search_uri, '+AND+patient_age_y:', '[', age[1], '+TO+', age[2], ']')
  }

  if (gender != "All"){
    search_uri <- paste0(search_uri, '+AND+patient_gender:', gender)
  }

  if(!is.null(rxn)){
    search_uri <- paste0(search_uri, '+AND+reaction_pt:', remove_spaces(rxn))
  }

  if(!is.null(soc)) {
    search_uri <- paste0(search_uri, '+AND+reaction_soc:', remove_spaces(soc))
  }

  
  if(drug_inv == 'Concomitant' & !is.null(drugname) & search_type == 'brand'){
    search_uri <- paste0(search_uri, '+AND+report_drugname_concomitant:', remove_spaces(drugname))
  }
  
  else if(drug_inv == "Suspect" & !is.null(drugname) & search_type == 'brand'){
    search_uri <- paste0(search_uri, '+AND+report_drugname_suspect:', remove_spaces(drugname))
  }
  else if(drug_inv == "Suspect" & !is.null(drugname) & search_type == 'ingredient'){
    search_uri <- paste0(search_uri, '+AND+report_ingredient_suspect:', remove_spaces(drugname))
  }
  else if(drug_inv == "Concomitant" & !is.null(drugname) & search_type == 'ingredient'){
    search_uri <- paste0(search_uri, '+AND+report_ingredient_concomitant:', remove_spaces(drugname))
  }
  # else if ( !is.null(drugname)){
  #   search_uri <- paste0('+AND+report_drug.drugname:', remove_spaces(drugname))
  # 
  # }
  #THIS MIGHT BE  PROBLEM

  if(!is.null(seriousness)){
    
    if(seriousness == 'Death') {
      search_uri <- paste0(search_uri, '+AND+death:1')
    }
    else if(seriousness == 'Serious(Excluding Death)') {
      search_uri <- paste0(search_uri, '+AND+!death:1+AND+seriousness:Yes')
    }
  }
  
  return (search_uri)
}


#gets all time chart dat, splits by year and seriousness
#Params: current_search params - could be refactored to take a list
#Return: 
get_timechart_data <- function(time_period, date_list, gender, age, rxn, soc, drug_inv, drugname, seriousness, name_type, ...){
  result <- list()
  
  
  for (i in 1:(length(date_list)- 1)){

        search_uri<- create_uri(date_list[i], date_list[i+1], gender, age, rxn, soc, drug_inv, drugname, seriousness, name_type)
        
    search_uri <- add_term(search_uri)
    result[[i]] <- search_uri
  }
  
  return(result)
}

#
#Params: startDate - where sequence should start(inclusive), endDate - where Sequence should end (inclusive), time_period - month or year to split sequence by
#Return: dateSequence - interval of dates by time period specified, used to loop over and create a list of uri's for main graph generation
get_date_sequence <- function(startDate, endDate, time_period) {
  
  
  start_seq <- floor_date(as.Date(startDate), time_period)
  
  
  if(time_period == 'year') {
    dateSequence <- seq(start_seq, as.Date(endDate) %m+% years(1), by=time_period)
  }
  else {
    dateSequence <- seq(start_seq, as.Date(endDate) %m+% months(1), by=time_period)
  }
  
  dateSequence[1] <- as.Date(startDate)
  dateSequence[length(dateSequence)] <- endDate
  
  return(dateSequence)
}

#NANCY: 

#Params:
#Return:
parse_response <- function(uri){
  
  uri<-paste0(uri,'&limit=1000')
  response <- fromJSON(content(GET(uri), as='text'))$results
  total <- fromJSON(content(GET(uri), as='text'))$total
  if (total>1000){
    niter<-floor(total/1000)
    uri_more<-vector()
    result_more<-list()
    for (i in 1:niter){
      uri_more[i]<-paste0(uri,'&skip=',1000*i,'&limit=1000')
      result_more[[i]]<-fromJSON(content(GET(uri_more[i]), as='text'))$results
    }
    
    response_more<-rbind_pages(result_more)
    response_final<-rbind_pages(list(response,response_more))
    
  }else{
    
    response_final<-response
  }
  
  return(response_final)
}

