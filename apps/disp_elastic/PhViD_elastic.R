# rewrite as.PhVid function with API query:
library(jsonlite)
library(dplyr)
library(memoise)

base_url<-'https://node.hres.ca/drug/event'



hc_result<-function(url){
  
  result = httr::GET(url)
  httr::stop_for_status(result)
  hc_result<-fromJSON(httr::content(result, as='text'))$results
  
  return(hc_result)
}


test_res<-hc_result('https://node.hres.ca/drug/event?search=report_ingredient_suspect:abacavir+AND+reaction_pt:abdominal%20pain&count=datintreceived:quarter',F)


df_url<-paste0(base_url,'?count=')

as.PhViD_HCSC <-  function(DATA.FRAME, MARGIN.THRES = 1){
    n1.df<-paste0(base_url,'?count=report_ingredient_suspect.keyword')%>%
           hc_result()%>%
           filter(doc_count>=MARGIN.THRES)
           rename(n1. = doc_count)
    
    n.1df<-paste0(base_url,'?count=reaction_pt.keyword')%>%
             hc_result()%>%
             filter(doc_count>=MARGIN.THRES)
              rename(n.1 = doc_count)
           
  
    
    # generate new count table
    output <- DATA.FRAME %>%
      dplyr::left_join(n1._df, by = coln[1]) %>%
      dplyr::left_join(n.1_df, by = coln[2]) %>%
      # drop all rows for which a marginal doesn't exist
      dplyr::filter(!is.na(n1.) & !is.na(n.1))
    
    if ( identical(dplyr::select(output, 1:3), DATA.FRAME) ) {
      output %<>% dplyr::rename(n11 = count)
      # all marginals satisfy threshold since no entries dropped
      threshold_satisfied = TRUE
    } else {
      # recalculate marginals based on which terms were kept and
      #   check again that marginal threshold is satisified
      DATA.FRAME <- dplyr::select(output, 1:3)
    }
  }
  RES <- vector(mode="list")
  RES$L <- output %>% dplyr::select(1:2)
  RES$data <- output %>% dplyr::select(n11, n1., n.1)
  RES$N <- sum(output$n11)
  RES
}