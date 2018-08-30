server <- function(input, output, session) {
  
search_url<-reactiveValues( 

  d_count=paste0(base_url,'?search=report_ingredient_suspect:',ing_choices[1],'&count=_id.keyword')%>%add_api_key(),
  ae_count=paste0(base_url,'?search=reaction_pt:',pt_choices[1],'&count=_id.keyword')%>%add_api_key(),
  de_count=paste0(base_url,'?search=report_ingredient_suspect:',ing_choices[1],'+AND+','reaction_pt:',pt_choices[1],'&count=_id.keyword')%>%add_api_key(),
  total_count=paste0(base_url,'?count=_id.keyword')%>%add_api_key(),
  time=paste0(base_url,'?search=report_ingredient_suspect:',ing_choices[1],'+AND+','reaction_pt:',
                    pt_choices[1],'&count=datintreceived:quarter')%>%add_api_key(),
  drug=ing_choices[1],
  pt=pt_choices[1],
  cpa=paste0(base_url,'?search=report_ingredient_suspect:',ing_choices[1],'+AND+','reaction_pt:',
                                  pt_choices[1],'&count=datintreceived:quarter')%>%add_api_key(),
  dnprr_d=paste0(base_url,'?search=report_ingredient_suspect:',ing_choices[1],'&count=datintreceived:quarter')%>%add_api_key(),
  dnprr_ae=paste0(base_url,'?search=reaction_pt:',pt_choices[1],'&count=datintreceived:quarter')%>%add_api_key(),
  dnprr_total=paste0(base_url,'?count=datintreceived:quarter')%>%add_api_key(),
  llr_d=paste0(base_url,'?search=report_ingredient_suspect:',ing_choices[1],'&count=reaction_pt.keyword')%>%add_api_key()
)
  
  observeEvent(input$search_button,{
    
   url_d<-paste0(base_url,'?search=report_ingredient_suspect:',input$search_drug,'&count=_id.keyword')%>%add_api_key()
  
   url_ae<-paste0(base_url,'?search=reaction_pt:',input$search_pt,'&count=_id.keyword')%>%add_api_key()
 
   url_de<-paste0(base_url,'?search=report_ingredient_suspect:',input$search_drug,
                  '+AND+','reaction_pt:',input$search_pt,'&count=_id.keyword')%>%add_api_key()
 
   
   url_time<-paste0(base_url,'?search=report_ingredient_suspect:',input$search_drug,'+AND+','reaction_pt:',
                          input$search_pt,'&count=datintreceived:quarter')%>%add_api_key()
   
   
   url_cpa_count<-paste0(base_url,'?search=report_ingredient_suspect:',input$search_drug,'+AND+','reaction_pt:',
                         input$search_pt,'&count=datintreceived:quarter')%>%add_api_key()
   
   url_dnprr_d<-paste0(base_url,'?search=report_ingredient_suspect:',input$search_drug,'&count=datintreceived:month')%>%add_api_key()
   
   url_dnprr_ae<-paste0(base_url,'?search=reaction_pt:',input$search_pt,'&count=datintreceived:month')%>%add_api_key()
   url_dnprr_total<-paste0(base_url,'?count=datintreceived:month')%>%add_api_key()
   url_llr=paste0(base_url,'?search=report_ingredient_suspect:',ing_choices[1],'&count=reaction_pt.keyword')%>%add_api_key()
   
   search_url$d_count<-url_d
   search_url$ae_count<-url_ae
   search_url$de_count<-url_de
   search_url$time<-url_time
   search_url$drug<-input$search_drug
   search_url$pt<-input$search_pt
   search_url$cpa<-url_cpa_count
   search_url$dnprr_d<-url_dnprr_d
   search_url$dnprr_ae<-url_dnprr_ae
   search_url$dnprr_total<-url_dnprr_total
   search_url$llr_d<-url_llr
})

  
phvid_df<-reactive({
  
  L<-data.frame(ing=search_url$drug,
                         pt_name=search_url$pt)
  
  data<-data.frame(n11=hc_result(search_url$de_count),
                            n1.=hc_result(search_url$d_count),
                            n.1=hc_result(search_url$ae_count))
  
  N<-hc_result(search_url$total_count)
  
  return(list(L=L,data=data,N=N))
})  
    
  #asesemble into PhViD data type
disp_result<-reactive({
  
  withProgress(message='Performing Disproportionality analysis...',value=0,{
    
    
    DATA <- phvid_df()$data
    N <- phvid_df()$N
    
    L <- phvid_df()$L
    n11 <- DATA[,1]
    n1. <- DATA[,2] # marginal drug counts
    n.1 <- DATA[,3] # marginal AE counts
    n10 <- n1. - n11
    n01 <- n.1 - n11
    n00 <- N - (n11+n10+n01)
    #expected_count <- n1. * n.1 / N
    
    prr<-PRR(n11,n10,n01,n00,L)
    
    incProgress(1/5)
    
    ror<-ROR(n11,n10,n01,n00,L)
    rrr<-RRR(n11,n1.,n.1,N,L)
    
    incProgress(1/3)
    
    bcpnn<-BCPNN_HCSC(phvid_df())
    
    incProgress(3/4)
    rfet<-RFET_HCSC(phvid_df())
    
   prr_bind<-left_join(prr,ror)
            
   prr_final<-left_join(prr_bind,rrr)
    
   all_data<-left_join(bcpnn,prr_final)%>%left_join(rfet)
   
   return(list(prr_sum=prr_final,
               bcpnn=bcpnn,
               rfet=rfet,
               all=all_data,
               data=DATA
              ))
   
   
  })
  
})
    

#change of point input:
cpa_data<-reactive({
  
  datax<-hc_result(search_url$cpa,F)%>%
          dplyr::select(c(1,3))%>%
          rename(time=key_as_string,
                 n=doc_count)%>%
          mutate(time=substr(time,1,10))
  
  start<-c(as.numeric(substr(datax$time[1],1,4)),as.numeric(substr(datax$time[1],6,7)))
  datax2<-ts(datax$n,start=start,frequency = 4)
  
 
  return(list(datax=datax,
              datax2=datax2))
  
})


# cpa_mean,var and bcp calculation
cpa_cal<-reactive({
  
  withProgress(message='Performing ChangePoint analysis...',value=0,{
  datax2<-cpa_data()$datax2
  count_table<-cpa_data()$datax
  
  # validate(
  #   need(nrow(count_table)>=4,"Insufficient Data to fit a changepoint model,at least 4 observations are needed,
  #      please make a new selection")
  # )
  # 
  incProgress(1/3)
  cpt_mean<-cpt.mean(datax2, Q=5, method='BinSeg')
  
  cpt_var<-cpt.var(datax2, Q=5, method='BinSeg')
  incProgress(2/3)
  
  #add if statement to prevent crash:
  if(nrow(count_table)>=4){
    bcp.flu<-bcp(as.double(datax2),p0=0.3)
  }else{
    bcp.flu<-NULL
  }
 
  
  return(list(cpt_m=cpt_mean,
              cpt_v=cpt_var,
              bcp=bcp.flu))
  })
  
})


#build dynamic prr reactive data:

dnprr_cal<-reactive({
  
  withProgress(message='Calculating PRR over time...',value=0,{
  
  dnprr_d_ae<-hc_result(search_url$cpa,F)%>%
                 mutate(n11=cumsum(doc_count))%>%
                 dplyr::select(key_as_string,n11)
                 
  dnprr_d<-hc_result(search_url$dnprr_d,F)%>%
            mutate(n1.=cumsum(doc_count))%>%
            dplyr::select(key_as_string,n1.)
  
  dnprr_ae<-hc_result(search_url$dnprr_ae,F)%>%
            mutate(n.1=cumsum(doc_count))%>%
            dplyr::select(key_as_string,n.1)
  
  dnprr_total<-hc_result(search_url$dnprr_total,F)%>%
               mutate(N=cumsum(doc_count))%>%
               dplyr::select(key_as_string,N)
  incProgress(1/3)
  
  merged_tb<-left_join(dnprr_d_ae,dnprr_d)%>%
             left_join(dnprr_ae)%>%
             left_join(dnprr_total)
  
  
  PRR<-(merged_tb$n11 /merged_tb$n1.)/((merged_tb$n.1 -merged_tb$n11) / (merged_tb$N-merged_tb$n1.))
  logPRR <- log(PRR)
  
  var_logPRR <- 1/merged_tb$n11 - 1/merged_tb$n1. + 1/(merged_tb$n.1-merged_tb$n11) - 1/(merged_tb$N-merged_tb$n1.)
  
  incProgress(2/3)
  
  LB95_logPRR <- qnorm(0.025,logPRR,sqrt(var_logPRR))
  UB95_logPRR <- qnorm(0.975,logPRR,sqrt(var_logPRR))
  LB95_PRR <- exp(LB95_logPRR)
  UB95_PRR <- exp(UB95_logPRR)
  
  results<-data.frame(time=substr(merged_tb$key_as_string,1,7),prr=PRR,lb=LB95_PRR,ub=UB95_PRR,sd=sqrt(var_logPRR))
  
  return(results)
  
  })
})





########## Output
  # Display what query was searched

  output$current_search <- renderTable({
    result <- data.frame(names = c("Ingredient Name:",
                                   "Preferred Term:"),
                         terms = c(search_url$drug,
                                   search_url$pt),
                         stringsAsFactors=FALSE)
    result["" == result] <- "Not Specified"
    result
  }, include.colnames = FALSE)
  
  output$pt_data_dl <- downloadHandler(
    filename = function() {
      current_drug <- search_url$drug
      current_drug <- gsub(" ", "_", current_drug)
      paste0('pt_data_', current_drug, '.csv')
    },
    content = function(file) {
      write.csv(disp_result()$all, file, row.names=FALSE)
    }
  )
  
  

  # pass either datatable object or data to be turned into one to renderDataTable
  output$table_pt_prr <- DT::renderDataTable(
    DT::datatable(
    disp_result()$prr_sum,
    extensions = 'Buttons',
    options = list(
      scrollX = TRUE,
      dom = 'Bfrtip', buttons=I('colvis')))
      
      # buttons = list(list(extend = 'colvis',
      #                     text = 'Columns to display',
      #                     columns = 5:25)),
      # columnDefs = list(list(visible = FALSE,
      #                        targets = c(6:11,13:18,20:25)))
    
    )

  output$table_pt_bcpnn <- DT::renderDataTable(
    DT::datatable(
    disp_result()$bcpnn,
    options = list(
      scrollX = TRUE
      ))
    )
  
  
  output$table_pt_rfet <- DT::renderDataTable(
    DT::datatable(
    disp_result()$rfet)
    )
  
 
  # time-series data
  time_data_pt <- reactive({

    time_series<-hc_result(search_url$time,F)

    if (nrow(time_series) > 0) {
      time_count <-time_series%>%
                   rename(quarter=key_as_string,
                          n=doc_count)%>%
                   mutate(quarter=gsub('T00.*','',quarter))
    } else {
      showModal(modalDialog(
        title = list(icon("exclamation-triangle"), "No results found!"),
        "There were no reports matching your query.",
        size = "s",
        easyClose = TRUE))
      time_count <- data.frame(ing = NA, PT_NAME_ENG = NA, n = NA)# top_pairs %>% rename(ing = drug_code, PT_NAME_ENG = event_effect) %>%
    }

    return(time_count)
  })

  
  output$current_pt_title<- renderText({
    paste("Non-Cumulative Report Count Time Plot for:",search_url$drug,'&',search_url$pt)
  })
  
  
  output$timeplot_pt<-renderPlotly({
    data<-time_data_pt()%>%mutate(qtr = as.yearqtr(quarter,format='%Y-%m-%d'))
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 16
    )
    
    plot_ly(data,x=~qtr,y=~n,mode='lines+markers',name='time-series')%>%
            # marker = list(size = 10,
            #           color = 'rgba(255, 182, 193, .9)',
            #           line = list(color = 'rgba(152, 0, 0, .8)',
            #           width = 2)))%>%
      layout(xaxis=list(title='Quarter',
                        titlefont=f1,
                        tickangle=330),
             yaxis=list(title='Report Count'))
    
  })
 
 ###Change of Point analysis output:
  output$cpa_mean_title<- renderText({
    paste("Change of Point Mean Analysis for:",search_url$drug,'&',search_url$pt)
  })

  output$cpa_mean<-renderPlotly({
    
    p<-autoplot(cpa_cal()$cpt_m,ts.colour='dodgerblue4')+
                             labs(x='Quarter',y='Count')+
            theme(axis.text.x  = element_text(angle=20, vjust=0.5, size=10),
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                            colour = "gray96"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "gray96"))
                            
    ggplotly(p)

  })
  
  
  output$cpa_mean_txt<-renderTable({
    
    data<-cpa_cal()$cpt_m
    ts<-cpa_data()$datax
    cpa_loc<-paste(ts[data@cpts[1:length(data@cpts)-1],'time'],collapse=',')
    
    summary_table<-data.frame(names=c('Changepoint type','Method of analysis','Test Statistic','Type of penalty',
                                      'Maxium no. of cpts','ChangePoint locations'),
                              terms=c(data@cpttype,data@method,data@test.stat,data@pen.type,data@ncpts.max,cpa_loc))
    
   summary_table
  },colnames=F)
  
  
  output$cpa_var_title<- renderText({
    paste("Change of Point Variance Analysis for:",search_url$drug,'&',search_url$pt)
  })
  
  output$cpa_variance<-renderPlotly({
    p<-autoplot(cpa_cal()$cpt_v,ts.colour='dodgerblue4')+labs(x='Quarter',y='Count')+
       theme(axis.text.x  = element_text(angle=20, vjust=0.5, size=10),
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                            colour = "gray96"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "gray96"))
    ggplotly(p)
    
  })
  
  
  output$cpa_variance_txt<-renderTable({
    
    data<-cpa_cal()$cpt_v
    ts<-cpa_data()$datax
    cpa_loc<-paste(ts[data@cpts[1:length(data@cpts)-1],'time'],collapse=',')
    
    summary_table<-data.frame(names=c('Changepoint type','Method of analysis','Test Statistic','Type of penalty',
                                      'Maxium no. of cpts','Changepoint locations'),
                              terms=c(data@cpttype,data@method,data@test.stat,data@pen.type,data@ncpts.max,cpa_loc))
    
    summary_table
 },colnames=F)
  
 
  output$bcp_title<- renderText({
    paste("Bayesian Changepoint Analysis for:",search_url$drug,'&',search_url$pt)
  })
  
  output$bcpa<-renderPlot({
    plot(cpa_cal()$bcp)
  })
  
  output$bcpa_txt<-renderTable({
    data<-cpa_cal()$bcp
    data2<-cpa_data()$datax
    
    data2$postprob<-data$posterior.prob
    data2<-data2%>%arrange(desc(postprob))%>%slice(1:5)%>%
                   rename(Quarter=time,Count=n)
    
    data2
    
  })
  
  
  output$current_dpnrr_title<-renderText({
    paste("Dynamic PRR for:",search_url$drug,'&',search_url$pt)
  })
  
  output$dpnrr_plot<-renderPlotly({
    
    mydf<-dnprr_cal()
    
    mydf$quarter<-parse_date_time(mydf$time,"ym")
   
    #xloc <-parse_date_time(mydf$time,"ym")
    #labs <- mydf$time
    
    lbgap <-   exp(log(mydf$lb) + .96*mydf$sd) #exp ( log( prr ) - 1.96*sd )
    ubgap <-   exp(log(mydf$ub) - .96*mydf$sd)
    
    p<-ggplot(mydf,aes(x=quarter,y=prr))+geom_point(colour='dodgerblue4',size=1.5)+
      ylim( min(.5, min(mydf$lb)), max(2, max(mydf$ub) ) )+
      labs(x="Quarter",y="95% Confidence Interval for PRR")+
      theme(axis.text.x  = element_text(angle=20,size=10),
            panel.background = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                            colour = "gray96"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "gray96"))+
      scale_x_datetime('',date_labels="%Y-%m")+
      geom_errorbar(aes(ymin=lb, ymax=ub), colour="gray24", width=0.25)+
      # geom_segment(aes(x=xloc, y=ubgap, xend =xloc, yend =ub),arrow = arrow(angle=90,length = unit(0.05, "cm")))+
      # geom_segment(aes(x=xloc, y=lbgap, xend =xloc, yend =lb),arrow = arrow(angle=90,length = unit(0.05, "cm")))+
      geom_hline(yintercept=1,linetype='dotted')
      
    
    ggplotly(p, width = 1250, height = 550)
  })
  
  
}