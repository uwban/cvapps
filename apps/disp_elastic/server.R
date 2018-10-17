server <- function(input, output, session) {
  
search_input<-reactiveValues(
  drug=ing_choices[1],
  pt='Off label use'
)
  
  observeEvent(input$search_drug,{
    pt_choices<-create_uri(startDate,endDate,gender='All',rxn=NULL,drug_ing=input$search_drug,'reaction_pt.keyword')%>%
                add_api_key()%>%
                hc_result(F)%>%
                rename(`PT ordered by report number`=key)
                #mutate(key=paste0(key,' (',doc_count,')'))
    
    updateSelectInput(session,'search_pt',choices=pt_choices)
  })
  
  
  observeEvent(input$search_button,{
    
    search_input$drug<-input$search_drug
    search_input$pt<-input$search_pt
    
    url_de<-create_uri(startDate,endDate,rxn=input$search_pt,drug=input$search_drug,count_term ='id.keyword')%>%add_api_key()
      # paste0(base_url,'?search=report_ingredient_suspect:',input$search_drug,
      #              '+AND+','reaction_pt:',input$search_pt,'&count=_id.keyword')%>%add_api_key()

    
    n<-hc_result(url_de)
    
    if (n == 0) {
      setProgress(1)
      showModal(modalDialog(
        title = list(icon("exclamation-triangle"), "No results found!"),
        "There were no reports matching your query. Please make another selection",
        size = "s",
        easyClose = TRUE))
      
      
      search_input$drug<-ing_choices[1]
      search_input$pt<-pt_choices[1]
      
    }
    
    
})
  
  
phvid_df<-reactive({
  
  de_count<-create_uri(startDate,endDate,rxn=search_input$pt,drug=search_input$drug,count_term = 'id.keyword')%>%add_api_key()
  d_count<-create_uri(startDate,endDate,drug=search_input$drug,count_term = 'id.keyword')%>%add_api_key()
  ae_count<-create_uri(startDate,endDate,rxn=search_input$pt,count_term = 'id.keyword')%>%add_api_key()
  total_count<-create_uri(startDate,endDate,count_term = 'id.keyword')%>%add_api_key()
  
  L<-data.frame(ing=search_input$drug,
                pt_name=search_input$pt)
  
  data<-data.frame(n11=hc_result(de_count),
                   n1.=hc_result(d_count),
                   n.1=hc_result(ae_count))
  
  N<-hc_result(total_count)
  
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
    
    bcpnn<-BCPNN_HCSC(phvid_df(),MC=TRUE)
    
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
  
  cpa<-create_uri(startDate,endDate,rxn=search_input$pt,drug=search_input$drug,count_term='datintreceived:quarter')%>%add_api_key()
  
  datax<-hc_result(cpa,F)%>%
          dplyr::select(c(1,3))%>%
          rename(time=key_as_string,
                 n=doc_count)%>%
          mutate(time=substr(time,1,10))
  
  datax$qrt<-as.yearqtr(datax$time,format='%Y-%m-%d')
  start<-min(datax$qrt)
  end<-max(datax$qrt)
  
  start<-c(as.numeric(substr(start,1,4)),as.numeric(substr(start,7,8)))
  end<-c(as.numeric(substr(end,1,4)),as.numeric(substr(end,7,8)))
  datax2<-ts(datax$n,start=start,end=end,frequency=4)
  
 
  return(list(datax=datax,
              datax2=datax2,
              url=cpa))
  
})


# cpa_mean,var and bcp calculation
cpa_cal<-reactive({
  
  withProgress(message='Performing ChangePoint analysis...',value=0,{
  datax2<-cpa_data()$datax2
  count_table<-cpa_data()$datax
  check<-nrow(count_table)
  
  if (check <2) {
    cpt_mean<-data.frame(message='There is not enough data points for changepoint analysis')
    cpt_var<-data.frame(message='There is not enough data points for changepoint analysis')
    bcp.flu<-data.frame(message='There is not enough data points for changepoint analysis')
  }else{
  
  incProgress(1/3)
  cpt_mean<-cpt.mean(datax2, Q=5, method='BinSeg')
  
  cpt_var<-cpt.var(datax2, Q=5, method='BinSeg')
  incProgress(2/3)
  
  bcp.flu<-bcp(as.double(datax2),p0=0.3)
  
  }
  return(list(cpt_m=cpt_mean,
              cpt_v=cpt_var,
              bcp=bcp.flu))
  })
  
})


#build dynamic prr reactive data:

dnprr_cal<-reactive({
  
  withProgress(message='Calculating PRR over time...',value=0,{
    
 cpa<-create_uri(startDate,endDate,rxn=search_input$pt,drug=search_input$drug,count_term='datintreceived:quarter')%>%
      add_api_key()
    
 dnprr_d<-create_uri(startDate,endDate,drug=search_input$drug,count_term='datintreceived:quarter')%>%add_api_key()
 dnprr_ae<-create_uri(startDate,endDate,rxn=search_input$pt,count_term='datintreceived:quarter')%>%add_api_key()
 dnprr_total<-create_uri(startDate,endDate,count_term='datintreceived:quarter')%>%add_api_key()
 
  
  dnprr_d_ae<-hc_result(cpa,F)%>%
                 mutate(n11=cumsum(doc_count))%>%
                 dplyr::select(key_as_string,n11)
                 
  dnprr_d<-hc_result(dnprr_d,F)%>%
            mutate(n1.=cumsum(doc_count))%>%
            dplyr::select(key_as_string,n1.)
  
  dnprr_ae<-hc_result(dnprr_ae,F)%>%
            mutate(n.1=cumsum(doc_count))%>%
            dplyr::select(key_as_string,n.1)
  
  dnprr_total<-hc_result(dnprr_total,F)%>%
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

 output$search_url <- renderUI({
  url <- cpa_data()$url
  HTML(paste0('Search URL: <a href = ', url,' target="_blank">', url, '</a>'))
})


  output$current_search <- renderTable({
    result <- data.frame(names = c("Ingredient Name:",
                                   "Preferred Term:"),
                         terms = c(search_input$drug,
                                   search_input$pt),
                         stringsAsFactors=FALSE)
    result["" == result] <- "Not Specified"
    result
  }, include.colnames = FALSE)
  
  
  disp_download<-reactive({
    
    withProgress(message='Reformating JSON files to CSV',value=0,{
      
    report_data<-parse_all(startDate,endDate,'All',search_input$pt,search_input$drug)%>%
                 flatten()
    
    colnames(report_data)<-gsub('_source.','',colnames(report_data))
     report_data<-report_data%>%dplyr::select(-reactions,-reaction_pt,-report_drug_detail,-report_links,-reaction_soc,
          -report_ingredient_concomitant,-report_drugname_concomitant,-report_indication_eng)
          
      
    report_data$report_drugname_suspect<-sapply(report_data$report_drugname_suspect,paste,collapse=',',USE.NAMES = F)
    report_data$report_ingredient_suspect<-sapply(report_data$report_ingredient_suspect,paste,collapse=',',USE.NAMES = F)
      
    if(!is.null(input$select_column)){
      report_data<-report_data%>%dplyr::select(input$select_column)
    }else{
      report_data<-report_data
    }
    
    })
  })
  
  
  output$pt_data_dl <- downloadHandler(
    filename = function() {
      current_drug <- search_input$drug
      current_drug <- gsub(" ", "_", current_drug)
      current_pt<-gsub(" ","_",search_input$pt)
      paste0('DISP_data_', current_drug,'&',current_pt,'.csv')
    },
    content = function(file) {
      write.csv(disp_download(), file, row.names=FALSE)
    }
  )
  
  

  # pass either datatable object or data to be turned into one to renderDataTable
  output$table_pt_prr <- DT::renderDataTable(
    DT::datatable(
    disp_result()$prr_sum,
    extensions = 'Buttons',
    options = list(
      scrollX = TRUE,
      dom = 'Brtip', buttons=I('colvis')))
      
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
      scrollX = TRUE,
      dom='t'
      ))
    )
  
  
  output$table_pt_rfet <- DT::renderDataTable(
    DT::datatable(
    disp_result()$rfet,
    options=list(dom='t'))
    )
  
 
  # time-series data
  time_data_pt <- reactive({
    
    cpa<-create_uri(startDate,endDate,rxn=search_input$pt,drug=search_input$drug,count_term='datintreceived:quarter')%>%
      add_api_key()

    time_series<-hc_result(cpa,F)

      time_count <-time_series%>%
                   rename(quarter=key_as_string,
                          n=doc_count)%>%
                   mutate(quarter=gsub('T00.*','',quarter))

    return(time_count)
  })

  
  output$current_pt_title<- renderUI({
    title<-paste("Non-Cumulative Report Count Time Plot for:",search_input$drug,'&',search_input$pt)
    h3(strong(title))
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
  output$cpa_mean_title<- renderUI({
    h3(strong(paste("Change of Point Mean Analysis for:",search_input$drug,'&',search_input$pt)))
  })

  output$cpa_mean<-renderPlotly({
    
    count_table<-cpa_data()$datax
    check<-nrow(count_table)
    cpt_m<-cpa_cal()$cpt_m
    cpa_mean_fortify<-fortify(cpt_m)
    
    if(check <2){
      p <- plot_ly(data.frame(x=1,y=1), x = ~x, y = ~y, text ='There is not enough data points for changepoint analysis') %>%
        add_markers() %>%
        add_text(textfont = t, textposition = "top right") %>%
        layout(showlegend = FALSE)
      
    }else{
    
    
    trace<-cpa_mean_fortify$Index[!is.na(cpa_mean_fortify$mean)]
    max<-max(cpa_mean_fortify$Data)
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 16
    )
    
    p<-plot_ly(cpa_mean_fortify,x=~Index)%>%
      add_trace(y=~Data,mode='lines+markers',name='time-series')%>%
      layout(xaxis=list(title='Quarter',
                        titlefont=f1,
                        tickangle=330),
             yaxis=list(title='Report Count'))
    
    
    for (i in seq_along(trace)){
      p<-p%>%add_trace(x=trace[i],y=0:max,mode='lines',line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dash'),name='changepoint')
    }
    
    }
    p
    
    # p<-autoplot(cpa_cal()$cpt_m,ts.colour='#1f77b4',ts.geom='line')+
    #                          labs(x='Quarter',y='Count')+
    #         theme(axis.text.x  = element_text(angle=20, vjust=0.5, size=10),
    #         panel.background = element_rect(fill = "white",
    #                                         colour = "black",
    #                                         size = 0.5, linetype = "solid"),
    #         panel.grid.major = element_line(size = 0.25, linetype = 'solid',
    #                                         colour = "gray96"), 
    #         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
    #                                         colour = "gray96"))
    #                         
    # ggplotly(p)

  })
  
  
  output$cpa_mean_txt<-renderTable({
    
    count_table<-cpa_data()$datax
    check<-nrow(count_table)
    
    if (check <2){
      summary_table<-data.frame(Error='there is not enough data points for changepoint analysis')
    }else{
    
    data<-cpa_cal()$cpt_m
    ts<-cpa_data()$datax
    cpa_loc<-paste(ts[data@cpts[1:length(data@cpts)-1],'time'],collapse=',')
    
    summary_table<-data.frame(names=c('Changepoint type','Method of analysis','Test Statistic','Type of penalty',
                                      'Maxium no. of cpts','ChangePoint locations'),
                              terms=c(data@cpttype,data@method,data@test.stat,data@pen.type,data@ncpts.max,cpa_loc))
    }
   summary_table
  },colnames=F)
  
  
  output$cpa_var_title<- renderUI({
    h3(strong(paste("Change of Point Variance Analysis for:",search_input$drug,'&',search_input$pt)))
  })
  
  output$cpa_variance<-renderPlotly({
    
    count_table<-cpa_data()$datax
    check<-nrow(count_table)
    
    cpt_v<-cpa_cal()$cpt_v
    cpa_variance_fortify<-fortify(cpt_v)
    
    
    if(check <2){
      p <- plot_ly(data.frame(x=1,y=1), x = ~x, y = ~y, text ='There is not enough data points for changepoint analysis') %>%
        add_markers() %>%
        add_text(textfont = t, textposition = "top right") %>%
        layout(showlegend = FALSE)
    }else{
    
    trace<-cpa_variance_fortify$Index[!is.na(cpa_variance_fortify$variance)]
    max<-max(cpa_variance_fortify$Data)
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 16
    )
    
    p<-plot_ly(cpa_variance_fortify,x=~Index)%>%
      add_trace(y=~Data,mode='lines+markers',name='time-series')%>%
      layout(xaxis=list(title='Quarter',
                        titlefont=f1,
                        tickangle=330),
             yaxis=list(title='Report Count'))
    
    
    for (i in seq_along(trace)){
      p<-p%>%add_trace(x=trace[i],y=0:max,mode='lines',line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dash'),name='changepoint')
     }
    }
    p
    # p<-autoplot(cpa_cal()$cpt_v,ts.colour='#1f77b4')+labs(x='Quarter',y='Count')+
    #    theme(axis.text.x  = element_text(angle=20, vjust=0.5, size=10),
    #         panel.background = element_rect(fill = "white",
    #                                         colour = "black",
    #                                         size = 0.5, linetype = "solid"),
    #         panel.grid.major = element_line(size = 0.25, linetype = 'solid',
    #                                         colour = "gray96"), 
    #         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
    #                                         colour = "gray96"))
    # ggplotly(p)
    
  })
  
  
  output$cpa_variance_txt<-renderTable({
    
    count_table<-cpa_data()$datax
    check<-nrow(count_table)
    
    if (check <2){
      summary_table<-data.frame(Error='there is not enough data points for changepoint analysis')
    }else{
    
    data<-cpa_cal()$cpt_v
    ts<-cpa_data()$datax
    cpa_loc<-paste(ts[data@cpts[1:length(data@cpts)-1],'time'],collapse=',')
    
    
    summary_table<-data.frame(names=c('Changepoint type','Method of analysis','Test Statistic','Type of penalty',
                                      'Maxium no. of cpts','Changepoint locations'),
                              terms=c(data@cpttype,data@method,data@test.stat,data@pen.type,data@ncpts.max,cpa_loc))
    }
    summary_table
 },colnames=F)
  
 
  output$bcp_title<- renderUI({
    h3(strong(paste("Bayesian Changepoint Analysis for:",search_input$drug,'&',search_input$pt)))
  })
  
  output$bcpa<-renderPlot({
    
    data<-cpa_data()$datax
    
    if(nrow(data)<2){
      plot(x=1,y=1,type='n')
      text(1,1,'there is not enough data for changepoint analysis')
        
    }
    plot(cpa_cal()$bcp)
  })
  
  output$bcpa_txt<-renderTable({
    data2<-cpa_data()$datax
    check<-nrow(data2)
    
    if (check <2){
      data2<-data.frame(Error='there is not enough data points for changepoint analysis')
    }else{
    
    data<-cpa_cal()$bcp
    
    data2$postprob<-data$posterior.prob
    data2<-data2%>%
      dplyr::select(-qrt)%>%
      arrange(desc(postprob))%>%
      slice(1:5)%>%
      rename(Quarter=time,Count=n)
    }
    data2
    
  })
  
  
  output$current_dpnrr_title<-renderUI({
    h3(strong(paste("Cumulative PRR over time for:",search_input$drug,'&',search_input$pt)))
  })
  
  output$dpnrr_plot<-renderPlotly({
    
    mydf<-dnprr_cal()
    
    mydf$quarter<-parse_date_time(mydf$time,"ym")
   
    #xloc <-parse_date_time(mydf$time,"ym")
    #labs <- mydf$time
    
    lbgap <-   exp(log(mydf$lb) + .96*mydf$sd) #exp ( log( prr ) - 1.96*sd )
    ubgap <-   exp(log(mydf$ub) - .96*mydf$sd)
    
    p<-ggplot(mydf,aes(x=quarter,y=prr))+geom_point(colour='#1f77b4',size=1.5)+
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
      geom_errorbar(aes(ymin=lb, ymax=ub), colour="gray24",size=0.1,width=0.25)+
      # geom_segment(aes(x=xloc, y=ubgap, xend =xloc, yend =ub),arrow = arrow(angle=90,length = unit(0.05, "cm")))+
      # geom_segment(aes(x=xloc, y=lbgap, xend =xloc, yend =lb),arrow = arrow(angle=90,length = unit(0.05, "cm")))+
      geom_hline(yintercept=1,linetype='dotted')
      
    
    ggplotly(p)
  })
  
  
  output$dpnrr_table<-DT::renderDataTable(
    DT::datatable(
      dnprr_cal(),
      options = list(
        scrollX = TRUE
      ))
  )
  
  
}