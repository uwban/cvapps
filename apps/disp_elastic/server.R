server <- function(input, output, session) {
  
search_url<-reactiveValues( 

  d_count=paste0(base_url,'?search=report_ingredient_suspect:',ing_choices[1],'&count=_id.keyword'),
  ae_count=gsub(' ','%20',paste0(base_url,'?search=reaction_pt:',pt_choices[1],'&count=_id.keyword')),
  de_count=gsub(' ','%20',paste0(base_url,'?search=report_ingredient_suspect:',ing_choices[1],'+AND+','reaction_pt:',pt_choices[1],'&count=_id.keyword')),
  total_count=paste0(base_url,'?count=_id.keyword'),
  time=gsub(' ','%20',paste0(base_url,'?search=report_ingredient_suspect:',ing_choices[1],'+AND+','reaction_pt:',
                    pt_choices[1],'&count=datintreceived:quarter')),
  drug=ing_choices[1],
  pt=pt_choices[1]
)
  
  observeEvent(input$search_button,{
    
   url_d<-paste0(base_url,'?search=report_ingredient_suspect:',input$search_drug,'&count=_id.keyword')
   url_d<-gsub(' ','%20',url_d)
   url_ae<-paste0(base_url,'?search=reaction_pt:',input$search_pt,'&count=_id.keyword')
   url_ae<-gsub(' ','%20',url_ae)
   url_de<-paste0(base_url,'?search=report_ingredient_suspect:',input$search_drug,'+AND+','reaction_pt:',input$search_pt,'&count=_id.keyword')
   url_de<-gsub(' ','%20',url_de)
   
   url_time_count<-paste0(base_url,'?search=report_ingredient_suspect:',input$search_drug,'+AND+','reaction_pt:',
                          input$search_pt,'&count=datintreceived:quarter')
   url_time<-gsub(' ','%20',url_time_count)
   
   search_url$d_count<-url_d
   search_url$ae_count<-url_ae
   search_url$de_count<-url_de
   search_url$time<-url_time
   search_url$drug<-input$search_drug
   search_url$pt<-input$search_pt
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
  
  withProgress(message='Calculating...',value=0,{
    
    
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
  
  
  output$timeplot_pt<-renderPlotly(({
    data<-time_data_pt()%>%mutate(qtr = as.yearqtr(quarter %>% as.character(), '%Y-%q'))
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 18,
      color = "lightgrey"
    )
    
    plot_ly(data,x=~qtr,y=~n,name='time-series')%>%
      layout(xaxis=list(title='Quarter',
                        titlefont=f1,
                        tickangle=330),
             yaxis=list(title='Report Count'))
    
  }))
 
  # time_data_pt %>%  %>%
  #   ggvis(~qtr, ~n) %>%
  #   add_axis("x", title = "Quarter", properties = axis_props(
  #     label = list(angle = 330))) %>%
  #   add_axis("y", title = "Report Count") %>%
  #   add_tooltip(function(data){paste0("Count: ", as.character(data$n))},
  #               "hover") %>%
  #   layer_points(fill = ~label, stroke := "black") %>%
  #   group_by(label) %>%
  #   layer_paths(stroke = ~label) %>%
  #   set_options(width = 'auto') %>%  bind_shiny("timeplot_pt", "data")


  
}